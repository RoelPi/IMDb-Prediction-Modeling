getPackages <- function(list.of.packages) {
    # Install necessary packages
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if (length(new.packages)) install.packages(new.packages)
    lapply(list.of.packages,library, character.only=TRUE)
}
loadRatings <- function(file = "ratings.csv") {
    read.csv("ratings.csv",header=T,sep=",")
}
loadOMDB <- function(df = loadRatings()) {
    dfOMDB <- data.frame()
    for (i in 1:nrow(df)) {
        id <- df[i,2]
        url <- gsub(" ","%20",paste("http://www.omdbapi.com/?i=",as.character(id), sep = ""))
        message(paste("Requesting data for",id <- df[i,6],"- Item ",i,"of",nrow(df),collapse=" "))
        omdbData <- fromJSON(url)
        if (!is.null(omdbData$totalSeasons)) { omdbData$totalSeasons <- NULL }
        if (i == 1) { 
            dfOMDB <- as.data.frame(omdbData,stringsAsFactors=F) 
        } else { 
            dfOMDB[i,] <- as.character(omdbData)
        }
    }
    dfOMDB
}
makeLong <- function(dataset = df,col="Genre") {
    for (i in 1:nrow(dataset)) {
        types <- as.character(strsplit(dataset[i,col],", ")[[1]])
        for (j in 1:length(types)) {
            dataset[i,paste(col,types[j],sep="_")] <- 1
        }
    }
    colnames(dataset) <- gsub("[^[:alnum:]///' ]", "", colnames(dataset))
    dataset[is.na(dataset)] <- 0
    dataset
}
removeNA <- function(df) {
    suffix <- c("^Genre","^Actors","^Director","^Country")
    for (j in 1:length(suffix)) {
        genreCols <- grepl(suffix[j],names(df))
        for (i in 1:sum(genreCols)) {
            column <- df[,genreCols][i]
            column[is.na(column)] <- 0
            df[,genreCols][i] <- column
        }
    }
    df
}
selectColumns <- function(df) {
    df <- subset(df,select = -c(const,created,Rated,Title,Released,Runtime,MultipleG,MultipleD,Writer,MultipleA,Plot,Language,MultipleC,Awards,Poster,Response,Type,Metascore))
}
moviesOnly <- function(df) {
    df <- df[df$Type=="movie",]
}
addTreshold <- function(df) {
    sl <- c()
    for (i in 1:ncol(df)) {
        if (grepl("^(Actors|Director|Genre|Country)",names(df[i]))) {
            if (sum(df[,i])<5) {
                sl[i] <- FALSE
            } else {
                sl[i] <- TRUE
            }
        } else {
            sl[i] <- TRUE
        }
    }
    df <- df[,sl]
}

# Do cleanings, robot
getData <- function(dataFromRoel = loadRatings(),dataFromOMDB = loadOMDB()) {
    getPackages(c("jsonlite", "magrittr","dplyr"))
    dataFromRoel <- dataFromRoel[c("const","created","You.rated")]
    dataset <- merge(dataFromRoel,dataFromOMDB,by.x="const",by.y="imdbID")
}
cleanData <- function(ds = getData()) {
    ds <- makeLong(ds)
    ds <- makeLong(ds,"Director")
    ds <- makeLong(ds,"Actors")
    ds <- makeLong(ds,"Country")
    
    # Remove fake NA's
    ds[ds == 'N/A'] <- NA
    
    # Make important covariates & outcomes numeric
    ds$imdbVotes <- gsub(",","",ds$imdbVotes)
    ds[,c("Yourated","Metascore","imdbVotes","imdbRating","Year")] <- ds[,c("Yourated","Metascore","imdbVotes","imdbRating","Year")] %>% mutate_each(funs(as.numeric))
    # Replace dashes & spaces in column names
    colnames(ds) <- gsub("-","",colnames(ds))
    colnames(ds) <- gsub(" ","",colnames(ds))
    
    # Rename columns where values became dummy variables
    names(ds)[names(ds) == 'Director'] <- 'MultipleD'
    names(ds)[names(ds) == 'Actors'] <- 'MultipleA'
    names(ds)[names(ds) == 'Genre'] <- 'MultipleG'
    names(ds)[names(ds) == 'Country'] <- 'MultipleC'
    print("Movies only.")
    ds <- moviesOnly(ds)
    print("Removing unnecessary columns.")
    ds <- selectColumns(ds)
    print("Removing columns with few data.")
    ds <- addTreshold(ds)
    print("Removing NA's")
    ds <- removeNA(ds)
    ds$sixHigh <- ifelse(ds$Yourated > 6,"HIGH","LOW")
    ds <- ds[complete.cases(ds),]
}
# Do learnings, robot
createSets <- function(df) {
    df <- df[,2:197]
    set.seed(55552)
    library(caret)
    inTrain <- createDataPartition(y=df$sixHigh,p=0.75,list=F)
    movieTraining <<- df[inTrain,]
    movieTest <<- df[-inTrain,]
}
doKNN <- function(df) {
    knnFit <<- train(sixHigh ~.,data=df,method="knn",preProcess = c("center","scale"),tuneLength=20)
    knnPredict <<- predict(knnFit,newdata=movieTest)
    confusionMatrix(knnPredict,movieTest$sixHigh)
}
doRC <- function(df) {
    rcFit <<- train(sixHigh ~.,data=df,method="rf",preProcess = c("center","scale"),tuneLength=20)
    rcPredict <<- predict(rcFit,newdata=movieTest)
    step <- table(rcPredict,newdata=movieTest$sixHigh)
    confusionMatrix(step)
}
doGLM <- function(df) {
    glmFit <<- train(sixHigh ~ imdbRating, data=df,method="glm")
    glmPredict <<- predict(glmFit,newdata=movieTest)
    step <- table(glmPredict,newdata=movieTest$sixHigh)
    confusionMatrix(step)
}
