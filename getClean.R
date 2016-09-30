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
    if (!file.exists("omdbdata.csv")) {
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
        write.csv(dfOMDB,"omdbdata.csv",row.names=F)
    } 
    dfOMDB <- read.csv("omdbdata.csv",header=TRUE,stringsAsFactors = F)
}
makeLong <- function(dataset = ds,col="Genre") {
    for (i in 1:nrow(dataset)) {
        types <- strsplit(as.character(dataset[i,col]),", ")[[1]]
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
    df <- subset(df,select = -c(const,created,Rated,Released,Runtime,MultipleG,MultipleD,Writer,MultipleA,Plot,Language,MultipleC,Awards,Poster,Response,Type,Metascore))
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
    getPackages(c("jsonlite", "magrittr","dplyr","reshape","ggplot2","cowplot"))
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
    ds <- ds %>% mutate(diffRating = Yourated - imdbRating)
    ds
}

# Do summaries, robot
meltData <- function(ds = cleanData()) {
    ds <- melt(ds,id.vars=c("Title","Yourated","imdbRating","diffRating"))
    ds
}
makePlot <- function(ds = meltData(),feature="Genre",items=1:10,bottom=F) {
    # Select the appropriate data
    ds <- ds[grepl(feature,ds$variable),]
    ds <- ds[ds$value==1,]
    ds$variable <- gsub(feature,"",ds$variable)
    
    dsSummary <- ds %>% group_by(variable) %>% summarise(myratingmean=mean(Yourated),imdbratingmean=mean(imdbRating),diffratingmean=mean(diffRating),counts=n())
    dsSummaryByDiff <- dsSummary[order(dsSummary$diffratingmean,decreasing=TRUE),]
    dsSummaryByMy <- dsSummary[order(dsSummary$myratingmean,decreasing=TRUE),]
    dsSummaryByCount <- dsSummary[order(dsSummary$counts,decreasing=TRUE),]
    
    if (bottom == T) {
        print((nrow(dsSummaryByDiff)-length(items)))
        dsSummaryByDiff <- dsSummaryByDiff[(nrow(dsSummaryByDiff)-length(items)):nrow(dsSummaryByDiff),]
        dsSummaryByMy <- dsSummaryByMy[(nrow(dsSummaryByMy)-length(items)):nrow(dsSummaryByMy),]
    } else {
        dsSummaryByDiff <- dsSummaryByDiff[items,]
        dsSummaryByMy <- dsSummaryByMy[items,]
        dsSummaryByCount <- dsSummaryByCount[items,]
    }
    p <- ggplot(dsSummaryByMy,aes(x=reorder(variable,-myratingmean),y=myratingmean)) 
    p <- p + geom_bar(stat="identity",aes(fill=myratingmean))
    p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=8),legend.position="none") + xlab("") + ylab("My Rating")
    p <- p + scale_fill_gradient(low="#f3ce13", high="black")
    p
    q <- ggplot(dsSummaryByDiff,aes(x=reorder(variable,-diffratingmean),y=diffratingmean)) 
    q <- q + geom_bar(stat="identity",aes(fill=diffratingmean))
    q <- q + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=8),legend.position="none") + xlab("") + ylab("Difference in Rating")
    q <- q + scale_fill_gradient(low="#f3ce13", high="black")
    q
    r <- ggplot(dsSummaryByCount,aes(x=reorder(variable,-counts),y=counts)) 
    r <- r + geom_bar(stat="identity",aes(fill=counts))
    r <- r + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=8),legend.position="none") + xlab("") + ylab("# Watched")
    r <- r + scale_fill_gradient(low="#f3ce13", high="black")
    plot_grid(r,p,q,align="h",ncol=3)
}

stripData <- function(ds = cleanData()) {
    ds <- subset(ds,select=-c(Yourated,diffRating))
    ds
}
# Do learnings, robot
createSets <- function(ds) {
    set.seed(999)
    getPackages(c("caret","caretEnsemble"))
    inTrain <- createDataPartition(y=ds$sixHigh,p=0.75,list=F)
    movieTraining <<- ds[inTrain,]
    movieTest <<- ds[-inTrain,]
    trControl <<- trainControl(method="cv",number=3)
}
doRF <- function(df,testSet) {
    df <- subset(df,select=-c(Title))
    rfFit <<- train(sixHigh ~.,data=df,method="rf",trainControl=trControl)
    rfPredict <<- predict(rfFit,newdata=testSet)
}
doGLM <- function(df,testSet) {
    df <- subset(df,select=-c(Title))
    glmFit <<- train(sixHigh ~ imdbRating+imdbVotes+Year, data=df,method="glm")
    glmPredict <<- predict(glmFit,newdata=testSet)
}
doRPART <- function(df,testSet) {
    df <- subset(df,select=-c(Title))
    rpartFit <<- train(sixHigh ~., data=df,method="rpart")
    rpartPredict <<- predict(glmFit,newdata=testSet)
}
modelOverview <- function(models,predictions,testSet) {
    df <- data.frame(
            confusionMatrix(predictions[[1]],testSet$sixHigh)$overall,
            confusionMatrix(predictions[[2]],testSet$sixHigh)$overall,
            confusionMatrix(predictions[[3]],testSet$sixHigh)$overall
    )
    rownames(df) <- names(confusionMatrix(predictions[[1]],testSet$sixHigh)$overall)
    colnames(df) <- c(models[[1]]$method,models[[2]]$method,models[[3]]$method)
    round(df,2)
}
agree <- function(testSet,models,predictions) {
    df <- data.frame(testSet$Title,testSet$sixHigh,predictions[[1]],predictions[[2]],predictions[[3]])
    colnames(df) <- c("title","RealValues",models[[1]]$method,models[[2]]$method,models[[3]]$method)
    df <- mutate(df,agree=RealValues==glm&RealValues==rf&RealValues==rpart)
    sum(df$agree)/length(df$agree)
}
