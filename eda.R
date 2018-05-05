library("lattice")
library("stringr")
library("rstudioapi")

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# predictors and response in separate files. merge into one df
training.values <- read.csv("data/training_set_values.csv", header = TRUE)
training.labels <- read.csv("data/training_set_labels.csv", header = TRUE)
training.set <- merge(training.values, training.labels, by = "id")

# cast columns appropriately
training.set[,'construction_year'] <- as.factor(training.set[, 'construction_year'])
training.set[,'region_code'] <- as.factor(training.set[, 'region_code'])
training.set[,'date_recorded'] <- as.character(training.set[,'date_recorded'])
training.set[,'district_code'] <- as.factor(training.set[, 'district_code'])

col.names <- colnames(training.set)
response.var <- 'status_group'
id.var <- 'id'
num.rows <- nrow(training.set)

# get freq and proportion tables of response for full training df
response.freqs <- table(training.set[,response.var])
response.props <- prop.table(margin.table(response.freqs, 1))

aggregate.vals <- function(pred.df, df, predictor.var, response.var){
  groups <- c("large", "medium", "small")
  perc.obs.cutoffs <- c(.05, .01, 0)
  
  cutoffs <- as.list(perc.obs.cutoffs)
  names(cutoffs) <- groups
  
  aggregated.var.name <- paste(predictor.var, "group", sep=".")
  pred.df[,aggregated.var.name] <- NULL
  
  pred.df[,aggregated.var.name] <- with(pred.df, 
                                     ifelse(perc.obs < cutoffs$medium, 'small', 
                                        ifelse(perc.obs < cutoffs$large & functional > response.props["functional"], 'medium above',
                                        ifelse(perc.obs < cutoffs$large & functional <= response.props["functional"], 'medium below',         
                                            ifelse(perc.obs >= cutoffs$large & functional > response.props["functional"], 'large above', 
                                                'large below')))))
  
  small.pred.vals <- row.names(pred.df)[apply(pred.df, 1, function(row) row[aggregated.var.name] =='small')]
  med.above.pred.vals <- row.names(pred.df)[apply(pred.df, 1, function(row) row[aggregated.var.name]=='medium above')]
  med.below.pred.vals <- row.names(pred.df)[apply(pred.df, 1, function(row) row[aggregated.var.name]=='medium below')]
  large.above.pred.vals <- row.names(pred.df)[apply(pred.df, 1, function(row) row[aggregated.var.name]=='large above')]
  large.below.pred.vals <- row.names(pred.df)[apply(pred.df, 1, function(row) row[aggregated.var.name]=='large below')]

#   if (all(length(small.pred.vals) > 0,
#           length(med.above.pred.vals) > 0,
#           length(med.below.pred.vals) > 0, 
#           length(large.above.pred.vals) > 0,
#           length(large.below.pred.vals) > 0
#           )
#   ) {
  df[,aggregated.var.name] <- NA
  df[,aggregated.var.name][df[,predictor.var] %in% small.pred.vals ] <- 'small'
  df[,aggregated.var.name][df[,predictor.var] %in% med.above.pred.vals ] <- 'medium above'
  df[,aggregated.var.name][df[,predictor.var] %in% med.below.pred.vals ] <- 'medium below'
  df[,aggregated.var.name][df[,predictor.var] %in% large.above.pred.vals ] <- 'large above'
  df[,aggregated.var.name][df[,predictor.var] %in% large.below.pred.vals ] <- 'large below'
    
  # pred.aggregated <- apply(df, 1, function(row) {
  #   if(length(small.pred.vals) > 0) {
  #     ifelse(row[predictor.var] %in% small.pred.vals, 'small', row[,aggregated.var.name])
  #   }  
  #   if( length(med.above.pred.vals) > 0 ) {
  #     ifelse(row[predictor.var] %in% med.above.pred.vals, 'medium above', row[,aggregated.var.name])
  #   }
  #             
  #                   # ifelse(row[predictor.var] %in% med.below.pred.vals, 'medium below', 
  #                   #        ifelse(row[predictor.var] %in% large.above.pred.vals, 'large above',
  #                   #               'large below'))))
  #   })
  #   
  # df[,aggregated.var.name] <- pred.aggregated
    table(df[,response.var], df[,aggregated.var.name])
    #print(prop.table(table(df[,response.var], df[,aggregated.var.name]), 2))
    jpeg(paste('output/images/factor_', aggregated.var.name, '.jpg', sep=""))
    print(histogram(~ df[,response.var] | df[,aggregated.var.name] , data = df,  xlab = aggregated.var.name))
    dev.off()
    #print(histogram(~ df[,response.var] | df[,aggregated.var.name], data = df, xlab = aggregated.var.name))
  write.csv(pred.df, paste("output/", predictor.var, "_detail.csv", sep=""))
  return(df)
}

factor.eda.fxn <- function(df, predictor.var, response.var, num.rows) {
  num.values.cutoff <- 10
  
  freq <- as.data.frame(table(df[,predictor.var]))
  colnames(freq) <- c(predictor.var, "count")
  rownames(freq) <- freq[,predictor.var]
  freq <- freq[,"count", drop=FALSE]
  
  response.freq <- table(df[,predictor.var], df[,response.var])
  response.prop <- as.data.frame.matrix(prop.table(response.freq, 1))
  pred.df <- merge(freq, response.prop, by=0)
  rownames(pred.df) <- pred.df$Row.names
  pred.df$perc.obs <- pred.df$count / num.rows
  pred.df <- pred.df[,c("count", "functional", "functional needs repair", "non functional", "perc.obs")]
  pred.df <- pred.df[order(-pred.df[,"count"]),]
  pred.df <- within(pred.df, cuml.perc.obs <- cumsum(perc.obs))
  
  if(nrow(freq) > num.values.cutoff) {
    #print(paste("aggregating vals"))
    df <- aggregate.vals(pred.df, df, predictor.var, response.var)
  } else{
    #print(prop.table(table(df[,response.var], df[,predictor.var]), 2))
    jpeg(paste('output/images/factor_', predictor.var, '.jpg', sep=""))
    print(histogram(~ df[,response.var] | df[,predictor.var] , data = df,  xlab = predictor.var))
    dev.off()
    #print(histogram(~ df[,response.var] | df[,predictor.var] , data = df,  xlab = predictor.var))
  }
  return(df)
}

numeric.eda.fxn <- function(df, predictor.var, response.var) {
  jpeg(paste('output/images/numeric_', predictor.var, '.jpg', sep=""))
  print(densityplot(~ df[,predictor.var], groups = df[,response.var], data = training.set, xlab=predictor.var,plot.points = FALSE, ref = TRUE, auto.key = list(columns = 1)))
  dev.off()
}

not.useful.cols <- c("id", "date_recorded", "num_private", response.var)
for(col in col.names) {
  if(col %in% not.useful.cols) {next}
  if(class(training.set[,col]) == 'factor') {
    #print(paste("*****running factor fxn on col", col))
    training.set <- factor.eda.fxn(training.set, col, response.var, num.rows)
  }
  else {
    numeric.eda.fxn(training.set, col, response.var)
  }
}