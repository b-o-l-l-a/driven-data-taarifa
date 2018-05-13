library("lattice")
library("stringr")
library("rstudioapi")

get.data.set <- function(df.type, id.var) {
  
  if(df.type == "train") {
    csv.name.prefix <- "training_set"
  } else {
    csv.name.prefix <- "test_set"
  }
  
  # predictors and response in separate files. merge into one df
  df.values <- read.csv(paste("data/", csv.name.prefix ,"_values.csv", sep=""), header = TRUE)
  df.labels <- read.csv(paste("data/", csv.name.prefix ,"_labels.csv", sep=""), header = TRUE)
  df <- merge(df.values, df.labels, by = id.var)

  # cast columns appropriately
  df[,'construction_year'] <- as.factor(df[, 'construction_year'])
  df[,'region_code'] <- as.factor(df[, 'region_code'])
  df[,'date_recorded'] <- as.character(df[,'date_recorded'])
  df[,'district_code'] <- as.factor(df[, 'district_code'])
  
  return(df)
}

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

  df[,aggregated.var.name] <- NA
  df[,aggregated.var.name][df[,predictor.var] %in% small.pred.vals ] <- 'small'
  df[,aggregated.var.name][df[,predictor.var] %in% med.above.pred.vals ] <- 'medium above'
  df[,aggregated.var.name][df[,predictor.var] %in% med.below.pred.vals ] <- 'medium below'
  df[,aggregated.var.name][df[,predictor.var] %in% large.above.pred.vals ] <- 'large above'
  df[,aggregated.var.name][df[,predictor.var] %in% large.below.pred.vals ] <- 'large below'

  table(df[,response.var], df[,aggregated.var.name])
  jpeg(paste('output/images/factor_', aggregated.var.name, '.jpg', sep=""))
  print(histogram(~ df[,response.var] | df[,aggregated.var.name] , data = df,  xlab = aggregated.var.name))
  dev.off()
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
    df <- aggregate.vals(pred.df, df, predictor.var, response.var)
  } else{

    jpeg(paste('output/images/factor_', predictor.var, '.jpg', sep=""))
    print(histogram(~ df[,response.var] | df[,predictor.var] , data = df,  xlab = predictor.var))
    dev.off()
  }
  return(df)
}

numeric.eda.fxn <- function(df, predictor.var, response.var) {
  jpeg(paste('output/images/numeric_', predictor.var, '.jpg', sep=""))
  print(densityplot(~ df[,predictor.var], groups = df[,response.var], data = training.set, xlab=predictor.var,plot.points = FALSE, ref = TRUE, auto.key = list(columns = 1)))
  dev.off()
}

non.predictors.fxn <- function(response.var) {
  
  non.preds <- c("id", "date_recorded", "num_private", "recorded_by", "subvillage.group", "ward.group" ,response.var)
  return(non.preds)
  
}
