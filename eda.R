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


col.names <- colnames(training.set)
response.var <- 'status_group'
id.var <- 'id'
num.rows <- nrow(training.set)

# get freq and proportion tables of response for full training df
response.freqs <- table(training.set[,response.var])
response.props <- prop.table(margin.table(response.freqs, 1))

aggregate.vals <- function(pred.df, subsetted.df, predictor.var, response.var){
  groups <- c("large", "medium", "small")
  perc.obs.cutoffs <- c(.05, .01, 0)
  
  cutoffs <- as.list(perc.obs.cutoffs)
  names(cutoffs) <- groups
}

factor.eda.fxn <- function(subsetted.df, predictor.var, response.var, num.rows) {
  num.values.cutoff <- 10
  
  freq <- as.data.frame(table(subsetted.df[,predictor.var]))
  colnames(freq) <- c(predictor.var, "count")
  rownames(freq) <- freq[,predictor.var]
  freq <- freq[,"count", drop=FALSE]
  
  response.freq <- table(subsetted.df[,predictor.var], subsetted.df[,response.var])
  response.prop <- as.data.frame.matrix(prop.table(response.freq, 1))
  pred.df <- merge(freq, response.prop, by=0)
  rownames(pred.df) <- pred.df$Row.names
  pred.df$perc.obs <- pred.df$count / num.rows
  pred.df <- pred.df[,c("count", "functional", "functional needs repair", "non functional", "perc.obs")]
  pred.df <- pred.df[order(-pred.df[,"count"]),]
  pred.df <- within(pred.df, cuml.perc.obs <- cumsum(perc.obs))
  
  if(nrow(freq) > num.values.cutoff) {
    var.to.be.named.later <- aggregate.vals(pred.df, subsetted.df, predictor.var, response.var)
  }
  return 
}

not.useful.cols <- c("id", "date_recorded", "num_private")
for(col in col.names) {
  print(col)
  subsetted.df <- training.set[,c(id.var, predictor.var, response.var)]
  if(class(subsetted.df[,predictor.var] == 'factor')) {
    
  }
}