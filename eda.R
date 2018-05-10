library("lattice")
library("stringr")
library("rstudioapi")

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
source("utils.R")

response.var <- 'status_group'
id.var <- 'id'
training.set <- get.data.set(df = "train", id.var)

num.rows <- nrow(training.set)
col.names <- colnames(training.set)
# get freq and proportion tables of response for full training df
response.freqs <- table(training.set[,response.var])
response.props <- prop.table(margin.table(response.freqs, 1))

not.useful.cols <- c("id", "date_recorded", "num_private", response.var)
for(col in col.names) {
  if(col %in% not.useful.cols) {next}
  if(class(training.set[,col]) == 'factor') {
    training.set <- factor.eda.fxn(training.set, col, response.var, num.rows)
  }
  else {
    numeric.eda.fxn(training.set, col, response.var)
  }
}