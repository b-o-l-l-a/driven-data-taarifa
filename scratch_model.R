library("nnet")
library("lattice")
library("stringr")
library("rstudioapi")

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
source("utils.R")

response.var <- 'status_group'
id.var <- 'id'
#training.set <- get.data.set(df = "train", id.var)
training.set <- read.csv("data/full_training_set.csv")

col.names <- colnames(training.set)

training.set$status_group_releveled <- relevel(training.set[,response.var], ref="non functional")
test <- multinom(status_group ~ waterpoint_type, data=training.set)
#test <- multinom(status_group_releveled ~ waterpoint_type, data=training.set)
summary(test)
exp(coef(test))
pp <- fitted(test)

a <- prop.table(table(training.set[,response.var]))
chisq.test(table(training.set$waterpoint_type == "cattle trough", training.set$status_group)[2,], p=a)

z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

coef.df <- as.data.frame(coef(summary(test)))
odds.ratio <- exp(coefficients(test))
odds.df <- as.data.frame(odds.ratio)
merged.df <- merge(coef.df, odds.df, by=0)


response.freqs <- table(training.set[,response.var])
response.props <- prop.table(margin.table(response.freqs, 1))
response.props.df <- as.data.frame(response.props)

total.func.prop <- response.props.df[response.props.df$Var1 == "functional", "Freq"]
total.repair.prop <- response.props.df[response.props.df$Var1 == "functional needs repair", "Freq"]
total.nonfunc.prop <- response.props.df[response.props.df$Var1 == "non functional", "Freq"]

null.model <- multinom(as.formula(paste(response.var, "1", sep="~")), data=training.set)
null.coef.df <- as.data.frame(coef(summary(null.model)))
null.odds.ratio <- exp(coefficients(null.model))
null.odds.df <- as.data.frame(null.odds.ratio)

categorical.predictor.df <- data.frame(
  predictor = character(),
  predictor.val = character(),
  num.obs = numeric(),
  #coef      = numeric(),
  #std.err   = numeric(),
  #wald      = numeric(),
  repair.p.val     = numeric(),
  nonfunc.p.val     = numeric(),
  val.func.idx    = numeric(),
  val.repair.idx    = numeric(),
  val.nonfunc.idx    = numeric()
)
not.useful.cols <- non.predictors.fxn(response.var)
for(col in col.names) {
  if(col %in% not.useful.cols) {next}
  if(class(training.set[,col]) == 'factor' && !(paste(col,".group", sep="")) %in% col.names ) {
    print("**********")
    print(col)
    single.multinom.lm <- multinom(as.formula(paste(response.var, col, sep="~")), data=training.set)
    single.multinom.lm.z <- summary(single.multinom.lm)$coefficients/summary(single.multinom.lm)$standard.errors
    single.multinom.lm.p <- (1 - pnorm(abs(single.multinom.lm.z), 0, 1)) * 2
    
    for(val in unique(training.set[,col])) {
      print(paste("    ", val))
      val.response.freqs <- table(training.set[,response.var], training.set[,col] == val)
      val.response.freqs <- as.table(val.response.freqs[,"TRUE"])
      val.response.props <- prop.table(margin.table(val.response.freqs, 1))
      val.response.props.df <- as.data.frame(val.response.props)

      val.func.prop <- val.response.props.df[val.response.props.df$Var1 == "functional", "Freq"]
      val.repair.prop <- val.response.props.df[val.response.props.df$Var1 == "functional needs repair", "Freq"]
      val.nonfunc.prop <- val.response.props.df[val.response.props.df$Var1 == "non functional", "Freq"]
      
      val.func.idx <- (val.func.prop / total.func.prop ) * 100
      val.repair.idx <- (val.repair.prop / total.repair.prop ) * 100
      val.nonfunc.idx <- (val.nonfunc.prop / total.nonfunc.prop) * 100
      
      val.col.name <- paste(col,val, sep="")
      if(val.col.name %in% colnames(single.multinom.lm.p)) {
        val.lm.p.repair <- single.multinom.lm.p[,val.col.name]["functional needs repair"] 
        val.lm.p.nonfunc <- single.multinom.lm.p[,val.col.name]["non functional"]  
      } else {
        val.lm.p.repair <- -1000
        val.lm.p.nonfunc <- -1000
      }
      
      val.row <- data.frame(
        predictor = col,
        predictor.val = val,
        num.obs = nrow(training.set[training.set[,col] == val,]),
        #coef      = numeric(),
        #std.err   = numeric(),
        #wald      = numeric(),
        repair.p.val     = val.lm.p.repair,
        nonfunc.p.val     = val.lm.p.nonfunc,
        val.func.idx    = val.func.idx,
        val.repair.idx    = val.repair.idx,
        val.nonfunc.idx    = val.nonfunc.idx
      )
      categorical.predictor.df <- rbind(categorical.predictor.df, val.row)
    }
  }
  else {
    
    #numeric.eda.fxn(training.set, col, response.var)
  }
}
write.csv(categorical.predictor.df, paste("output/eda/single_multinom_summary.csv", sep=""))

col <- "basin"
single.multinom.lm <- multinom(as.formula(paste(response.var, col, sep="~")), data=training.set)
single.multinom.lm.z <- summary(single.multinom.lm)$coefficients/summary(single.multinom.lm)$standard.errors
single.multinom.lm.p <- (1 - pnorm(abs(single.multinom.lm.z), 0, 1)) * 2
#summary(single.multinom.lm)
coef.df <- as.data.frame(coef(summary(single.multinom.lm)))
odds.ratio <- exp(coefficients(single.multinom.lm))
odds.df <- as.data.frame(odds.ratio)

basin.rufiji.response.freqs <- table(training.set[,response.var], training.set[,col] == "Rufiji")
basin.rufiji.response.freqs[,"TRUE"]