library("lattice")
library("stringr")

setwd("/Users/greg.bolla/Desktop/git-projects/driven-data-taarifa")

training.values <- read.csv("data/training_set_values.csv", header = TRUE)
# num rows of test is 54000. divide into validation set
test.values <- read.csv("data/training_set_values.csv", header = TRUE)

training.labels <- read.csv("data/training_set_labels.csv", header = TRUE)

training.set <- merge(training.values, training.labels, by = "id")

training.set[,'construction_year'] <- as.factor(training.set[, 'construction_year'])
training.set[,'date_recorded'] <- as.character(training.set[,'date_recorded'])
col.names <- colnames(training.set)
response.var <- 'status_group'

response.freqs <- table(training.set[,response.var])
response.props <- prop.table(margin.table(response.freqs, 1))
#amount_tsh
##### 
# ***summary:
# amount_tsh is highly skewed to the left (>70% have val of 0)
# > 50% of rows with amount_tsh == 0 are non-func or non-func-needs-repair
# > 70% of rows with amount_tsh > 0 are func
predictor.var <- 'amount_tsh'
two.col.df <- training.set[, c(predictor.var, response.var)]
table(two.col.df[,predictor.var] == 0)
table(two.col.df[,predictor.var] == 0, two.col.df[,response.var])

#####
# date_recorded 
#####
# *** summary: nothing of particular interest
# makes sense since the date a waterpoint was measured prob doesn't have much effect on whether it's functional
predictor.var <- 'date_recorded'
two.col.df <- training.set[, c(predictor.var, response.var)]
two.col.df[,c("year", "month", "day")] <- str_split_fixed(two.col.df[,predictor.var], '-', 3)
two.col.df[,"year"] <- as.factor(two.col.df[,"year"])
table(two.col.df[,'year'], two.col.df[,response.var])
# indiv date probably too granular to be useful. separate by year & month

#####
# funder 
#####
# summary: way too many levels (~1800) in raw data to be useful. 
# this aggregates those levels up to only 5 
# -- large funder ( > 1000 wells) with above & below average functional,
# -- med funder (bt 100 & 1000 wells) with above and below average functional,
# -- small funders (< 100), * not divided into above/below
predictor.var <- 'funder'
two.col.df <- training.set[, c(predictor.var, response.var)]
# var has many levels. this gets frequency of each level
funder.freq <- as.data.frame(table(two.col.df[,predictor.var]))
colnames(funder.freq) <- c("funder.var", "count")
rownames(funder.freq) <- funder.freq$funder.var
funder.freq <- funder.freq[,"count", drop=FALSE]
#funder.response.freq <- as.data.frame.matrix(table(two.col.df[,predictor.var], two.col.df[,response.var]))
funder.response.freq <- table(two.col.df[,predictor.var], two.col.df[,response.var])
funder.response.prop <- as.data.frame.matrix(prop.table(funder.response.freq, 1))
funder.df <- merge(funder.freq, funder.response.prop, by=0)
rownames(funder.df) <- funder.df$Row.names
funder.df$perc.obs <- funder.df$count / nrow(training.set)
funder.df <- funder.df[,c("count", "functional", "functional needs repair", "non functional")]
funder.df$funder.group <- NULL
funder.df$funder.group <- with(funder.df, 
                               ifelse(count < 100, 'small', 
                               ifelse(count < 1000 & functional > response.props["functional"], 'medium above',
                               ifelse(count < 1000 & functional <= response.props["functional"], 'medium below',         
                               ifelse(count > 1000 & functional > response.props["functional"], 'large above', 
                               'large below')))))
small.funders <- row.names(funder.df)[apply(funder.df, 1, function(row) row['funder.group']=='small')]
med.above.funders <- row.names(funder.df)[apply(funder.df, 1, function(row) row['funder.group']=='medium above')]
med.below.funders <- row.names(funder.df)[apply(funder.df, 1, function(row) row['funder.group']=='medium below')]
large.above.funders <- row.names(funder.df)[apply(funder.df, 1, function(row) row['funder.group']=='large above')]
large.below.funders <- row.names(funder.df)[apply(funder.df, 1, function(row) row['funder.group']=='large below')]
funder.group <- apply(two.col.df, 1, function(row) {
  ifelse(row['funder'] %in% small.funders, 'small', 
  ifelse(row['funder'] %in% med.above.funders, 'medium above', 
  ifelse(row['funder'] %in% med.below.funders, 'medium below', 
  ifelse(row['funder'] %in% large.above.funders, 'large above',
  'large below'))))
})
  
two.col.df <- cbind(two.col.df, funder.group)
table(two.col.df[,response.var], two.col.df[,'funder.group'])
prop.table(table(two.col.df[,response.var], two.col.df[,'funder.group']), 2)


write.csv(funder.df, "output/funder_detail.csv")
#funder.response.freq$funder.var <- rownames(funder.response.freq)
#rownames(funder.response.freq) <- NULL
# Divide into small / medium

#####
# gps_height
#####
predictor.var <- 'gps_height'
# summary: continuous variable, the higher the height, the more likely it seems to be functional
# splitting at 3rd quartile value has 64.5% func / 6.0% func needs repair / 29.4% not func
# gps_height of 0 has more non-func than func
# 
two.col.df <- training.set[, c(predictor.var, response.var)]
summary(two.col.df[,predictor.var])
prop.table(table(two.col.df[,predictor.var] > 1319.2, two.col.df[,response.var]), 1)
densityplot(~ gps_height, groups = status_group, data = two.col.df, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))
#####
# installer
#####
# summary : similar to funders, many different levels to account for
# medium (bt 100 and 500) see highest disparity between above / below averages
# larger installers have a slighter disparity bt above/below average functional, but still considerable
predictor.var <- 'installer'
two.col.df <- training.set[, c(predictor.var, response.var)]
installer.freq <- as.data.frame(table(two.col.df[,predictor.var]))
colnames(installer.freq) <- c("installer.var", "count")
write.csv(installer.freq, 'output/installer_freq.csv')
rownames(installer.freq) <- installer.freq$installer.var
installer.freq <- installer.freq[,"count", drop=FALSE]
#funder.response.freq <- as.data.frame.matrix(table(two.col.df[,predictor.var], two.col.df[,response.var]))
installer.response.freq <- table(two.col.df[,predictor.var], two.col.df[,response.var])
installer.response.prop <- as.data.frame.matrix(prop.table(installer.response.freq, 1))
installer.df <- merge(installer.freq, installer.response.prop, by=0)
rownames(installer.df) <- installer.df$Row.names
installer.df$perc.obs <- installer.df$count / nrow(training.set)
installer.df <- installer.df[,c("count", "functional", "functional needs repair", "non functional")]
installer.df$installer.group <- NULL
installer.df$installer.group <- with(installer.df, 
                               ifelse(count < 100, 'small', 
                                      ifelse(count < 500 & functional > response.props["functional"], 'medium above',
                                             ifelse(count < 500 & functional <= response.props["functional"], 'medium below',         
                                                    ifelse(count > 500 & functional > response.props["functional"], 'large above', 
                                                           'large below')))))
small.installers <- row.names(installer.df)[apply(installer.df, 1, function(row) row['installer.group']=='small')]
med.above.installers <- row.names(installer.df)[apply(installer.df, 1, function(row) row['installer.group']=='medium above')]
med.below.installers <- row.names(installer.df)[apply(installer.df, 1, function(row) row['installer.group']=='medium below')]
large.above.installers <- row.names(installer.df)[apply(installer.df, 1, function(row) row['installer.group']=='large above')]
large.below.installers <- row.names(installer.df)[apply(installer.df, 1, function(row) row['installer.group']=='large below')]
installer.group <- apply(two.col.df, 1, function(row) {
  ifelse(row['installer'] %in% small.installers, 'small', 
         ifelse(row['installer'] %in% med.above.installers, 'medium above', 
                ifelse(row['installer'] %in% med.below.installers, 'medium below', 
                       ifelse(row['installer'] %in% large.above.installers, 'large above',
                              'large below'))))
})

two.col.df <- cbind(two.col.df, installer.group)
table(two.col.df[,response.var], two.col.df[,'installer.group'])
prop.table(table(two.col.df[,response.var], two.col.df[,'installer.group']), 2)
write.csv(installer.df, "output/installer_detail.csv")

#####
# longitude
#####
# *** summary
# - peak at 0 longitude (or unknown?) which has more non-func than func
# - peak of func wells bt ~33-38 longitude, other longitude have either non func or repair as more numerous 
predictor.var <- 'longitude'
two.col.df <- training.set[,c("id",predictor.var, response.var)]
summary(two.col.df[,predictor.var])
densityplot(~ longitude, groups = status_group, data = two.col.df, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))
#####
# latitude
#####
# *** summary
# - two peaks of func wells around -9 lat and -3 lat. 
# - repair and non-func more numerous at other lats
predictor.var <- 'latitude'
two.col.df <- training.set[,c("id", predictor.var, response.var)]
summary(two.col.df[,predictor.var])
densityplot(~ latitude, groups = status_group, data = two.col.df, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))

#####
# wpt_name
#####
predictor.var <- 'wpt_name'
two.col.df <- training.set[,c("id", predictor.var, response.var)]
wpt_name.freq <- as.data.frame(table(two.col.df[,predictor.var]))
colnames(wpt_name.freq) <- c("wpt_name", "count")
write.csv(wpt_name.freq, 'output/wpt_name_freq.csv')

wpt_name.freq <- wpt_name.freq[,"count", drop=FALSE]
wpt_name.response.freq <- table(two.col.df[,predictor.var], two.col.df[,response.var])
wpt_name.response.prop <- as.data.frame.matrix(prop.table(wpt_name.response.freq, 1))
wpt_name.df <- merge(wpt_name.freq, wpt_name.response.prop, by=0)
rownames(wpt_name.df) <- wpt_name.df$Row.names
wpt_name.df$perc.obs <- wpt_name.df$count / nrow(training.set)
wpt_name.df <- wpt_name.df[,c("count", "functional", "functional needs repair", "non functional")]
wpt_name.df$wpt_name.group <- NULL
wpt_name.df$wpt_name.group <- with(wpt_name.df, 
                                     ifelse(count < 50, 'small', 
                                            ifelse(count < 100 & functional > response.props["functional"], 'medium above',
                                                   ifelse(count < 100 & functional <= response.props["functional"], 'medium below',         
                                                          ifelse(count >= 100 & functional > response.props["functional"], 'large above', 
                                                                 'large below')))))
small.wpt_names <- row.names(wpt_name.df)[apply(wpt_name.df, 1, function(row) row['wpt_name.group']=='small')]
med.above.wpt_names <- row.names(wpt_name.df)[apply(wpt_name.df, 1, function(row) row['wpt_name.group']=='medium above')]
med.below.wpt_names <- row.names(wpt_name.df)[apply(wpt_name.df, 1, function(row) row['wpt_name.group']=='medium below')]
large.above.wpt_names <- row.names(wpt_name.df)[apply(wpt_name.df, 1, function(row) row['wpt_name.group']=='large above')]
large.below.wpt_names <- row.names(wpt_name.df)[apply(wpt_name.df, 1, function(row) row['wpt_name.group']=='large below')]
wpt_name.group <- apply(two.col.df, 1, function(row) {
  ifelse(row['wpt_name'] %in% small.wpt_names, 'small', 
         ifelse(row['wpt_name'] %in% med.above.wpt_names, 'medium above', 
                ifelse(row['wpt_name'] %in% med.below.wpt_names, 'medium below', 
                       ifelse(row['wpt_name'] %in% large.above.wpt_names, 'large above',
                              'large below'))))
})

two.col.df <- cbind(two.col.df, wpt_name.group)
table(two.col.df[,response.var], two.col.df[,'wpt_name.group'])
prop.table(table(two.col.df[,response.var], two.col.df[,'wpt_name.group']), 2)
write.csv(wpt_name.df, "output/wpt_name_detail.csv")

#####
# num_private
#####
# *** summary
# - 58,600 of 59,400 have same value (0). doesn't look like a useful predictor
predictor.var <- 'num_private'
two.col.df <- training.set[,c("id", predictor.var, response.var)]
as.data.frame(table(two.col.df[,predictor.var]))

#####
# basin
#####
# *** summary
# - only 9 distinct values, each with > 2400 rows
predictor.var <- 'basin'
two.col.df <- training.set[,c("id", predictor.var, response.var)]
basin.freq <- as.data.frame(table(two.col.df[,predictor.var]))
colnames(basin.freq) <- c("basin", "count")
write.csv(basin.freq, 'output/basin_freq.csv')

rownames(basin.freq) <- basin.freq$basin
basin.freq <- basin.freq[,"count", drop=FALSE]
basin.response.freq <- table(two.col.df[,predictor.var], two.col.df[,response.var])
basin.response.prop <- as.data.frame.matrix(prop.table(basin.response.freq, 1))
basin.df <- merge(basin.freq, basin.response.prop, by=0)
rownames(basin.df) <- basin.df$Row.names
basin.df$perc.obs <- basin.df$count / nrow(training.set)
basin.df <- basin.df[,c("count", "functional", "functional needs repair", "non functional")]
histogram(~ factor(status_group) | basin, data = two.col.df)
write.csv(basin.df, "output/basin_detail.csv")

#####
#
#####


#####
#
#####


#####
#
#####


#####
#
#####


#####
#
#####


#####
#
#####