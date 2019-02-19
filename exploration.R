setwd('~/Sites/pudding/')

library(data.table)

x = fread('rowers.txt', data.table=F)
x[x == 'None'] = NA
x$birthyear = as.numeric(x$birthyear)
x$first = as.numeric(x$first)
x$last = as.numeric(x$last)
dim(x)
head(x)
str(x)

