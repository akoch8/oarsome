setwd('~/Sites/pudding/')

library(data.table)

x = fread('rowers_results.txt', data.table=F)
x[x == 'None'] = NA
x$birthyear = as.numeric(x$birthyear)
x$first = as.numeric(x$first)
x$last = as.numeric(x$last)
dim(x)
head(x)
str(x)

# Remove rowers with missing data.
x = na.omit(x)
nrow(x)

# Remove any duplicate entries.
x = unique(x)
nrow(x)

# Which are the most and least common countries?
countries = table(x$country)
length(countries)
countries = countries[order(countries, decreasing=T)]
head(countries, n=10)
tail(countries, n=10)

# Order the data by year of birth.
x = x[order(x$birthyear),]
head(x)

# Calculate the career length for each rower.
x$career = x$last - x$first + 1
summary(x$career)
hist(x$career)

# There appear to be some rowers with suspiciously long careers. Let's check
# if these are genuine.
nrow(x)
nrow(x[which(x$career > 40),])
x[which(x$career > 40),c('name', 'country', 'gender', 'birthyear', 'first', 'last', 'career')]

# Remove rowers with an unrealistically long career.
x = x[which(x$career < 40),]
nrow(x)

# Check how old the rowers were when they participated in their first competition.
x$startAge = x$first - x$birthyear
range(x$startAge)

# There are some very young rowers (could be coxwains), but a negative age at the
# first competition obviously does not make sense. Let's remove the youngest ones.
x = x[-which(x$startAge < 10),]
nrow(x)
range(x$startAge)

# Check how old the rowers were when they participated in their last competition.
x$endAge = x$last - x$birthyear
range(x$endAge)

hist(x$startAge, col='#85c4c999', border=NA, xlim=c(0, 100), ylim=c(0, 2500), breaks=seq(0, 100, 1))
hist(x$endAge, xlim=c(0, 80), col='#3b738f99', border=NA, ylim=c(0, 2500), breaks=seq(0, 100, 1), add=T)
abline(v=35, lty=1, lwd=1, col='#2e2e2e')

# Let's put a cutoff on the starting age. Most (if not all) elite international rowers
# start their career as a teenager or in their early twenties. If we put the cutoff at
# 35, we're sure to catch all of them (barring some rare exceptions). Rowers that start
# after 35 could be coxwains, master rowers or coastal rowers.
nrow(x[which(x$startAge < 35),]) / nrow(x) * 100
x = x[which(x$startAge < 35),]
plot(x$birthyear, x$career, bty='n', pch=20, col='#3b738f10', xlim=c(1900, 2019))

# There isn't a whole lot of data available from the early years. Let's try looking at
# the data for rowers born after 1950.
xSub = x[which(x$birthyear > 1950),]
nrow(xSub) / nrow(x) * 100
dim(xSub)
head(xSub)

# Functions for creating the figures.
formatName = function(name) {
	n = strsplit(name, split=' ')[[1]]
	formattedName = paste0(toupper(substring(n, 1, 1)), tolower(substring(n, 2)), collapse=' ')
	return(formattedName)
}
markSingleRower = function(uniqueBirthYears, rowerData, col) {
	if (nrow(rowerData) > 0) {
		yPosLine = length(uniqueBirthYears) - which(uniqueBirthYears == rowerData$birthyear) + 0.5
		segments(rowerData$first, yPosLine, rowerData$last + 1, yPosLine, lwd=2, col=col)
		segments(rowerData$first, yPosLine + 0.25, rowerData$first, yPosLine - 0.25, lwd=2, col=col)
		segments(rowerData$last + 1, yPosLine + 0.25, rowerData$last + 1, yPosLine - 0.25, lwd=2, col=col)
		rowerName = formatName(rowerData$name)
		par(xpd=T)
		if (rowerName == 'Alexander Koch') {
			rowerName = 'This is me!'
		}
		text(rowerData$first - 3, length(uniqueBirthYears) - which(uniqueBirthYears == rowerData$birthyear) + 0.5, rowerName, adj=c(1, 0.5), col=col, font=2)
		par(xpd=F)
	}
}
markSingleRower2 = function(uniqueBirthYears, rowerData, col) {
	if (nrow(rowerData) > 0) {
		yPosLine = length(uniqueBirthYears) - which(uniqueBirthYears == rowerData$birthyear) + 0.5
		segments(rowerData$startAge, yPosLine, rowerData$endAge + 1, yPosLine, lwd=2, col=col)
		segments(rowerData$startAge, yPosLine + 0.25, rowerData$startAge, yPosLine - 0.25, lwd=2, col=col)
		segments(rowerData$endAge + 1, yPosLine + 0.25, rowerData$endAge + 1, yPosLine - 0.25, lwd=2, col=col)
		rowerName = formatName(rowerData$name)
		par(xpd=T)
		if (rowerName == 'Alexander Koch') {
			rowerName = 'This is me!'
		}
		text(rowerData$startAge - 3, length(uniqueBirthYears) - which(uniqueBirthYears == rowerData$birthyear) + 0.5, rowerName, adj=c(1, 0.5), col=col, font=2)
		par(xpd=F)
	}
}
plotCareersByYOB = function(file, data, uniqueBirthYears, rectCol) {
	png(file, width=10, height=12, units='in', res=150)

	# Randomize the rows to avoid distracting patterns in the figure.
	data = data[sample(nrow(data)),]
	data = data[order(data$birthyear),]
	par(mar=c(4, 5, 2, 2))
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1960, 2020), ylim=c(0, length(uniqueBirthYears)))
	title(xlab='Year', col.lab='#3f3f3f', line=2, cex=1.5)
	title(ylab='Year of birth', col.lab='#3f3f3f', line=3.5, cex=1.5)
	
	# Mark the Olympic Games.
	olympics = seq(1960, 2016, 4)
	for (i in 1:length(olympics)) {
		abline(v=olympics[i] + 1, col='#f3f3f3', lwd=2)
	}
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$birthyear == birthYear),]
		if (nrow(yearData) > 0) {
			for (j in 1:nrow(yearData)) {
				rect(yearData$first[j], length(uniqueBirthYears) - i + 1, yearData$last[j] + 1, length(uniqueBirthYears) - i, border=NA, col=rectCol)
			}
		}
	}
	
	# Add lines showing the median start and end of all the careers for each birthyear.
	avgStart = rep(NA, length(uniqueBirthYears))
	avgEnd = rep(NA, length(uniqueBirthYears))
	avgCareer = rep(NA, length(uniqueBirthYears))
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$birthyear == birthYear),]
		if (nrow(yearData) > 0) {
			avgStart[i] = median(yearData$first)
			avgEnd[i] = median(yearData$last + 1)
			avgCareer[i] = median(yearData$career)
		}
	}
	points(avgStart, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	points(avgEnd, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	
	# Mark particular rowers.
	markSingleRower(uniqueBirthYears, data[which(data$name == 'Steve REDGRAVE'),], '#3f3f3f')
	markSingleRower(uniqueBirthYears, data[which(data$name == 'Elisabeta LIPA-OLENIUC'),], '#3f3f3f')
	markSingleRower(uniqueBirthYears, data[which(data$name == 'Alexander KOCH' & data$country == 'BEL'),], '#3f3f3f')
	longestCareer = data[which(data$career == max(data$career)),]
	markSingleRower(uniqueBirthYears, data[which(data$name == longestCareer$name),], '#3f3f3f')
	
	# Add the axes.
	axis(1, at=seq(1960, 2020, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(1, at=seq(1960, 2020, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
	axis(2, at=seq(3, 53, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(2, at=seq(3, 53, 10), labels=seq(2000, 1950, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)
	dev.off()
}
plotCareersByStartAge = function(file, data, uniqueBirthYears, rectCol) {
	png(file, width=10, height=12, units='in', res=150)
	
	# Randomize the rows to avoid distracting patterns in the figure.
	data = data[sample(nrow(data)),]
	data = data[order(data$birthyear),]
	par(mar=c(4, 5, 2, 2))
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(0, 65), ylim=c(0, length(uniqueBirthYears)))
	title(xlab='Age', col.lab='#3f3f3f', line=2, cex=1.5)
	title(ylab='Year of birth', col.lab='#3f3f3f', line=3.5, cex=1.5)
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$birthyear == birthYear),]
		if (nrow(yearData) > 0) {
			for (j in 1:nrow(yearData)) {
				rect(yearData$startAge[j], length(uniqueBirthYears) - i + 1, yearData$endAge[j] + 1, length(uniqueBirthYears) - i, border=NA, col=rectCol)
			}
		}
	}
	
	# Add lines showing the median start and end of all the careers for each birthyear.
	avgStart = rep(NA, length(uniqueBirthYears))
	avgEnd = rep(NA, length(uniqueBirthYears))
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$birthyear == birthYear),]
		if (nrow(yearData) > 0) {
			avgStart[i] = median(yearData$startAge)
			avgEnd[i] = median(yearData$endAge + 1)
		}
	}
	points(avgStart, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	points(avgEnd, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	
	# Mark particular rowers.
	markSingleRower2(uniqueBirthYears, data[which(data$name == 'Steve REDGRAVE'),], '#3f3f3f')
	markSingleRower2(uniqueBirthYears, data[which(data$name == 'Elisabeta LIPA-OLENIUC'),], '#3f3f3f')
	markSingleRower2(uniqueBirthYears, data[which(data$name == 'Alexander KOCH' & data$country == 'BEL'),], '#3f3f3f')
	longestCareer = data[which(data$career == max(data$career)),]
	markSingleRower2(uniqueBirthYears, data[which(data$name == longestCareer$name),], '#3f3f3f')
	
	# Add the axes.
	axis(1, at=seq(0, 65, 5), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(1, at=seq(0, 65, 5), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
	axis(2, at=seq(3, 53, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(2, at=seq(3, 53, 10), labels=seq(2000, 1950, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)
	dev.off()
}

# Plot the career data by year of birth (and calendar year).
uniqueBirthYears = unique(xSub$birthyear)
plotCareersByYOB('img/careersByYearOfBirth.png', xSub, uniqueBirthYears, '#3b738f10')

# Plot the career data by year of birth and start age.
plotCareersByStartAge('img/careersByStartAge.png', xSub, uniqueBirthYears, '#3b738f10')

# Split up the athletes in two groups based on:
# - gender (color inspiration from: https://blog.datawrapper.de/gendercolor/)
# - started racing as a junior
# - best result = medal
# - lightweight vs heavyweight
xSubSub = xSub[which(xSub$gender == 'M'),]
plotCareersByYOB('img/careersByYearOfBirth-men.png', xSubSub, uniqueBirthYears, '#18847610')
plotCareersByStartAge('img/careersByStartAge-men.png', xSubSub, uniqueBirthYears, '#18847610')
xSubSub = xSub[which(xSub$gender == 'W'),]
plotCareersByYOB('img/careersByYearOfBirth-women.png', xSubSub, uniqueBirthYears, '#591a8e10')
plotCareersByStartAge('img/careersByStartAge-women.png', xSubSub, uniqueBirthYears, '#591a8e10')
startedAsJunior = grepl('J', xSub$results)
xSubSub = xSub[startedAsJunior,]
plotCareersByYOB('img/careersByYearOfBirth-junior.png', xSubSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersByStartAge-junior.png', xSubSub, uniqueBirthYears, '#3b738f10')
xSubSub = xSub[!startedAsJunior,]
plotCareersByYOB('img/careersByYearOfBirth-senior.png', xSubSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersByStartAge-senior.png', xSubSub, uniqueBirthYears, '#3b738f10')
medalled = grepl('FA? Final,[123]', xSub$results)
xSubSub = xSub[medalled,]
plotCareersByYOB('img/careersByYearOfBirth-medal.png', xSubSub, uniqueBirthYears, '#f1a34010')
plotCareersByStartAge('img/careersByStartAge-medal.png', xSubSub, uniqueBirthYears, '#f1a34010')
xSubSub = xSub[!medalled,]
plotCareersByYOB('img/careersByYearOfBirth-no-medal.png', xSubSub, uniqueBirthYears, '#88888810')
plotCareersByStartAge('img/careersByStartAge-no-medal.png', xSubSub, uniqueBirthYears, '#88888810')
weight = grepl('L[MW]', xSub$results)
xSubSub = xSub[weight,]
plotCareersByYOB('img/careersByYearOfBirth-light.png', xSubSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersByStartAge-light.png', xSubSub, uniqueBirthYears, '#3b738f10')
xSubSub = xSub[!weight,]
plotCareersByYOB('img/careersByYearOfBirth-heavy.png', xSubSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersByStartAge-heavy.png', xSubSub, uniqueBirthYears, '#3b738f10')

