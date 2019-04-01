setwd('~/Sites/pudding/')

library(data.table)

x = fread('mlb_player_data.csv', data.table=F)
dim(x)
head(x)
str(x)

# Select the columns of interest.
x = x[,c('birthYear', 'birthCountry', 'nameFirst', 'nameLast', 'debut', 'finalGame', 'bats', 'throws')]
head(x)

# And convert them to the format used in the NBA script.
x$name = paste0(x$nameFirst, ' ', x$nameLast)
x = x[,!grepl('nameFirst|nameLast', colnames(x))]
x$year_start = as.numeric(sub('-.+$', '', x$debut))
x$year_end = as.numeric(sub('-.+$', '', x$finalGame))
x = x[,!grepl('debut|finalGame', colnames(x))]
colnames(x)[1:2] = c('year_birth', 'country_birth')
head(x)

# Replace empty strings with NA.
x[x == ''] = NA

# Remove rows with missing data.
x = na.omit(x)

# Order the data by year of birth.
x = x[order(x$year_birth),]
head(x)

# Calculate the career length for each rower.
x$career = x$year_end - x$year_start + 1
head(x)

# Check how old the athletes were when they started their MLB career.
x$age_start = x$year_start - x$year_birth
range(x$age_start)

# Check how old the athletes were when they ended their MLB career.
x$age_end = x$year_end - x$year_birth
range(x$age_end)

hist(x$age_start, col='#85c4c999', border=NA, xlim=c(10, 60), ylim=c(0, 4000), breaks=seq(10, 60, 1))
hist(x$age_end, xlim=c(10, 60), col='#3b738f99', border=NA, ylim=c(0, 4000), breaks=seq(10, 60, 1), add=T)

# Functions for creating the figures.
markSingleAthlete = function(uniqueBirthYears, athleteData, type, col) {
	if (nrow(athleteData) > 0) {
		yPosLine = length(uniqueBirthYears) - which(uniqueBirthYears == athleteData$year_birth) + 0.5
		if (type == 'age') {
			start = athleteData$age_start
			end = athleteData$age_end
		} else if (type == 'year') {
			start = athleteData$year_start
			end = athleteData$year_end
		} else {
			message('Plot type should be "age" or "year".')
			break
		}
		segments(start, yPosLine, end + 1, yPosLine, lwd=2, col=col)
		segments(start, yPosLine + 0.25, start, yPosLine - 0.25, lwd=2, col=col)
		segments(end + 1, yPosLine + 0.25, end + 1, yPosLine - 0.25, lwd=2, col=col)
		par(xpd=T)
		text(start - 3, length(uniqueBirthYears) - which(uniqueBirthYears == athleteData$year_birth) + 0.5, athleteData$name, adj=c(1, 0.5), col=col, font=2)
		par(xpd=F)
	}
}
plotCareersByYOB = function(file, data, uniqueBirthYears, rectCol, athletes=vector()) {
	png(file, width=10, height=12, units='in', res=150)

	# Randomize the rows to avoid distracting patterns in the figure.
	data = data[sample(nrow(data)),]
	data = data[order(data$year_birth),]
	par(mar=c(4, 5, 2, 2))
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1870, 2020), ylim=c(0, length(uniqueBirthYears)))
	title(xlab='Year', col.lab='#3f3f3f', line=2, cex=1.5)
	title(ylab='Year of birth', col.lab='#3f3f3f', line=3.5, cex=1.5)
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$year_birth == birthYear),]
		if (nrow(yearData) > 0) {
			for (j in 1:nrow(yearData)) {
				rect(yearData$year_start[j], length(uniqueBirthYears) - i + 1, yearData$year_end[j] + 1, length(uniqueBirthYears) - i, border=NA, col=rectCol)
			}
		}
	}
	
	# Add lines showing the median start and end of all the careers for each birthyear.
	avgStart = rep(NA, length(uniqueBirthYears))
	avgEnd = rep(NA, length(uniqueBirthYears))
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$year_birth == birthYear),]
		if (nrow(yearData) > 0) {
			avgStart[i] = median(yearData$year_start)
			avgEnd[i] = median(yearData$year_end + 1)
		}
	}
	points(avgStart, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	points(avgEnd, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	
	# Mark particular athletes.
	if (length(athletes) > 0) {
		for (i in 1:length(athletes)) {
			if (nrow(data[which(data$name == athletes[i]),]) == 1) {
				markSingleAthlete(uniqueBirthYears, data[which(data$name == athletes[i]),], 'year', '#3f3f3f')
			}
		}
	}
	
	# Add the axes.
	axis(1, at=seq(1870, 2020, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(1, at=seq(1870, 2020, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
	axis(2, at=seq(4, 164, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(2, at=seq(4, 164, 10), labels=seq(1995, 1835, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)
	dev.off()
}
plotCareersByStartAge = function(file, data, uniqueBirthYears, rectCol, athletes=vector()) {
	png(file, width=10, height=12, units='in', res=150)
	
	# Randomize the rows to avoid distracting patterns in the figure.
	data = data[sample(nrow(data)),]
	data = data[order(data$year_birth),]
	par(mar=c(4, 5, 2, 2))
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(0, 60), ylim=c(0, length(uniqueBirthYears)))
	title(xlab='Age', col.lab='#3f3f3f', line=2, cex=1.5)
	title(ylab='Year of birth', col.lab='#3f3f3f', line=3.5, cex=1.5)
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$year_birth == birthYear),]
		if (nrow(yearData) > 0) {
			for (j in 1:nrow(yearData)) {
				rect(yearData$age_start[j], length(uniqueBirthYears) - i + 1, yearData$age_end[j] + 1, length(uniqueBirthYears) - i, border=NA, col=rectCol)
			}
		}
	}
	
	# Add lines showing the median start and end of all the careers for each birthyear.
	avgStart = rep(NA, length(uniqueBirthYears))
	avgEnd = rep(NA, length(uniqueBirthYears))
	for (i in 1:length(uniqueBirthYears)) {
		birthYear = uniqueBirthYears[i]
		yearData = data[which(data$year_birth == birthYear),]
		if (nrow(yearData) > 0) {
			avgStart[i] = median(yearData$age_start)
			avgEnd[i] = median(yearData$age_end + 1)
		}
	}
	points(avgStart, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	points(avgEnd, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
	
	# Mark particular athletes.
	if (length(athletes) > 0) {
		for (i in 1:length(athletes)) {
			if (nrow(data[which(data$name == athletes[i]),]) == 1) {
				markSingleAthlete(uniqueBirthYears, data[which(data$name == athletes[i]),], 'age', '#3f3f3f')
			}
		}
	}
	
	# Add the axes.
	axis(1, at=seq(0, 60, 5), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(1, at=seq(0, 60, 5), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
	axis(2, at=seq(4, 164, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(2, at=seq(4, 164, 10), labels=seq(1995, 1835, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)
	dev.off()
}

# Plot the career data by year of birth (and calendar year).
uniqueBirthYears = seq(min(x$year_birth), max(x$year_birth))
plotCareersByYOB('img/careersMLBByYearOfBirth.png', x, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays'))

# Plot the career data by year of birth and start age.
plotCareersByStartAge('img/careersMLBByStartAge.png', x, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays'))

# Split up the data in US born and non-US born athletes.
x$us_born = grepl('^USA$', x$country_birth)
xSub = x[x$us_born,]
plotCareersByYOB('img/careersMLBByYearOfBirth-us-born.png', xSub, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays'))
xSub = x[!x$us_born,]
plotCareersByYOB('img/careersMLBByYearOfBirth-non-us-born.png', xSub, uniqueBirthYears, '#3b738f10')

# Split up the data by the side a players bats on (left or right).
xSub = x[which(x$bats == 'L'),]
plotCareersByYOB('img/careersMLBByYearOfBirth-bats-left.png', xSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersMLBByStartAge-bats-left.png', xSub, uniqueBirthYears, '#3b738f10')
xSub = x[which(x$bats == 'R'),]
plotCareersByYOB('img/careersMLBByYearOfBirth-bats-right.png', xSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersMLBByStartAge-bats-right.png', xSub, uniqueBirthYears, '#3b738f10')
xSub = x[which(x$bats == 'B'),]
plotCareersByYOB('img/careersMLBByYearOfBirth-bats-both.png', xSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersMLBByStartAge-bats-both.png', xSub, uniqueBirthYears, '#3b738f10')

