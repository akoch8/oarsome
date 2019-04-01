setwd('~/Sites/pudding/')

library(data.table)

# Load the MLB data.
mlb = fread('mlb_player_data.csv', data.table=F)
mlb = mlb[,c('birthYear', 'birthCountry', 'nameFirst', 'nameLast', 'debut', 'finalGame')]
mlb$name = paste0(mlb$nameFirst, ' ', mlb$nameLast)
mlb = mlb[,!grepl('nameFirst|nameLast', colnames(mlb))]
mlb$year_start = as.numeric(sub('-.+$', '', mlb$debut))
mlb$year_end = as.numeric(sub('-.+$', '', mlb$finalGame))
mlb = mlb[,!grepl('debut|finalGame', colnames(mlb))]
colnames(mlb)[1:2] = c('year_birth', 'country_birth')
mlb[mlb == ''] = NA
mlb = na.omit(mlb)
mlb = mlb[order(mlb$year_birth),]
mlb$career = mlb$year_end - mlb$year_start + 1
mlb$age_start = mlb$year_start - mlb$year_birth
mlb$age_end = mlb$year_end - mlb$year_birth

# Load the NBA data.
nba = fread('nba_player_data.csv', data.table=F)
nba[nba == ''] = NA
nba = na.omit(nba)
nba$year_birth = as.numeric(gsub('^.+, ', '', nba$birth_date))
nba = nba[order(nba$year_birth),]
nba$career = nba$year_end - nba$year_start + 1
nba$age_start = nba$year_start - nba$year_birth
nba$age_end = nba$year_end - nba$year_birth

# Combine the two datasets.
yob = max(min(nba$year_birth), min(mlb$year_birth))
nba = nba[which(nba$year_birth >= yob),]
mlb = mlb[which(mlb$year_birth >= yob),]
columns = colnames(nba)[colnames(nba) %in% colnames(mlb)]
nba = nba[,columns]
mlb = mlb[,columns]
x = rbind(nba, mlb)
x$source = c(rep('nba', nrow(nba)), rep('mlb', nrow(mlb)))

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
	plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1930, 2020), ylim=c(0, length(uniqueBirthYears)))
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
	axis(1, at=seq(1930, 2020, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(1, at=seq(1930, 2020, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
	axis(2, at=seq(4, 84, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(2, at=seq(4, 84, 10), labels=seq(1995, 1915, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)
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
	axis(2, at=seq(4, 84, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
	axis(2, at=seq(4, 84, 10), labels=seq(1995, 1915, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)
	dev.off()
}

# Plot the career data by year of birth (and calendar year).
uniqueBirthYears = seq(min(x$year_birth), max(x$year_birth))
plotCareersByYOB('img/careersMLB-NBAByYearOfBirth.png', x, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays', 'Michael Jordan', 'Kareem Abdul-Jabbar'))

# Plot the career data by year of birth and start age.
plotCareersByStartAge('img/careersMLB-NBAByStartAge.png', x, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays', 'Michael Jordan', 'Kareem Abdul-Jabbar'))

# Split up the data in NBA and MLB athletes.
xSub = x[which(x$source == 'mlb'),]
plotCareersByYOB('img/careersMLB-NBAByYearOfBirth-mlb.png', xSub, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays'))
plotCareersByStartAge('img/careersMLB-NBAByStartAge-mlb.png', xSub, uniqueBirthYears, '#3b738f10', c('Babe Ruth', 'Willie Mays'))
xSub = x[which(x$source == 'nba'),]
plotCareersByYOB('img/careersMLB-NBAByYearOfBirth-nba.png', xSub, uniqueBirthYears, '#3b738f10', c('Michael Jordan', 'Kareem Abdul-Jabbar'))
plotCareersByStartAge('img/careersMLB-NBAByStartAge-nba.png', xSub, uniqueBirthYears, '#3b738f10', c('Michael Jordan', 'Kareem Abdul-Jabbar'))


