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

# Let's remove those rowers that have a large gap in their competitive career as
# these are more likely to be errors in the data.
#suspiciousRowers = x[which(x$career > 30),]
#gaps = vector()
#for (i in 1:nrow(suspiciousRowers)) {
#	years = suspiciousRowers$competition_years[i]
#	years = as.numeric(unique(unlist(strsplit(years, ','))))
#	years = years[order(years)]
#	maxGap = 0
#	for (j in 1:(length(years) - 1)) {
#		gap = years[j + 1] - years[j]
#		maxGap = ifelse(gap > maxGap, gap, maxGap)
#	}
#	gaps[i] = maxGap
#}
#suspiciousRowers = suspiciousRowers[which(gaps > 10),]
#x = x[!rownames(x) %in% rownames(suspiciousRowers),]

# Check how old the rowers were when they participated in their first competition.
x$startAge = x$first - x$birthyear
range(x$startAge)

# There are some very young rowers (could be coxwains), but a negative age at the
# first competition obviously does not make sense. Let's remove the youngest ones.
x = x[-which(x$startAge < 10),]
nrow(x)
range(x$startAge)
#startAgeTable = table(x$startAge)

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

# Plot the career data by year of birth (and age).
#ede5cf,#e0c2a2,#d39c83,#c1766f,#a65461,#813753,#541f3f
#d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674
#freqCol = colorRampPalette(c('#d1eeea', '#a8dbd9', '#85c4c9', '#68abb8', '#4f90a6', '#3b738f', '#2a5674'))(10)
#freqCol = c(
#	rep(freqCol[1], 1),
#	rep(freqCol[2], 1),
#	rep(freqCol[3], 1),
#	rep(freqCol[4], 2),
#	rep(freqCol[5], 2),
#	rep(freqCol[6], 3),
#	rep(freqCol[7], 5),
#	rep(freqCol[8], 10),
#	rep(freqCol[9], 25),
#	rep(freqCol[10], 50)
#)
uniqueBirthYears = unique(x$birthyear)
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(0, 100), ylim=c(0, length(uniqueBirthYears)))
colorsUsed = vector()
for (i in 1:length(uniqueBirthYears)) {
	birthYear = uniqueBirthYears[i]
	yearData = x[which(x$birthyear == birthYear),]
	for (j in 1:nrow(yearData)) {
		rect(yearData$startAge[j], i - 1, yearData$endAge[j], i, border=NA, col='#3b738f10')
	}
	#for (j in 1:100) {
	#	yearDataSub = yearData[which(yearData$startAge <= j & yearData$endAge >= j),]
	#	if (nrow(yearDataSub) > 0) {
	#		rectCol = freqCol[ceiling(nrow(yearDataSub) / max(startAgeTable) * 100)]
	#		rect(j - 1, i - 1, j, i, border=NA, col=rectCol)
	#		colorsUsed = c(colorsUsed, rectCol)
	#	}
	#}
}
axis(1, at=seq(0, 100, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(1, at=seq(0, 100, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
axis(2, at=seq(0, 100, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(2, at=seq(0, 100, 10), labels=seq(1900, 2000, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)

# Plot the career data by year of birth (and calendar year).
uniqueBirthYears = unique(x$birthyear)
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1900, 2020), ylim=c(0, length(uniqueBirthYears)))
for (i in 1:length(uniqueBirthYears)) {
	birthYear = uniqueBirthYears[i]
	yearData = x[which(x$birthyear == birthYear),]
	for (j in 1:nrow(yearData)) {
		rect(yearData$first[j], i - 1, yearData$last[j] + 1, i, border=NA, col='#3b738f10')
	}
}
axis(1, at=seq(1900, 2020, by=20), labels=NA, col.axis='#3f3f3f', col='#3f3f3f', lwd=0.5)
axis(1, at=seq(1900, 2020, by=20), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
axis(2, at=seq(0, 100, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(2, at=seq(0, 100, 10), labels=seq(1900, 2000, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)




# There is much less data available from the early years. Let's try looking at
# the data for rowers born after 1950.
xSub = x[which(x$birthyear > 1950),]
#xSub = xSub[which(xSub$gender == 'W'),]
nrow(xSub) / nrow(x) * 100
dim(xSub)
head(xSub)



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
				rect(yearData$first[j], length(uniqueBirthYears) - i + 1, yearData$last[j] + 1, length(uniqueBirthYears) - i, border=NA, col=rectCol)#541f3f05
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
plotCareersByYOB('img/careersByYearOfBirth-6.png', xSub, uniqueBirthYears, '#3b738f10')

# Plot the career data by year of birth and start age.
plotCareersByStartAge('img/careersByStartAge-3.png', xSub, uniqueBirthYears, '#3b738f10')

# Split up the athletes in two groups based on:
# - gender
# - started racing as a junior
# - best result = medal
xSubSub = xSub[which(xSub$gender == 'M'),]
plotCareersByYOB('img/careersByYearOfBirth-men-2.png', xSubSub, uniqueBirthYears, '#18847610')
plotCareersByStartAge('img/careersByStartAge-men-2.png', xSubSub, uniqueBirthYears, '#18847610')
xSubSub = xSub[which(xSub$gender == 'W'),]
plotCareersByYOB('img/careersByYearOfBirth-women-2.png', xSubSub, uniqueBirthYears, '#591a8e10')
plotCareersByStartAge('img/careersByStartAge-women-2.png', xSubSub, uniqueBirthYears, '#591a8e10')
startedAsJunior = grepl('J', xSub$results)
xSubSub = xSub[startedAsJunior,]
plotCareersByYOB('img/careersByYearOfBirth-junior-1.png', xSubSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersByStartAge-junior-1.png', xSubSub, uniqueBirthYears, '#3b738f10')
xSubSub = xSub[!startedAsJunior,]
plotCareersByYOB('img/careersByYearOfBirth-senior-1.png', xSubSub, uniqueBirthYears, '#3b738f10')
plotCareersByStartAge('img/careersByStartAge-senior-1.png', xSubSub, uniqueBirthYears, '#3b738f10')
medalled = grepl('FA? Final,[123]', xSub$results)
xSubSub = xSub[medalled,]
plotCareersByYOB('img/careersByYearOfBirth-medal-1.png', xSubSub, uniqueBirthYears, '#f1a34010')
plotCareersByStartAge('img/careersByStartAge-medal-1.png', xSubSub, uniqueBirthYears, '#f1a34010')
xSubSub = xSub[!medalled,]
plotCareersByYOB('img/careersByYearOfBirth-no-medal-1.png', xSubSub, uniqueBirthYears, '#88888810')
plotCareersByStartAge('img/careersByStartAge-no-medal-1.png', xSubSub, uniqueBirthYears, '#88888810')









# Plot the career data by year of birth (and age).
uniqueBirthYears = unique(xSub$birthyear)
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(0, 70), ylim=c(0, 50))
for (i in 1:length(uniqueBirthYears)) {
	birthYear = uniqueBirthYears[i]
	yearData = xSub[which(xSub$birthyear == birthYear),]
	for (j in 1:nrow(yearData)) {
		rect(yearData$first[j] - birthYear, i - 1, yearData$last[j] + 1 - birthYear, i, border=NA, col='#541f3f05')
	}
}
axis(1, at=seq(0, 70, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(1, at=seq(0, 70, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
axis(2, at=seq(5, 45, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(2, at=seq(5, 45, 10), labels=seq(1950, 1990, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)

# Draw all careers individually.
xSub = xSub[order(xSub$first),]
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1950, 2020), ylim=c(0, nrow(xSub)))
for (i in 1:nrow(xSub)) {
	rect(xSub$birthyear[i], i - 1, xSub$first[i], i, border=NA, col='#ede5cf')
	rect(xSub$first[i], i - 1, xSub$last[i] + 1, i, border=NA, col='#541f3f')
}

xSub = xSub[order(xSub$first),]
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(0, 80), ylim=c(0, nrow(xSub)))
for (i in 1:nrow(xSub)) {
	#rect(xSub$startAge[i], i - 1, xSub$first[i], i, border=NA, col='#ede5cf')
	rect(xSub$startAge[i], i - 1, xSub$endAge[i] + 1, i, border=NA, col='#541f3f')
}


head(xSub)
max(xSub$career)
max(xSub$startAge)


xSub = xSub[order(xSub$birthyear),]
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(0, max(xSub$career)), ylim=c(0, nrow(xSub)))
for (i in 1:nrow(xSub)) {
	#rect(xSub$startAge[i], i - 1, xSub$first[i], i, border=NA, col='#ede5cf')
	rect(0, i - 1, xSub$career[i], i, border=NA, col='#541f3f')
}


xSub = xSub[sample(nrow(xSub)),]
xSub = xSub[order(xSub$birthyear),]
par(mar=c(4, 4, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(-max(xSub$startAge), max(xSub$career)), ylim=c(0, nrow(xSub)))
for (i in 1:nrow(xSub)) {
	rect(-xSub$startAge[i], nrow(xSub) - i + 1, 0, nrow(xSub) - i, border=NA, col='#c1766f')
	rect(0, nrow(xSub) - i + 1, xSub$career[i], nrow(xSub) - i, border=NA, col='#541f3f')
}



