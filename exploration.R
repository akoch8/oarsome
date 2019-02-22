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

# Remove rowers with missing data.
x = na.omit(x)
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
nrow(x[which(x$career > 30),])
x[which(x$career > 30),c('name', 'country', 'gender', 'birthyear', 'first', 'last')]

# Let's remove those rowers that have a large gap in their competitive career as
# these are more likely to be errors in the data.
suspiciousRowers = x[which(x$career > 30),]
gaps = vector()
for (i in 1:nrow(suspiciousRowers)) {
	years = suspiciousRowers$competition_years[i]
	years = as.numeric(unique(unlist(strsplit(years, ','))))
	years = years[order(years)]
	maxGap = 0
	for (j in 1:(length(years) - 1)) {
		gap = years[j + 1] - years[j]
		maxGap = ifelse(gap > maxGap, gap, maxGap)
	}
	gaps[i] = maxGap
}
suspiciousRowers = suspiciousRowers[which(gaps > 10),]
x = x[!rownames(x) %in% rownames(suspiciousRowers),]

# Check how old the rowers were when they participated in their first competition.
x$startAge = x$first - x$birthyear
range(x$startAge)

# There are some very young rowers (could be coxwains), but a negative age at the
# first competition obviously does not make sense. Let's remove the youngest ones.
x = x[-which(x$startAge < 10),]
range(x$startAge)
hist(x$startAge, xlim=c(0, 80), col='#85c4c999', border=NA, ylim=c(0, 2500), breaks=seq(0, 100, 1))
startAgeTable = table(x$startAge)
log(max(startAgeTable))
# Check how old the rowers were when they participated in their last competition.
x$endAge = x$last - x$birthyear
range(x$endAge)
hist(x$endAge, xlim=c(0, 80), col='#3b738f99', border=NA, ylim=c(0, 2500), breaks=seq(0, 100, 1), add=T)

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
		rect(yearData$first[j] - birthYear, i - 1, yearData$last[j] + 1 - birthYear, i, border=NA, col='#541f3f05')
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
axis(2, at=seq(0, 90, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(2, at=seq(0, 90, 10), labels=seq(1900, 1990, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)

# Plot the career data by year of birth (and calendar year).
uniqueBirthYears = unique(x$birthyear)
par(mar=c(2, 2, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1900, 2020), ylim=c(0, length(uniqueBirthYears)))
for (i in 1:length(uniqueBirthYears)) {
	birthYear = uniqueBirthYears[i]
	yearData = x[which(x$birthyear == birthYear),]
	for (j in 1:nrow(yearData)) {
		rect(yearData$first[j], i - 1, yearData$last[j] + 1, i, border=NA, col='#ff000010')
	}
}
axis(1, at=seq(1900, 2020, by=20), col.axis='#3f3f3f', col='#3f3f3f', lwd=0.5)

# There is much less data available from the early years. Let's try looking at
# the data for rowers born after WWII.
xSub = x[which(x$birthyear > 1945),]
#xSub = xSub[which(xSub$gender == 'W'),]
dim(xSub)
head(xSub)

# Plot the career data by year of birth (and calendar year).
png('img/careersByYearOfBirth.png', width=10, height=12, units='in', res=150)

xSub = xSub[sample(nrow(xSub)),]
xSub = xSub[order(xSub$birthyear),]
uniqueBirthYears = unique(xSub$birthyear)
par(mar=c(4, 5, 2, 2))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', xlim=c(1960, 2020), ylim=c(0, length(uniqueBirthYears)))
title(xlab='Year', col.lab='#3f3f3f', line=2, cex=1.5)
title(ylab='Year of birth', col.lab='#3f3f3f', line=3.5, cex=1.5)

# Mark the Olympic Games.
olympics = seq(1960, 2016, 4)
for (i in 1:length(olympics)) {
	rect(olympics[i], 0, olympics[i] + 1, length(uniqueBirthYears), border=NA, col='#f3f3f3')
}

for (i in 1:length(uniqueBirthYears)) {
	birthYear = uniqueBirthYears[i]
	yearData = xSub[which(xSub$birthyear == birthYear),]
	for (j in 1:nrow(yearData)) {
		rect(yearData$first[j], length(uniqueBirthYears) - i + 1, yearData$last[j] + 1, length(uniqueBirthYears) - i, border=NA, col='#541f3f05')
	}
}

# Add lines showing the average start and end of all the careers for each birthyear.
avgStart = rep(NA, length(uniqueBirthYears))
avgEnd = rep(NA, length(uniqueBirthYears))
avgCareer = rep(NA, length(uniqueBirthYears))
for (i in 1:length(uniqueBirthYears)) {
	birthYear = uniqueBirthYears[i]
	yearData = xSub[which(xSub$birthyear == birthYear),]
	avgStart[i] = median(yearData$first)
	avgEnd[i] = median(yearData$last + 1)
	avgCareer[i] = median(yearData$career)
}
#fef6b5,#ffdd9a,#ffc285,#ffa679,#fa8a76,#f16d7a,#e15383
points(avgStart, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)
points(avgEnd, seq(length(uniqueBirthYears), 1), type='s', col='#ffdd9a', lwd=2)

# Mark a particular rower.
redgrave = xSub[which(xSub$name == 'Steve REDGRAVE'),]
rect(redgrave$first, length(uniqueBirthYears) - which(uniqueBirthYears == redgrave$birthyear) + 1, redgrave$last + 1, length(uniqueBirthYears) - which(uniqueBirthYears == redgrave$birthyear), border='#3f3f3f', lwd=2, col=NA)
text(redgrave$last + 2, length(uniqueBirthYears) - which(uniqueBirthYears == redgrave$birthyear) + 0.5, 'Steve Redgrave', adj=c(0, 0.5), col='#3f3f3f', font=2)
#lipa = xSub[which(xSub$name == 'Elisabeta LIPA'),]
me = xSub[which(xSub$name == 'Alexander KOCH' & xSub$country == 'BEL'),]
rect(me$first, length(uniqueBirthYears) - which(uniqueBirthYears == me$birthyear) + 1, me$last + 1, length(uniqueBirthYears) - which(uniqueBirthYears == me$birthyear), border='#f3f3f3', lwd=2, col=NA)
text(me$last + 2, length(uniqueBirthYears) - which(uniqueBirthYears == me$birthyear) + 0.5, 'This is me!', adj=c(0, 0.5), col='#f3f3f3', font=2)
longestCareer = xSub[which(xSub$career == max(xSub$career)),]
longestCareerName = longestCareer$name
n = strsplit(longestCareerName, split=' ')[[1]]
longestCareerName = paste0(toupper(substring(n, 1, 1)), tolower(substring(n, 2)), collapse=' ')
rect(longestCareer$first, length(uniqueBirthYears) - which(uniqueBirthYears == longestCareer$birthyear) + 1, longestCareer$last + 1, length(uniqueBirthYears) - which(uniqueBirthYears == longestCareer$birthyear), border='#3f3f3f', lwd=2, col=NA)
text(longestCareer$last + 2, length(uniqueBirthYears) - which(uniqueBirthYears == longestCareer$birthyear) + 0.5, longestCareerName, adj=c(0, 0.5), col='#3f3f3f', font=2)

# Add the axes.
axis(1, at=seq(1960, 2020, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(1, at=seq(1960, 2020, 10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.5)
axis(2, at=seq(5, 45, 10), labels=NA, col='#3f3f3f', col.axis='#3f3f3f', lwd=0.5)
axis(2, at=seq(5, 45, 10), labels=seq(1990, 1950, -10), col='#3f3f3f', col.axis='#3f3f3f', lwd=0, line=-0.2, las=1)

dev.off()


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



