setwd('bi163')
bigN <- rnorm(100000, mean=66.5, sd=4.43)
## Normal distribution

#quartzFonts(Linux=c("Linux Libertine O Regular", "Linux Libertine O Bold", "Linux Libertine O Italic", "Linux Libertine O Bold Italic"))

#op <- par(family="serif")
png(file="normal_curve.png", bg="transparent", width=1028, height=768)
plot(density(bigN, adjust=2), col='gray25', bty='l', main="", xlab="Height (inches)", ylab='Sample Probability', las=1, lwd=2) 
abline(v=66.5, lty=1)
dev.off()
#par(op)

png(file="normal_curve_four_means.png", bg="transparent", width=1028, height=768)
plot(density(bigN, adjust=2), col='gray25', bty='l', main="", xlab="Height (inches)", ylab='Sample Probability', las=1, lwd=2) 
abline(v=66.5, lty=1)
for(i in 1:4){
  x <- mean((sample(bigN, 15)))
  abline(v=x, lty=3)
}
dev.off()
