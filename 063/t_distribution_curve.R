df <- 6

alpha<-0.05

crit.t <- round(qt(1-alpha/2, df),2)

x <- rt(100000,df)

coord.x2 <- c(crit.t, seq(crit.t,6,0.01), 3)
coord.y2 <- c(0,dt(seq(crit.t,6,0.01),df),0)
coord.x1 <- c(-3, seq(-6,-crit.t,0.01), -crit.t)
coord.y1 <- c(0,dt(seq(-6,-crit.t,0.01),df),0)
plot((density(x, adjust=2)),ylim=c(0,0.4),xlim=c(-6,6), bty='l', las = 1,
     main="", ylab="Sample Probability", xlab="t score")
polygon(coord.x2,coord.y2, col='gray', border=NA)
polygon(coord.x1,coord.y1, col='gray', border=NA)
#axis(2,0.05, las=1)

segments(crit.t,0, crit.t,0.035, lty=1)
segments(-crit.t,0, -crit.t,0.035, lty=1)
text(crit.t,0.05,paste('t = ',crit.t,', p= 0.05', sep=""), pos=4)
text(-crit.t,0.05,paste('t = ',-crit.t), pos=2)

#segments (-6, 0.025, -crit.t, 0.025, lty=2)

