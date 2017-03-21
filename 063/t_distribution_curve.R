df <- 6

alpha_significance<-0.05

crit.t <- round(qt(1-alpha_significance/2, df),2)

x <- rt(100000,df)

png(file='09_t_distrib.png', bg="transparent", width=1028, height=768)
coord.x2 <- c(crit.t, seq(crit.t,6,0.01), 3)
coord.y2 <- c(0,dt(seq(crit.t,6,0.01),df),0)
coord.x1 <- c(-3, seq(-6,-crit.t,0.01), -crit.t)
coord.y1 <- c(0,dt(seq(-6,-crit.t,0.01),df),0)
plot((density(x, adjust=2)),type="n",ylim=c(0,0.4),xlim=c(-6,6), bty='l', las = 1,
     main="", ylab="Sample Probability", xlab="t score")
polygon(coord.x2,coord.y2, col='gray', border=NA)
polygon(coord.x1,coord.y1, col='gray', border=NA)
lines(density(x, adjust=2))
#axis(2,0.05, las=1)

segments(crit.t,0, crit.t,0.034, lty=1, col='grey60')
segments(-crit.t,0, -crit.t,0.034, lty=1, col='grey60')
text(crit.t,0.05,bquote(t==.(crit.t)*',' ~ alpha==0.05), pos=4)
text(-crit.t,0.05,bquote(t==.(-crit.t)), pos=2)

dev.off()

#segments (-6, 0.025, -crit.t, 0.025, lty=2)


assay <- "Some assay"
xlab <- bquote(.(assay) ~ AC50 ~ (mu*M))
plot(0, xlab=xlab)


