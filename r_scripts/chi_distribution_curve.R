df <- 3

alpha_significance<-0.05

crit.chi <- round(qchisq(1-alpha_significance, df), 2)

x <- rchisq(100000,df)

plot_x_upper_lim <- 12

png(file='13_chi_distrib.png', bg="transparent", width=1000, height=607)

op <- par(family='serif', mar=c(5,6,1,1))


coord.x2 <- c(crit.chi, seq(crit.chi,plot_x_upper_lim,0.01), plot_x_upper_lim)
coord.y2 <- c(0,dchisq(seq(crit.chi,plot_x_upper_lim,0.01),df),0)
plot((density(x, adjust=2)),type="n",ylim=c(0,0.25), xlim=c(0,plot_x_upper_lim), bty='l', las = 1,
     main="", ylab="", xlab=bquote(chi^2 ~ "value"), yaxs = "i", cex.lab=1.3, cex.axis=1.2)
polygon(coord.x2,coord.y2, col='gray', border=NA)

segments(crit.chi,0, crit.chi,0.022, lty=1, col='grey60')
curve(dchisq(x, df), add=TRUE)

text(crit.chi,0.1,bquote(chi^2==.(crit.chi)*',' ~ alpha==0.05), pos=4, cex=1.3)

text(1,0.07, "Does not differ \nfrom expected values.", pos=4, cex=1.2)
text(9, 0.04, "Differs significantly \nfrom expected values.", pos=4, cex=1.2)
mtext("Sample Probability", side=2, line=4, cex=1.3)
arrows(8.6, 0.0075, 9,0.04, code=1, length=0.1)

par(op)

dev.off()



