df <- 3

alpha_significance<-0.05

# crit.chi <- round(qchisq(1-alpha_significance/2, df),2)

crit.chi <- round(qchisq(1-alpha_significance, df), 2)


x <- rchisq(100000,df)

png(file='13_chi_distrib.png', bg="transparent", width=1000, height=607)

plot_x_upper_lim <- 12

op <- par(family='serif')

coord.x2 <- c(crit.chi, seq(crit.chi,plot_x_upper_lim,0.01), plot_x_upper_lim)
coord.y2 <- c(0,dchisq(seq(crit.chi,plot_x_upper_lim,0.01),df),0)
# coord.x1 <- c(-3, seq(-1,-crit.chi,0.01), -crit.chi)
# coord.y1 <- c(0,dchisq(seq(-1,-crit.chi,0.01),df),0)
plot((density(x, adjust=2)),type="n",ylim=c(0,0.25), xlim=c(0,plot_x_upper_lim), bty='l', las = 1,
     main="", ylab="Sample Probability", xlab=bquote(chi^2 ~ "value"))
polygon(coord.x2,coord.y2, col='gray', border=NA)
# polygon(coord.x1,coord.y1, col='gray', border=NA)

segments(crit.chi,0, crit.chi,0.022, lty=1, col='grey60')
text(crit.chi,0.05,bquote(chi^2==.(crit.chi)*',' ~ alpha==0.05), pos=4)

# segments(-crit.chi,0, -crit.chi,0.024, lty=1, col='grey60')
# text(-crit.chi,0.05,bquote(chi==.(-crit.chi)), pos=2)

lines(density(x, adjust=2), lwd=2)


par(op)

dev.off()

