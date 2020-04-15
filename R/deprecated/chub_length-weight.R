
life.history.dat <- read.csv(file = "sample_data_combined_TL.csv")
life.history.dat$w <- life.history.dat$Wet.weight..g.
life.history.dat$L <- life.history.dat$TL.cm.

#3 weight length 7
remove.ind <- 
  c(which(life.history.dat$L < 25 & life.history.dat$w > 150),
    which(life.history.dat$L > 30 & life.history.dat$w < 200),
   which(life.history.dat$L > 35 & life.history.dat$w < 300),
   which(life.history.dat$L > 40 & life.history.dat$w < 500))

summary(life.history.dat)
length(life.history.dat$Wet.weight..g.)

life.history.dat <- life.history.dat[-remove.ind,]

plot(w  ~ L, data = life.history.dat, xlim = c(20,40), ylim = c(0,600), 
     xlab= "TL (cm)", ylab = "WT (g)", col ="grey30")
#execute model fitting
w.TLfit <- nls(Wet.weight..g. ~ a.est * (L^b.est),
                start = c(a.est = 0.001, b.est = 3),
                data = life.history.dat)
confint(w.TLfit)
w.TLsumm <- summary(w.TLfit)
w.TLsumm
a.est <- w.TLsumm$coefficients[1,1]
b.est <- w.TLsumm$coefficients[2,1]
##########plot##############
png(file= "length weight Grand Plots.png", 
    height= 6, pointsize=18,  width=9 , res=300, units = "in", family = "serif")
par(mar=c(4.5 , 4.5, 1, 1))
plot(Wet.weight..g.  ~ L, data = life.history.dat, xlim = c(min(L),max(L)), 
     ylim = c(0,600), xlab= "TL (cm)", ylab = "WT (g)", col ="grey30")
curve(a.est * (x^b.est), from = min(life.history.dat$L), 
      to =max(life.history.dat$L), add=T, lwd =3)
dev.off()
###################transparent plot 
png(file= "transparent length weight Grand Plots.png", 
    height= 6, pointsize=18,  width=9 , res=300, units = "in", family = "serif")
par(mar=c(4 , 4, 1, 1), fg="white", bg = NA, col.axis = "white", col.lab ="white")
plot(Wet.weight..g.  ~ L, data = life.history.dat, xlim = c(17,46), 
     ylim = c(0,600), xlab= "TL (cm)", ylab = "WT (g)", col ="grey")
curve(a.est * (x^b.est), from = min(life.history.dat$L), 
      to =max(life.history.dat$L), add=T, lwd =3)
dev.off()
