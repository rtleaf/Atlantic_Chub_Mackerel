life.history.dat <- read.csv(file= "Ageing_all_QAQC.csv")
#in Ageing folder
life.history.dat <-Ageing_all_QAQC
#############################clean up data
life.history.dat$L <- life.history.dat$`TL (cm)`
#remove zeros from data set
remove.ind <- 
  c(which(is.na(life.history.dat$L)),
    which(life.history.dat$L==0))

life.history.dat <- life.history.dat[-remove.ind,]
dim(life.history.dat)

L = life.history.dat$L
Age = life.history.dat$Age
Month = life.history.dat$Month
################convert age to julian
yearmon(Age)
life.history.dat$adjusted.age <- life.history.dat$Age + ((life.history.dat$Month-1)/12)
plot(life.history.dat$adjusted.age, life.history.dat$L)
#######################################################
model.fit <- nls(L ~ linf * (1-exp(-k*(adjusted.age-t0))),
                   start = c(linf = 50, k = 0.2, t0= -1.3),
                   data = life.history.dat)

model.fit.summ <- summary(model.fit)
model.fit.summ

linf.est <- model.fit.summ$coefficients[1,1]
k.est <- model.fit.summ$coefficients[2,1]
t0.est <- model.fit.summ$coefficients[3,1]


####################################3-p
model.fit.2 <- nls(L ~ linf * (1-exp(-k*(Age-t0))),
                   start = c(linf = 50, k = 0.2, t0= -1.3),
                   data = life.history.dat)

model.fit.summ.2 <- summary(model.fit.2)
model.fit.summ.2

linf.est.2 <- model.fit.summ.2$coefficients[1,1]
k.est.2 <- model.fit.summ.2$coefficients[2,1]
t0.est.2 <- model.fit.summ.2$coefficients[3,1]

##################plot
plot(x = (life.history.dat$Age), y = (life.history.dat$L), axes=FALSE,
     xlim=c(0,8), pch =16, cex = .75, col = "grey30", ylim=c(0,40),
     ylab = "TL (cm)", xlab = "Age (years)")
axis(1, at=seq(0,8, by=1), las=1)
axis(2, at=seq(0,40, by=5), las=1)
lines(x = (life.history.dat$adjusted.age), y =(life.history.dat$L), col="blue", type="p")
curve(linf.est * (1-exp(-k.est*(x-(t0.est)))),
      from = 0, to = 10,
      lwd= 3, add = T, cex =1.5, col="blue", lty=3)
curve(linf.est.2 * (1-exp(-k.est.2*(x-(t0.est.2)))),
     from = 0, to = 10,
      lwd= 3, add = T, cex =1.5)


AIC(model.fit.2)
confint(model.fit.2)

AIC(model.fit)
confint(model.fit)