#Taylor Daley
#12/5/2017
#######Weight-at-length

#read.csv(file = "sample_data_combined_TL.csv")
#read.csv(file = "Combined_all_samples_QAQC.csv")
#life.history.dat <- Combined_all_samples_QAQC
#life.history.dat <- read.csv("C:\\Users\\w981371\\Dropbox\\chub mackerel\\Data\\Samples collected\\Combined_all_samples_QAQC.csv")

life.history.dat <- read.csv("~/Dropbox/chub mackerel/Data/Samples collected/Combined_all_samples_QAQC.csv")
##################clean up data
life.history.dat$w <- life.history.dat$Wet.weight..g.
life.history.dat$L <- life.history.dat$TL..cm.
#life.history.dat$L <- life.history.dat$`TL (cm)`
#life.history.dat$w <- life.history.dat$`Wet weight (g)`
remove.ind <- 
  c(which(life.history.dat$L < 25 & life.history.dat$w > 150),
    which(life.history.dat$L > 30 & life.history.dat$w < 200),
   which(life.history.dat$L > 35 & life.history.dat$w < 300),
   which(life.history.dat$L > 40 & life.history.dat$w < 500),
   which(life.history.dat$L==0),
  which(is.na(life.history.dat$w)))


life.history.dat <- life.history.dat[-remove.ind,]
length(life.history.dat $Month)
summary(life.history.dat)

data.val <- list(n = dim(life.history.dat)[1], 
                 W = life.history.dat$w,
                 L = life.history.dat$L)

#create matrix of parameter estimates from previos studies
#a.d <- c(0.002,0.0066,0.0009,0.0021,0.0130,0.0035)
#b.d  <- c(3.44,3.14,3.7,3.41,2.97,3.23)
#length(b.d)
#par.est <- matrix(c(a.d, b.d), nrow=length(a.d))
#################################
#use funtion to determine which distribution to use
#put in matrix of mean parameter estimates from previous studies
#this does the same thing as moment matching
#cont.dist.param.id(par.est[,1])
plot(density(rlnorm(1000, meanlog= 1, sdlog= sqrt(1/1.6))))
a.prev <- c(0.00231,0.002,0.0020,0.0021,0.0035,0.003,0.00278,0.0066,0.0009,0.0130)
a.prev.mean <- mean(a.prev)
a.prev.mean
b.prev <- c(3.38,3.46,3.44,3.41,3.23,3.31,3.33,3.14,3.70,2.97)
b.prev.mean <- mean(b.prev)
b.prev.mean
#####################################bayesian model
require(coda)
require(rjags)
#inits.val <- list(log.a = log(0.01), log.b = log(2.6), sigma = 1)
######jags
sink("length.at.weight")
cat("model {
    
      # Priors
    a ~ dnorm(0.0038, 1/(100 ^ 2))
    b ~ dnorm(3.34, 1/(100 ^ 2))
    sigma ~ dunif(0, 100) #sigma is SD
    tau.C <- 1/ (sigma ^ 2)  #tau is variance

# Likelihood 
    for (j in 1:n) {
    W[j] ~ dnorm(eta[j], tau.C)
    eta[j] <- a*(L[j]^b)
E.sim[j] ~ dnorm(eta[j], tau.C)
    } 

#posterior predictive distribution
mean.E <- mean(W[])
    mean.E.new <- mean(E.sim[])
    p.mean <- step(mean.E.new-mean.E)
    
    sd.data <- sd(W[])
    sd.sim <- sd(E.sim[])
    p.sd <- step(sd.sim-sd.data)
    
    for(j in 1:n) {
    sq[j] <- (W[j]-eta[j])*(W[j]-eta[j])
    sq.new[j] <- (E.sim[j] - eta[j])*(E.sim[j] - eta[j])
    }
    
    fit <-sum(sq[])
    fit.new <- sum(sq.new[])
    pvalue.fit <- step(fit.new - fit)
    
    }",fill=TRUE)
sink()
#a and b can not be negative so do lognormal
###############################################
#####################################
j.mod <- jags.model(file = "length.at.weight", data =data.val, n.chains=3, n.adapt = 200000)
update(j.mod, burnin=50000)
vals <- coda.samples(j.mod, variable.names = c("a","b", "sigma", "tau.C",  "fit", "fit.new", "pvalue.fit","p.mean","p.sd"), n.iter = 1000000, thin = 3000)
vals.g <- as.data.frame(as.matrix(vals))
#gives distribution of posteriors
plot(vals[,c(1,2,9)])
head(vals)
vals.g[3]
vals.g[4]
autocorr.plot(vals[,c(1,2,8)])
##############model checks
mean(vals.g$fit.new > vals.g$fit) #calculate p-value
sd(vals.g$fit.new > vals.g$fit)
plot(vals.g$fit, col="blue", ylim= c(0,140000000))
points(vals.g$fit.new, col="red")

plot(vals.g$fit.new, vals.g$fit, xlim=c(0,1400000))
abline(0,1, col = "red",xlim=c(0,1400000))

gelman.plot(vals[,c(1,2,8)])
gelman.diag(vals[,c(1,2,8)])
a.est <- median(vals.g[,1])
b.est <- median(vals.g[,2])

a.vec <- (vals.g[,1])
b.vec <- (vals.g[,2])

quantile(a.vec, probs = c(0.025, 0.975))  
quantile(b.vec ,probs = c(0.025, 0.975))
##########plot##############

a.est <- 0.0258
b.est <-  2.72
#png(file= "weight at length.png", 
    #height= 8, pointsize=18,  width=9 , res=600, units = "in", family = "serif")
#par(mar=c(4 , 4, 1, 1), las=1)
setEPS()

postscript("Figure 4 weight at length.eps")


plot(x =life.history.dat$L, y = life.history.dat$w, 
     xlim = c(20, 40),
     ylim = c(0,600), xlab= "TL (cm)", ylab = "WT (g)", col ="grey30")
curve(a.est * (x^b.est), from = min(life.history.dat$L),
      to = max(life.history.dat$L),lty=1, col="black", add=T, lwd =3)
dev.off()
#curve(a.est.1 * (x^b.est.1), from = min(life.history.dat$L),
      #to =max(life.history.dat$L), lty=3, col="blue", add=T, lwd =3)
#legend("bottomright", c("Bayesian","nls"),
       #lty=c(3,3), col=c("orange","blue"), lwd= c(3,3))
##################transparent plot
# png(file= "transparent length weight Grand Plots.png", 
#     height= 6, pointsize=18,  width=9 , res=300, units = "in", family = "serif")
# par(mar=c(4 , 4, 1, 1), fg="white", bg = NA, col.axis = "white", col.lab ="white")
# plot(Wet.weight..g.  ~ L, data = life.history.dat, xlim = c(17,46), 
#      ylim = c(0,600), xlab= "TL (cm)", ylab = "WT (g)", col ="grey")
# curve(a.est * (x^b.est), from = min(life.history.dat$L), 
#       to =max(life.history.dat$L), add=T, lwd =3)
# dev.off()
# 
# ###############################CI#################################
# # Plot credible intervals for predicted weight-at-length
# # Get MCMC vectors for each parameter
# a.mcmc <- (vals.g[,1])
# b.mcmc <- (vals.g[,2])
# # Create a vector of possible lengths
# #lengths.vec <- c(22:40) Range of my data
# lengths.vec <- c(0:40) #should I show predictions starting at 0?
# # Predict weight at length
# weight.at.length.df <- data.frame(matrix(NA, nrow = length(a.mcmc), ncol = length(lengths.vec)))
# colnames(weight.at.length.df) <- paste("weight-at-length-", lengths.vec, sep = "")
# head(weight.at.length.df)
# 
# for (i in 1:length(lengths.vec)){
#   pred.weight.temp <- a.mcmc * (lengths.vec[i]^b.mcmc)
#   weight.at.length.df[,i] <- pred.weight.temp
# }
# dim(weight.at.length.df)
# 
# # Credible intervals of mean predicted weight-at-length 
# weigth_at_length_quantiles <- data.frame(matrix(NA, nrow = length(lengths.vec), ncol = 4))
# colnames(weigth_at_length_quantiles) <- c("Mean", "Median", "2.5% Quantile", "97.5% Quanitle")
# rownames(weigth_at_length_quantiles) <- lengths.vec
# for(i in 1:length(lengths.vec)){
#   length.subsample = lengths.vec[i]
#   print(paste("Calculating 95% credible interval of weight-at-length-", length.subsample,sep=""))
#   dat <- weight.at.length.df[,i]
#   weigth_at_length_quantiles[i,c(3,4)] <- quantile(dat, probs = c(0.025, 0.975))
#   weigth_at_length_quantiles[i,1] <- mean(dat)
#   weigth_at_length_quantiles[i,2] <- median(dat)
# }
# weigth_at_length_quantiles
# 
# # Plot data
# png(file= "predicted weight at length model comparison1.png", 
#     height= 6, pointsize=18,  width=9 , res=300, units = "in", family = "serif")
# par(mar=c(4 , 4, 1, 1))
# plot(x = (life.history.dat$L), y = (life.history.dat$w),
#      xlim=c(17,46), pch =1, cex = .75, col = "grey30", ylim=c(0,700), 
#      ylab = "Weight (g)", xlab = "TL (cm)")
# curve(a.est * (x^b.est), from = min(life.history.dat$L),
#       to = max(life.history.dat$L),lty=2, add=T, lwd =3)
# 
# # Plot the 95% credible interal of mean predicted weigtht at length
# for (i in 1:length(lengths.vec)){
#   length.subsample = lengths.vec[i]
#   print(paste("Taking 95% credible interval of weight-at-length-", length.subsample,sep=""))
#   dat <- weight.at.length.df[,i]
#   quant.pred.weight <- quantile(dat, probs = c(0.025, 0.975))
#   segments(x0 = length.subsample, y0 = quant.pred.weight[1], x1 = length.subsample, y1 = quant.pred.weight[2], col = "red", lwd = 3)
# }
# dev.off()
# ###############################for other studies###############################
# a.vec <- c(0.0052, 0.00231, 0.0015)
# b.vec <- c(3.22, 3.4, 3.51)
# 
# mymat <- matrix(nrow=length(a.vec), c(a.vec, b.vec))
# 
# #loop to include all studies
# 
# for(j in 1:length(mymat[,1])){ #a
#   for(k in 1:length(mymat[,2])){ #b
#     
#     # Predict weight at length
#     weight.at.length.df <- data.frame(matrix(NA, nrow = length(mymat[,j]), ncol = length(lengths.vec)))
#     colnames(weight.at.length.df) <- paste("weight-at-length-", lengths.vec, sep = "")
#    
#     for (i in 1:length(lengths.vec)){
#       pred.weight.temp <- mymat[,1]* (lengths.vec[i]^mymat[,2])
#       weight.at.length.df[,i] <- pred.weight.temp
#     }
#   
#         # Credible intervals of mean predicted weight-at-length 
#     weigth_at_length_quantiles <- data.frame(matrix(NA, nrow = length(lengths.vec), ncol = 12))
#     colnames(weigth_at_length_quantiles) <- c("Mean", "Median", "2.5% Quantile", "97.5% Quanitle","Mean", "Median", "2.5% Quantile", "97.5% Quanitle","Mean", "Median", "2.5% Quantile", "97.5% Quanitle")
#     rownames(weigth_at_length_quantiles) <- lengths.vec
#     for(i in 1:length(lengths.vec)){
#       length.subsample = lengths.vec[i]
#       print(paste("Calculating 95% credible interval of weight-at-length-", length.subsample,sep=""))
#       dat <- weight.at.length.df[,i]
#       for(l in 1:length(dat)){###########
#         weigth_at_length_quantiles[i,c((3+4*(l-1)),(4+4*(l-1)), (2+4*(l-1)))] <- quantile(dat[l], probs = c(0.025, 0.975, 0.5))
#         weigth_at_length_quantiles[i,(1+4*(l-1))] <- mean(dat[l])
#       } 
#       # weigth_at_length_quantiles[i,c(3,4)] <- quantile(dat[1], probs = c(0.025, 0.975))
#       # weigth_at_length_quantiles[i,1] <- mean(dat[1])
#       # weigth_at_length_quantiles[i,2] <- median(dat[1])
#       # 
#       # weigth_at_length_quantiles[i,c(7,8)] <- quantile(dat[2], probs = c(0.025, 0.975))
#       # weigth_at_length_quantiles[i,5] <- mean(dat[2])
#       # weigth_at_length_quantiles[i,6] <- median(dat[2])
#       # 
#       # weigth_at_length_quantiles[i,c(11,12)] <- quantile(dat[3], probs = c(0.025, 0.975))
#       # weigth_at_length_quantiles[i,9] <- mean(dat[3])
#       # weigth_at_length_quantiles[i,10] <- median(dat[3])
#     }
#     weigth_at_length_quantiles
#   # Plot the 95% credible interal of mean predicted weigtht at length
#     for (i in 1:length(lengths.vec)){
#       length.subsample = lengths.vec[i]
#       print(paste("Taking 95% credible interval of weight-at-length-", length.subsample,sep=""))
#       dat <- weight.at.length.df[,i]
#       quant.pred.weight.1 <- quantile(dat[1], probs = c(0.025, 0.975))
#       quant.pred.weight.2 <- quantile(dat[2], probs = c(0.025, 0.975))
#       quant.pred.weight.3 <- quantile(dat[3], probs = c(0.025, 0.975))
#      
#       segments(x0 = length.subsample, y0 = quant.pred.weight.1[1], x1 = length.subsample, y1 = quant.pred.weight.1[2], col = "green", lwd = 6)
#       segments(x0 = length.subsample, y0 = quant.pred.weight.2[1], x1 = length.subsample, y1 = quant.pred.weight.2[2], col = "blue", lwd = 6)
#       segments(x0 = length.subsample, y0 = quant.pred.weight.3[1], x1 = length.subsample, y1 = quant.pred.weight.3[2], col = "orange", lwd = 6)
#     }
#     
#   }
# }  
# 
# dev.off()
