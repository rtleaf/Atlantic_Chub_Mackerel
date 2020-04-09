

ch <- read.csv("C:\\Users\\w981371\\Desktop\\chub_2017_ sampling.csv")
ch$FL <- ch$TL

july <- subset(ch, month == 7)
head(july)
july <- july[which(july$FL != 0), ]
unique(july$FL)
length(july$FL)
#30
range(july$FL)
# range is 304 370
unique(july$FL)
july.freq <- as.data.frame(table(july$FL))
              
hist(july$FL, breaks = c(seq(from = 230, to = 400, by = 10)), main = "July", xlab= "TL (mm)")


june <- subset(ch, month == 6)
june <- june[which(aug$FL != 0), ]
length(june$FL)
#116
unique(june$FL)
range(june$FL)
#288 387
summary(june$FL)
june.freq <- as.data.frame(table(june$FL))
hist(june$FL, breaks = c(seq(from = 165, to = 400, by = 10)), main = "June", xlab= "TL (mm)")

sep <- subset(ch, month == 11)
length(sep$FL)
#21
unique(sep$FL)
range(sep$FL)
#316 381
sep.freq <- as.data.frame(table(sep$FL))
hist(sep$FL, breaks = c(seq(from = 00, to = 390, by = 10)), main = "September", xlab= "FL (mm)")



ch <- Chub_mackerel_sample_data_august
names(ch)
ch$FL <- ch$FL..mm.


head(ch)
ch <- ch[which(ch$FL != "0"), ]
unique(ch$FL)
length(ch$FL)
#846
range(ch$FL)
# range is 152 - 399
as.data.frame(table(ch$FL))
hist(ch$FL, breaks = c(seq(from = 140, to = 400, by = 10)), main = "August initial sampling")




#plot(runif(100,0,2))
#hist(runif(100,0,2))

