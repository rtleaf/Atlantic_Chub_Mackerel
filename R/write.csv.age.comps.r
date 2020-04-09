# Import 
ACM.dat <- readxl::read_excel(path = "./data/Condensed_Chub_Mackerel_Data.xlsx") # Import data from excel file
ACM.dat 
names(ACM.dat)

ACM.dat <- ACM.dat %>% select(c(Capture_Year, Capture_Month, 'FL (mm)', 'TL (mm)', 'Weight (g)', Age, 
                                Maturity, Sex, "Gonad_Weight (g)"))
names(ACM.dat)[c(3,4,5)] <- c("FL", "TL", "Weight")
names(ACM.dat)[6] <- "Age"

summary(ACM.dat$Capture_Year)   # Note that we don't have all years of data
summary(ACM.dat$Capture_Month) 
summary(ACM.dat$FL)
summary(ACM.dat$TL)

ACM.dat$TL <- ACM.dat$TL/10
ACM.dat$FL <- ACM.dat$FL/10

# convert TL to FL
FL.2.TL <- coef(lm(TL ~ FL, data = ACM.dat))
ACM.dat <- ACM.dat %>% mutate(TL = ifelse(is.na(TL), FL*FL.2.TL[2] + FL.2.TL[1], TL))
TL.2.FL <- coef(lm(FL ~ TL, data = ACM.dat))
ACM.dat <- ACM.dat %>% mutate(FL = ifelse(is.na(FL), TL*TL.2.FL[2] + TL.2.FL[1], FL))

rm(FL.2.TL, TL.2.FL)

source("./R/age.at.L.VBGF.3P.r")

ACM.dat <- ACM.dat %>% mutate(est.age = ifelse(!is.na(Age), Age, age.at.L.VBGF.3P(Length.in = FL)))
ACM.dat <- ACM.dat %>% mutate(est.age = ifelse(est.age < 0, 0, est.age))
ACM.dat <- ACM.dat[complete.cases(ACM.dat$est.age),]
ACM.dat <- ACM.dat %>% dplyr::filter(est.age <= 6)

write.csv(x = table(ACM.dat$Capture_Year, round(ACM.dat$est.age)),
          file = ".annual.age.comps.csv")
