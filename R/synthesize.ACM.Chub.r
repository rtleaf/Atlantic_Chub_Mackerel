# Chub Mackerel Length Composition

# Game plan
# Import - get data from excel sheet
# Tidy - examine data and do some QA/QC
# Model
# Visualize - use ggplot to get:
#  1. year specific histograms - length composition, age composition
#  2. Tables - summary statistics, monthly, annual range in age, length, number

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

# convert TL to FL
FL.2.TL <- coef(lm(TL ~ FL, data = ACM.dat))
ACM.dat <- ACM.dat %>% mutate(TL = ifelse(is.na(TL), FL*FL.2.TL[2] + FL.2.TL[1], TL))
TL.2.FL <- coef(lm(FL ~ TL, data = ACM.dat))
ACM.dat <- ACM.dat %>% mutate(FL = ifelse(is.na(FL), TL*TL.2.FL[2] + TL.2.FL[1], FL))

ACM.dat <- ACM.dat %>% dplyr::filter(TL <= 400, FL >= 190 , !is.na(Capture_Year))
ACM.dat.grand <- data.frame(FL = rep(ACM.dat$FL, length(unique(ACM.dat$Capture_Year))),
                            Year = sort(rep(unique(ACM.dat$Capture_Year), length(ACM.dat$FL))))

ggplot() + 
  geom_density(data = ACM.dat.grand, aes(x = FL), alpha=0.75, fill="orangered2", bw = 5) +
  geom_histogram(data = ACM.dat, aes(y=..density.., x = FL), 
                 colour="black", fill="white", binwidth = 10) + 
  geom_density(data = ACM.dat, aes(x = FL), alpha=0.25, fill="lightgray") +
  facet_wrap(Capture_Year ~ .) + 
  labs(x = "Fork Length (mm)", y = "Density") +
  theme_minimal(base_size = 10) + 
  ggsave(filename = "./figs/annual.histograms.png", device = "png")


