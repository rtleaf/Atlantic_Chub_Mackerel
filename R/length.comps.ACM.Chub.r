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

ACM.dat$TL <- ACM.dat$TL/10
ACM.dat$FL <- ACM.dat$FL/10

# convert TL to FL
FL.2.TL <- coef(lm(TL ~ FL, data = ACM.dat))
ACM.dat <- ACM.dat %>% mutate(TL = ifelse(is.na(TL), FL*FL.2.TL[2] + FL.2.TL[1], TL))
TL.2.FL <- coef(lm(FL ~ TL, data = ACM.dat))
ACM.dat <- ACM.dat %>% mutate(FL = ifelse(is.na(FL), TL*TL.2.FL[2] + TL.2.FL[1], FL))

ACM.dat <- ACM.dat %>% dplyr::filter(TL <= 40, TL >= 10, !is.na(Capture_Year))

year.ind <- sort(unique(ACM.dat$Capture_Year))
ACM.dat.grand <- data.frame(FL = NA, Year = NA)
# ACM.dat %>% group_by(Capture_Year) %>% summarize(n. = length(FL)) %>% arrange(n.)
for (j in 1:length(year.ind)) {
  y.dat <- data.frame(FL = ACM.dat$FL[sample(which(ACM.dat$Capture_Year == year.ind[j]), size = 50)],                 Year = year.ind[j])
  ACM.dat.grand <- rbind(ACM.dat.grand, y.dat)
}
ACM.dat.grand <- ACM.dat.grand[complete.cases(ACM.dat.grand),]

ggplot() + 
  geom_density(data = ACM.dat.grand, aes(x = FL), alpha=0.75, fill="orangered2", bw = 1) +
  geom_histogram(data = ACM.dat, aes(y=..density.., x = FL), 
                 colour="black", fill="white", binwidth = 1) + 
  geom_density(data = ACM.dat, aes(x = FL), alpha=0.25, fill="lightgray") +
  facet_wrap(Capture_Year ~ .) + 
  labs(x = "Fork Length (cm)", y = "Density") +
  theme_minimal(base_size = 10) + 
  ggsave(filename = "./figs/length.comps.panel.png", device = "png")
