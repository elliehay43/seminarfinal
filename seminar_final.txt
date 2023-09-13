library(dplyr)
library(lmtest)

Path <- choose.files()
Seminar_final <- read.csv(path)

Ndf <- seminar_final %>%
  Select(ID, branch, sub_branch, year, ROA, ROE, RevenueGrowthRate, ProfitMargin, AssetsTurnover, GrossProfitMargin, CurrentAssetsRate, CurrentLiabilitiesRate)
Mutate(
  Year=as.numeric(year)
  Branch=as.factor(branch)
  ROA=as.numeric(gsub(',', '', ROA)),
  ROE=as.numeric(gsub(',', '', ROE)),
  RevenueGrowthRate=as.numeric(gsub(',', '', RevenueGrowthRate)),
  ProfitMargin=as.numeric(gsub(',', '', ProfitMargin)),
  AssetsTurnover=as.numeric(gsub(',', '', AssetsTurnover)),
  GrossProfitMargin=as.numeric(gsub(',', '', GrossProfitMargin)),
  CurrentAssetsRate=as.numeric(gsub(',', '', CurrentAssetsRate)),
  CurrentLiabilitiesRate=as.numeric(gsub(',', '', CurrentLiabilitiesRate))
)

# Add the Treatment column based on the year
Ndf$Treatment <- ifelse (Ndf$year <= year, 0, 1)

# 1 ROA
check_ROA <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_ROA = mean(ROA), .groups = 'drop')
check_ROA$year <- as.factor(Ndf$year)

model_ROA <- lm(avg_ROA ~ ID + branch + Treatment + branch:Treatment)
Summary(model_ROA)

# 2 ROE
check_ROE <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_ROE = mean(ROE), .groups = 'drop')
check_ROE$year <- as.factor(Ndf$year)

model_ROE <- lm(avg_ROE ~ ID + branch + Treatment + branch:Treatment)
Summary(model_ROE)

# 3 RevenueGrowthRate
check_RevenueGrowthRate <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_ROE = mean(RevenueGrowthRate), .groups = 'drop')

model_RevenueGrowthRate <- lm(avg_RevenueGrowthRate ~ ID + branch + Treatment + branch:Treatment)
Summary(model_RevenueGrowthRate)

# 3.1 RevenueGrowthRate - by year
check_RevenueGrowthRate$year <- as.factor(Ndf$year)
mR <- lm(avg_RevenueGrowthRate ~ ID + branch + year + branch:year)
summary(mR)

# 4 ProfitMargin
check_ProfitMargin <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_ProfitMargin = mean(ProfitMargin), .groups = 'drop')

model_ProfitMargin <- lm(avg_ProfitMargin ~ ID + branch + Treatment + branch:Treatment)
Summary(model_ProfitMargin)

# 5 OperatingMargin
check_OperatingMargin <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_OperatingMargin = mean(OperatingMargin), .groups = 'drop')

model_OperatingMargin <- lm(avg_OperatingMargin ~ ID + branch + Treatment + branch:Treatment)
Summary(model_OperatingMargin)

# 6 AssetsTurnover
check_AssetsTurnover <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_OperatingMargin = mean(OperatingMargin), .groups = 'drop')

model_OperatingMargin <- lm(avg_OperatingMargin ~ ID + branch + Treatment + branch:Treatment)
Summary(model_OperatingMargin)

# 7 GrossProfitMargin
check_GrossProfitMargin <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_GrossProfitMargin = mean(GrossProfitMargin), .groups = 'drop')

model_GrossProfitMargin <- lm(avg_GrossProfitMargin ~ ID + branch + Treatment + branch:Treatment)
Summary(model_GrossProfitMargin)

# 8 CurrentAssetsRate
check_CurrentAssetsRate <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_CurrentAssetsRate = mean(CurrentAssetsRate), .groups = 'drop')

model_CurrentAssetsRate <- lm(avg_CurrentAssetsRate ~ ID + branch + Treatment + branch:Treatment)
Summary(model_CurrentAssetsRate)

# 9 CurrentLiabilitiesRate
check_CurrentLiabilitiesRate <- Ndf %>%
  group_by(ID, branch, Treatment, year) %>%
  Summarise(avg_CurrentLiabilitiesRate = mean(CurrentLiabilitiesRate), .groups = 'drop')

model_CurrentLiabilitiesRate <- lm(avg_CurrentLiabilitiesRate ~ ID + branch + Treatment + branch:Treatment)
Summary(model_CurrentLiabilitiesRate)