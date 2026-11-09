finaldata <- readRDS("df_clean.rds")


lmmod <- lm(maternal_mortality ~ -1 + as.numeric(armed_conflict) + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso, 
            data = finaldata)

library(plm)
plmmod <- plm(maternal_mortality ~ as.numeric(armed_conflict) + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within", data = finaldata)


summary(plmmod)
summary(lmmod)

lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso + as.factor(year), #if year is saved as numeric, as.factor(x) 
            data = finaldata)

summary(lmmod)

summary(df_clean)
class(df_clean$armed_conflict)
