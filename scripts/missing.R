finaldata <- readRDS("data/processed/df_clean.rds")

summary(finaldata)
#the missing values 

library(here)
library(tidyverse)
library(knitr)
library(flextable)
library(texreg)
library(sjPlot)
library(bp)
library(plm)
library(mice)
library(naniar)

#STEP 1 - Set up the data for imputation 

finaldata <- finaldata |>
  mutate(loggdp = log(gdp_1000)) |>#create the log(gdp) variable 
  select(-gdp_1000) #take away original gdp variable 

#heat plot to look at missingness across the data frame 
finaldata |>
  arrange(year, iso) |>
  dplyr::select(-iso, -year) |>
  vis_miss()

#based on this plot we can see a lot of missing data froms from maternal mortality,
#some in the other mortalitis, plus a few of the covariates! 


#MULTIPLE IMPUTATION 
set.seed(100)

summary(finaldata)
#iso is a string variable - convert accordingly 

midata <- finaldata |>
  mutate(iso_num = as.numeric(as.factor(finaldata$iso))) |> #make sure iso is numeric
  select(-iso) #take away original iso factor column 

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method

print(meth)
#this is specifying the imputation method for each column of data 
#as you can see for variables that have missing data, pmm is used. 

meth[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"
#change them accordingly. So this is saying change the method of imputation for the linear mixed effects model 21.lmer
#syntax is meth["variable name(s)"] <- "new imputation method"
#use methods(mice) to see the method names + codes 


pred <- mice0$predictorMatrix
print(pred)
pred[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality"), "iso_num"] <- -2
#we change these to -2 to indicate multilevel imputation 

#--------------------------------------------------------------------------------
#1) perform with m=10 imputations and max iteration to 20
    #rerun multiple imputation


mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

#plot to check that the imputed datasets converged
plot(mice.multi.out) #each colour represnts one imputation data set 

#save the multiple imputation as an R object 
save(mice.multi.out, file = "outputs/mice_out.RData")

#STEP 2 - FIT THE REGRESSION MODELS (MATERNAL MORTALITY)
fit.mi.matmor <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

out.mi.matmor <- pool(fit.mi.matmor)
#apply Rubin's rules to summarize the regression estimates and standard errors

summary(out.mi.matmor)

#NEONATAL MORTALITY

fit.neonatal.mor <- with(mice.multi.out, 
                      lm(neonatal_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

out.neonatal.mor <- pool(fit.neonatal.mor)
summary(out.neonatal.mor)

#UNDER 5 MORTALITY 
fit.under5.mor <- with(mice.multi.out, 
                      lm(under5_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

out.under5.mor <- pool(fit.under5.mor)
summary(out.under5.mor)

#INFANT MORTALITY 
fit.infant.mor <- with(mice.multi.out, 
                      lm(infant_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

out.infant.mor <- pool(fit.infant.mor)
summary(out.infant.mor)


#fit the CCA regression results! 
library(plm)
preds <- as.formula(" ~ armed_conflict + loggdp + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

matmormod <- plm(update.formula(preds, maternal_mortality ~ .), index = c("iso", "year"), effect = "twoways",
                 model = "within", data = finaldata)
un5mormod <- plm(update.formula(preds, under5_mortality ~ .), index = c("iso", "year"), effect = "twoways",
                 model = "within", data = finaldata)
infmormod <- plm(update.formula(preds, infant_mortality ~ .), index = c("iso", "year"), effect = "twoways",
                 model = "within", data = finaldata)
neomormod <- plm(update.formula(preds, neonatal_mortality ~ .), index = c("iso", "year"), effect = "twoways",
                 model = "within", data = finaldata)

#STEP 4 --> CREATE COMPARISON TABLE 
library(texreg)
texreg::htmlreg(list(matmormod, out.mi.matmor, un5mormod, out.under5.mor, infmormod, out.infant.mor, neomormod, out.neonatal.mor),
        custom.model.names=c("Complete Case", 
                             "MI (m=10)",
                             "Complete Case",
                             "MI (m=10)",
                             "Complete Case",
                             "MI (m=10)", 
                             "Complete Case",
                             "MI (m=10)"),
  
        ci.force=TRUE,
        
        digits = 3,
  
        file="outputs/table3_mortalitymodelcomparison.html",
  
        omit.coef = "as.factor\\(year\\)|as.factor\\(iso_num\\)",
  
        custom.coef.names=c("Armed conflict", "LOG of GDP per capita", "OECD member", "Population Density", 
                    "Urban residence", "Age dependency ratio", "Male education", "Temperature", "Rainfall", "Earthquakes", "Droughts"),
  
        custom.header=list("Maternal Mortality" = 1:2, "Under 5 Mortality" = 3:4, "Infant Mortality" = 5:6, "Neonatal Mortality" = 7:8), 
  
        custom.note = paste(
                      "<sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01<br>",
                      "Data given as adjusted beta coefficient (95% confidence interval).",
                      "Robust standard errors were employed.",
                      "Each column is the output from 1 panel regression with fixed effects adjusted for the covariates in the table in addition to year dummies (not shown).",
                      "Coefficients are interpreted as the absolute change in the dependent variable following a 1-unit change of the independent variable.",
                      "GDP per capita is in current US dollars, and its unit is scaled up by 1,000.",
                      "Population density represents the percentage of the population living in a density of >1,000 people/km<sup>2</sup>.",
                      "Urban residence represents the percentage of the population living in urban areas.",
                      "The age dependency ratio represents the proportion of dependents (aged under 15 years or over 64 years) per 100 working-age individuals.",
                      "Male education is expressed as years per capita and is age-standardised.",
                      "Temperature is in degrees Celsius and is the mean population-weighted annual temperature.",
                      "Rainfall is the mean population-weighted annual rainfall in millimetres per year, scaled down by 1,000.",
                      "Earthquake and drought are binary variables representing their absence or presence.",
                      "All armed conflict variables were lagged by 1 year.<br>GDP, gross domestic product; OECD, Organisation for Economic Co-operation and Development."),
        single.row=TRUE,
  )

#CODEX BEAUTIFIED VERSION - TO GET RID OF GOF, FIX FORMATTING, ETC


## The model objects below are expected to be created by the analysis script
## before this section is run.
models <- list(
  matmormod, out.mi.matmor,
  un5mormod, out.under5.mor,
  infmormod, out.infant.mor,
  neomormod, out.neonatal.mor
)

model_names <- c(
  "Complete Case", "MI (m=10)",
  "Complete Case", "MI (m=10)",
  "Complete Case", "MI (m=10)",
  "Complete Case", "MI (m=10)"
)

## Extract first, then clear every extracted GOF statistic. This also removes
## MI-specific rows (such as nimp/nobs) that the include.* switches may miss.
texreg_models <- lapply(models, texreg::extract)
texreg_models <- lapply(texreg_models, function(model) {
  model@gof <- numeric(0)
  model@gof.names <- character(0)
  model@gof.decimal <- logical(0)
  model
})

table_note <- paste(
  "<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01",
  "Data given as adjusted beta coefficient (95% confidence interval).",
  "Robust standard errors were employed.",
  "Each column is the output from 1 panel regression with fixed effects adjusted for the covariates in the table in addition to year dummies (not shown).",
  "Coefficients are interpreted as the absolute change in the dependent variable following a 1-unit change of the independent variable.",
  "GDP per capita is in current US dollars, and its unit is scaled up by 1,000.",
  "Population density represents the percentage of the population living in a density of &gt;1,000 people/km<sup>2</sup>.",
  "Urban residence represents the percentage of the population living in urban areas.",
  "The age dependency ratio represents the proportion of dependents (aged under 15 years or over 64 years) per 100 working-age individuals.",
  "Male education is expressed as years per capita and is age-standardised.",
  "Temperature is in degrees Celsius and is the mean population-weighted annual temperature.",
  "Rainfall is the mean population-weighted annual rainfall in millimetres per year, scaled down by 1,000.",
  "Earthquake and drought are binary variables representing their absence or presence.",
  "All armed conflict variables were lagged by 1 year.<br>GDP, gross domestic product; OECD, Organisation for Economic Co-operation and Development.",
  sep = "<br>"
)

output_file <- "outputs/table3_mortalitymodelcomparison.html"

texreg::htmlreg(
  texreg_models,
  custom.model.names = model_names,
  ci.force = TRUE,
  single.row = TRUE,
  digits = 2,
  file = output_file,
  omit.coef = "as.factor\\(year\\)|as.factor\\(iso_num\\)",
  custom.coef.names = c(
    "Armed conflict", "LOG of GDP per capita", "OECD member",
    "Population Density", "Urban residence", "Age dependency ratio",
    "Male education", "Temperature", "Rainfall", "Earthquakes", "Droughts"
  ),
  custom.header = list(
    "Maternal Mortality" = 1:2,
    "Under 5 Mortality" = 3:4,
    "Infant Mortality" = 5:6,
    "Neonatal Mortality" = 7:8
  ),
  custom.note = table_note,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  include.nobs = FALSE,
  inline.css = FALSE,
  doctype = TRUE,
  html.tag = TRUE,
  head.tag = TRUE,
  body.tag = TRUE,

  caption.above=TRUE,
  caption="Table 3. Complete Case Analysis vs Multiple Imputation (m=10) for regression models"
)

## htmlreg does not expose table-width/font controls, so add a small stylesheet
## to its HTML document. The wide table can scroll horizontally on narrow screens.
html <- readLines(output_file, warn = FALSE, encoding = "UTF-8")
style <- paste0(
  "<style>",
  "body { font-family: Arial, Aptos, sans-serif; margin: 1.5rem; }",
  "table { border-collapse: collapse; width: 100%; min-width: 1500px; table-layout: auto; }",
  "th, td { font-family: Arial, Aptos, sans-serif; padding: 0.45rem 0.7rem; vertical-align: top; white-space: nowrap; }",
  "td:first-child, th:first-child { text-align: left; }",
  "</style>"
)
head_end <- grep("</head>", html, ignore.case = TRUE)
if (length(head_end)) {
  html[head_end[1]] <- sub("</head>", paste0(style, "</head>"), html[head_end[1]], ignore.case = TRUE)
} else {
  html <- c(style, html)
}
writeLines(html, output_file, useBytes = TRUE)
