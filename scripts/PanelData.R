
finaldata <- readRDS("data/processed/df_clean.rds")
library(plm)

lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso, 
            data = finaldata) #you get an intercept for every country (fixed effects model)


plmmod <- plm(maternal_mortality ~ as.numeric(armed_conflict) + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within", data = finaldata)


summary(plmmod)
summary(lmmod)

#rather than writing out the predictors each time, we can encompass it in a formula 

preds <- as.formula(" ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +  
                  iso + as.factor(year)")
#basically we can 'copy' this into the linear model equation

#lm is LINEAR MODEL. plm is panel linear model. 
maternal_mortalitymod <- lm(update.formula(preds, maternal_mortality ~ .), data = finaldata)
un5mormod <- lm(update.formula(preds, under5_mortality ~ .), data = finaldata)
infmormod <- lm(update.formula(preds, infant_mortality ~ .), data = finaldata)
neomormod <- lm(update.formula(preds, neonatal_mortality ~ .), data = finaldata)

#IN CLASS ASSIGNMENT 

# 1 LOG transform GDP and use the transformed GDP as one of the predictors 
library(dplyr)
final_data_log <- finaldata %>% mutate(log_gdp = log(gdp_1000))

###make new predictor vectors
preds_log <- as.formula(" ~ armed_conflict + log_gdp + gdp_1000 + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +  
                  iso + as.factor(year)")

# 2 FIT the four mortality mod3els using plm()
maternal_mortality_log <- plm(update.formula(preds_log, maternal_mortality ~ .), 
  index = c("iso"), model = "within", data = final_data_log)

un5mormod_log <- plm(update.formula(preds_log, under5_mortality ~ .), 
  index = c("iso"), model = "within", data = final_data_log)

infmormod_log <- plm(update.formula(preds_log, infant_mortality ~ .), 
  index = c("iso"), model = "within", data = final_data_log)

neomormod_log <- plm(update.formula(preds_log, neonatal_mortality ~ .), 
  index = c("iso"), model = "within", data = final_data_log)

# 4 CREATE A TABLE 

install.packages("stargazer")
library(stargazer)

stargazer(maternal_mortality_log, un5mormod_log, infmormod_log, neomormod_log,
  title="Table 2. The association between armed conflict and maternal and child mortality (adjusted beta coefficients, 95% confidence intervals).",
  
  column.labels =c("Maternal mortality ratio<br>per 100,000 live births", "Under- 5 mortality rate<br>per 1,000 live births",
                    "Infant mortality rate<br>per 1,000 live births", "Neonatal mortality rate<br>per 1,000 live births "),
  
  covariate.labels=c("Armed conflict", "LOG of GDP per capita", "GDP per capita", "OECD member", "population density", 
                    "Urban residence", "Age dependency ratio", "Male education", "Temperature", "Rainfall", "Earthquakes", "Droughts"),
  
  align=TRUE, type="html", keep.stat="n", ci.level=0.95, ci=TRUE, single.row=TRUE,
  out="outputs/table2_panelregression.html", omit="as.factor\\(year\\)",

  digits=2, dep.var.labels.include=FALSE, dep.var.caption="",

  add.lines=list(c("Number of Countries", "183", "185", "185", "185")),

  notes = paste(
  "<sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01<br>", #sup is for superscript. The </ distinguishes closing 
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
  
  notes.align="l", notes.append=FALSE)

#used codex to help me beautify this a tad 
css <- "<style>
  body { font-family: 'Times New Roman', Georgia, serif; margin: 40px auto; max-width: 1000px; color: #222; }

  /* fixed table width so the long footnote wraps inside the table instead of squeezing columns */
  table { border-collapse: collapse; width: 100%; font-size: 14px; }

  /* spacing and alignment */
  table td { padding: 5px 14px; text-align: center; vertical-align: top; line-height: 1.35; }
  table td:first-child { text-align: left; white-space: nowrap; width: 18%; }

  /* title and footnote cells (the ones spanning all columns) */
  table td[colspan] { text-align: left; }

</style>"

html <- readLines("outputs/table2_panelregression.html")
html <- gsub("<td>\\(([1-4])\\)</td>", "<td></td>", html)  #find cells 1-4 in row 3 (\\). Replace with blank,



# optional: blank the (1)-(4) text, keep the line
writeLines(c(css, html), "outputs/table2_panelregression.html")

#to get the number of countries for each output 
finaldata %>%
  filter(!is.na(maternal_mortality)) %>%
  count(iso) %>%
  nrow()

finaldata %>%
  filter(!is.na(neonatal_mortality)) %>%
  count(iso) %>%
  nrow()

finaldata %>%
  filter(!is.na(under5_mortality)) %>%
  count(iso) %>%
  nrow()

finaldata %>%
  filter(!is.na(infant_mortality)) %>%
  count(iso) %>%
  nrow()