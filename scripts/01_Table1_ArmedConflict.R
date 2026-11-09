install.packages("table1")
install.packages("Hmisc")

library(table1)
library(Hmisc)

setwd("C:/Users/iamvi/OneDrive - University of Toronto/Year 2/Computation for Health Data/armed_conflict/data/processed")

#first we will source our data from the exploratory analysis 
df_clean <- readRDS('df_clean.rds')

df_clean$armed_conflict <- as.factor(df_clean$armed_conflict)
df_clean$armed_conflict  <- factor(df_clean$armed_conflict, levels=1:2, labels=c("No Armed Conflict", "Yes Armed Conflict"))
df_clean$oecd     <- factor(df_clean$oecd, levels=0:1, labels=c("No", "Yes"))
df_clean$earthquake  <- factor(df_clean$earthquake, levels=0:1, labels=c("No", "Yes"))
df_clean$drought <- factor(df_clean$drought, levels=0:1, labels=c("No", "Yes"))

label(df_clean$armed_conflict)      <- "Armed Conflict<sup>i</sup>"
label(df_clean$oecd)      <- "OECD Membership"
label(df_clean$earthquake)    <- "Had >=1 earthquake<sup>h</sup>"
label(df_clean$drought)    <- "Had >=1 drought<sup>h</sup>"
label(df_clean$pop_dens)  <- "Population Density<sup>a</sup>"
label(df_clean$gdp_1000)  <- "GDP Per Capita<sup>b</sup>"
label(df_clean$urban)  <- "Urban Residence<sup>c</sup>"
label(df_clean$age_dep)  <- "Age dependency ratio<sup>d</sup>"
label(df_clean$male_edu)  <- "Male Education<sup>e</sup>"
label(df_clean$temp)  <- "Temperature (C)<sup>f</sup>"
label(df_clean$rainfall_1000)  <- "Rainfall<sup>g</sup>"

my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits=2), c("",
        "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
    c("", sapply(stats.default(x), function(y) with(y,
        sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

footnote <- paste(
  "GDP = gross domestic product; OECD = Organisation for Economic Co-operation and Development.",
  "<sup>a</sup> Population density represents the percentage of the population living in a density of >1,000 people/km2.",
  "<sup>b</sup> GDP per capita is in current US dollars and is scaled by 1,000.",
  "<sup>c</sup> Urban residence represents the percentage of the population living in urban areas.",
  "<sup>d</sup> The age dependency ratio represents the proportion of dependents aged under 15 years or over 64 years per 100 working-age individuals.",
  "<sup>e</sup> Male education is expressed as years per capita and is age-standardised.",
  "<sup>f</sup> Temperature is the mean population-weighted annual temperature in degrees Celsius.",
  "<sup>g</sup> Rainfall is the mean population-weighted annual rainfall in millimetres per year, scaled by 1,000.",
  "<sup>h</sup> Earthquake and drought are binary variables representing their absence or presence.",
  "<sup>i</sup> All armed conflict variables were lagged by 1 year.",
  "Summary statistics are based on country-year observations from 2000 to 2017; countries may contribute multiple observations across years.",
  sep = "<br>")

caption <- "Table 1. Country baseline characteristics stratified by armed conflict status"

table1<-table1(~ pop_dens + gdp_1000 + age_dep + male_edu + temp + rainfall_1000 + urban +
         oecd + earthquake + drought | armed_conflict, data=df_clean, caption=caption, footnote=footnote,
       render.continuous=my.render.cont, render.categorical=my.render.cat)

table1


htmltools::save_html(table1, "../../outputs/table1.html")
