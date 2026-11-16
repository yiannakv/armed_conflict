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
