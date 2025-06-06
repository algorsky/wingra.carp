library(tidyverse)
library(lavaan) # for SEM
library(semPlot)

# Renaming just so the final plot is legible 
df = read_csv('data/summary_medians.csv') |> 
  rename(zoops = summer_zoop, filA = sum_fil_algae, precip = sum_precip,
         chl = chloro_median, secchi = secchi_median, TP = tp_median, 
         TN = totnuf_median) |> 
  filter(group == 'post') |> # just analyze post group
  select(secchi, TN, TP, chl, filA, zoops, precip) |> 
  mutate(
    TN_mol = TN / 14.01,
    TP_mol = TP / 30.97,
    TN_TP_molar = TN_mol / TP_mol
  )|>
  mutate(across(everything(), ~ scale(.))) # standardize all columns

########## Model focused on nitrogen ##########
# Define the structural model
model <- '
  # Nutrients influenced by precipitation
  TN ~ precip
  secchi ~ precip

  # TP influencing Chloro and Sum Filamentous Algae
  # chl ~ TP
  # filA ~ TP

  chl ~ TN
  filA ~ TN
  
  # Secchi influence
  filA ~ secchi
  chl ~ secchi
  zoops ~ secchi
  
  # Fila and zoops 
  filA ~ zoops
  chl ~ zoops
'

# Fit the model
fit <- sem(model, data = df)

# Summary of the model
summary(fit, standardized = TRUE)

# Plot the SEM model
semPaths(fit, 
         what = "std",      # standardized estimates
         layout = "tree2",   # tree layout
         edge.label.cex = 1, # size of the edge labels
         sizeMan = 8,       # size of manifest variables
         sizeLat = 10,      # size of latent variables
         edge.color = "black",
         residuals = TRUE,  # show residuals
         intercepts = FALSE # hide intercepts
)

########## Model focused on phosphorus ##########

model2 <- '
  # Nutrients influenced by precipitation
  TP ~ precip
  secchi ~ precip

  # TP influencing Chloro and Sum Filamentous Algae
  chl ~ TP
  filA ~ TP
  
  # Secchi influence
  filA ~ secchi
  chl ~ secchi
  zoops ~ secchi
  
  # Fila and zoops 
  filA ~ zoops
  chl ~ zoops
'

# Fit the model
fit2 <- sem(model2, data = df)

# Summary of the model
# summary(fit2, standardized = TRUE)

# Plot the SEM model
semPaths(fit2, 
         what = "std",      # standardized estimates
         layout = "tree2",   # tree layout
         edge.label.cex = 1, # size of the edge labels
         sizeMan = 8,       # size of manifest variables
         sizeLat = 10,      # size of latent variables
         edge.color = "black",
         residuals = TRUE,  # show residuals
         intercepts = FALSE # hide intercepts
)
########## Model focused on TN:TP ##########
# Define the structural model
model3 <- '
  # Nutrients influenced by precipitation
  TN_TP_molar ~ precip
  secchi ~ precip

  # TN_TP_molar influencing Chloro and Sum Filamentous Algae
  # chl ~ TN_TP_molar
  # filA ~ TN_TP_molar

  chl ~ TN_TP_molar
  filA ~ TN_TP_molar
  
  # Secchi influence
  filA ~ secchi
  chl ~ secchi
  zoops ~ secchi
  
  # Fila and zoops 
  filA ~ zoops
  chl ~ zoops
'

# Fit the model
fit3 <- sem(model3, data = df)

# Summary of the model
summary(fit3, standardized = TRUE)

# Plot the SEM model
semPaths(fit3, 
         what = "std",      # standardized estimates
         layout = "tree2",   # tree layout
         edge.label.cex = 1, # size of the edge labels
         sizeMan = 8,       # size of manifest variables
         sizeLat = 10,      # size of latent variables
         edge.color = "black",
         residuals = TRUE,  # show residuals
         intercepts = FALSE # hide intercepts
)
