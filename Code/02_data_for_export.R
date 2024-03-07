
## Load packages
source("Code/Assets/00a_packages.R")


## Functions

## Useful funciton to transform factors to double and integers
as.factor.double <- function(x){
  if(any(!is.na(as.numeric(levels(x))))){
    x <- as.integer(levels(x)[x])
  } else {
    x <- as.numeric(x)
  }
  return(x)
}

## Load dataset
ens_master <- read_stata("Input/NHS2016-17_newvars.dta") |> data.table()

# Preprocessing variables -------------------------------------------------

ens <- ens_master |>
  mutate(
    
    ## 01- Harm --------------------------
    
    ld1 = factor(ld1),
    ld2 = factor(ld2),
    ld3 = factor(ld3),
    dld = factor(dld),
    
  
    ## 02- Alcohol consumption -----------
    
    days_peryear_consumption = 
      case_when(m7p9 == 1 ~ 0, 
                m7p9 == 2 ~ 6,
                m7p9 == 3 ~ 36, 
                m7p9 == 4 ~ 130, 
                m7p9 == 5 ~ 287),
    
    days_peryear_consumption = as.numeric(days_peryear_consumption), ## AUX variable (removed)
    drinks_perday = ifelse(m7p9 == 1, 0, m7p10b),                    ## AUX variable (removed)
    
    net_consumption = days_peryear_consumption*drinks_perday,
    levels_consumption = 
      case_when(
        net_consumption == 0 ~ 1,
        net_consumption <= 4 ~ 2,
        net_consumption <= 12 ~ 3,
        net_consumption > 12 ~ 4
      ),
    levels_consumption = factor(levels_consumption, levels = 1:4, labels = c("Never",
                                                                             "Lowest", 
                                                                             "Middle", 
                                                                             "Highest")),
    frequency_consumption = factor(m7p9, levels = 1:5, labels = c("Never",
                                                                  "Once a month or less",
                                                                  "2 to 4 times a month",
                                                                  "2 to 3 times a week",
                                                                  "4 or more times a week")),
    prev_year = factor(prev_year),
    prev_month = factor(prev_month),
    prev_week = factor(prev_week),
    
    former_drinker = as.integer(m7p9 == 1 & m7p17 %in% c(2, 3)),
    former_drinker = factor(former_drinker),
    
    consumption_frequency = case_when(
      m7p9 %in% c(1, 2) ~ 1,
      m7p9 == 3 ~ 2,
      m7p9 %in% c(4, 5) ~ 3
    ),
    
    consumption_frequency = factor(consumption_frequency, levels = 1:3,
                                   labels = c("Low", "Medium", "High")),
    
    consumption_frequency_high = ifelse(consumption_frequency == "High", 1, 0),
    
    consumption_frequency_high = factor(consumption_frequency_high),
    
    consumption_intensity = ifelse(m7p9 == 1, 0, m7p10b),
    
    consumption_intensity_cat = 
      case_when(
        (sex == 0) & (consumption_intensity <= 1) ~ 1,
        (sex == 1) & (consumption_intensity <= 2) ~ 1,
        
        (sex == 0) & (consumption_intensity <= 2) ~ 2,
        (sex == 1) & (consumption_intensity <= 3) ~ 2,
        
        (sex == 0) & (consumption_intensity <= 3) ~ 3,
        (sex == 1) & (consumption_intensity <= 4) ~ 3
      ),
    
    consumption_intensity_cat = factor(consumption_intensity_cat, levels = 1:3, labels = c("Low",
                                                                                           "Medium",
                                                                                           "High")),
    
    consumption_intensity_high = ifelse(consumption_intensity_cat == "High", 1, 0),
    
    consumption_intensity_high = factor(consumption_intensity_high),
    
    aud1 = ifelse(sum_audit > 8 & sum_audit <= 15, 1, 0),
    aud1 = factor(aud1),
    aud2 = ifelse(sum_audit > 15 & sum_audit < 20, 1, 0),
    aud2 = factor(aud2),
    aud3 = ifelse(sum_audit > 20 & sum_audit <= 40, 1, 0),
    aud3 = factor(aud3),
    
    hed_dummy = 
      case_when(
        (m7p11a %in% c(4, 5)  & sex == 0) ~ 1,
        (m7p11a %in% c(1, 2, 3) & sex == 0) ~ 0,
        (m7p11b %in% c(4, 5)  & sex == 1) ~ 1,
        (m7p11b %in% c(1, 2, 3) & sex == 1) ~ 0,
        TRUE ~ 0
      ),
    
    hed_dummy = factor(hed_dummy, levels = 0:1),
    
    hed_cat5 = case_when(
      sex == 0 ~ m7p11b,
      sex == 1 ~ m7p11c
    ),
    
    hed_cat3 = case_when(
      hed_cat5 %in% c(1, 2) ~ 1,
      hed_cat5 == 3 ~ 2,
      hed_cat5 %in% c(4, 5) ~ 3
    ),
    
    hed_cat3 = factor(hed_cat3, levels = 1:3, labels = c("Less than once a month",
                                                         "Monthly",
                                                         "Weekly or more")),
    
    hed_cat5 = factor(hed_cat5, levels = 1:5, labels = c("Never",
                                                         "Less than once a month",
                                                         "Monthly",
                                                         "Weekly",
                                                         "Every day or almost every day")),
    
    
    ## 04- Socioeconomic status ----------
    
    hincpc_cat = factor(hincpc_cat, levels = 1:3, labels = c("Lowest", "Middle", "Highest")),
    education = factor(education, levels = 1:3, labels = c("<8 years", "8-12 years", "13+ years")),
    private_health = factor(private_health),
    health_insurance = factor(health_insurance),
    
    
    ## 05- Risk or mitigating factors ----
    
    ### (a) Comorbilities ====
    diabetes = factor(diabetes),
    hypertension = factor(hypertension),
    smet = factor(smet),
    obesity = factor(obesity),
    
    ### (b) Substance use ====
    tobacco = ifelse(tobacco %in% 1:2, 0, 1),
    tobacco = factor(tobacco),
    
    ### (c) Mental health ====
    depression = factor(depression),
    
    ### (d) Social capital ====
    support = factor(support),
    partner = as.integer(partner == 1),
    partner = factor(partner, labels = c("In a relationship", "Single")),
    
    
    ## 06- Area --------------------------
    
    rural = factor(rural),
    
    ## 07- Age ---------------------------
    
    ageg = factor(ageg, levels = 1:3, labels = c("[15-29]", "[30-64]", "[65+]")),

    ## 08- Sex ---------------------------
    
    sex = factor(sex),
    
    ## 09- Weights -----------------------
    
    fw = round(fexp_f1f2ex2p_corr),
    pw = fw/14518950
         
  ) |>
  rename(male = sex,
         prevalence = m7p9) |>
  select(-c(days_peryear_consumption, drinks_perday))

# Extra procedures for processing -----------------------------------------

## List for variable selection
source("Code/Assets/00b_variables_selection.r")

## Reorder data
ens <- ens |>
  select(all_of(c(colnames(ens)[!colnames(ens) %in% as.vector(unlist(variables))],
         as.vector(unlist(variables)))))

# Create a numeric version of dataset (converting factors to integers)
ens_numeric <- ens |> mutate_at(vars(as.vector(unlist(variables))), as.factor.double) |> suppressWarnings()

## Create a labelled data for non-numeric dataset
source("Code/Assets/00d_labelled_data.R")

# Export dataset ----------------------------------------------------------

## Export csv
write.csv(ens_numeric, "Output/NHS2016_processed.csv")

## Export RData (two databases in a .RData file)
save(ens, ens_numeric, file = "Output/NHS2016_processed_databases.RData")
 
## Export RDS
saveRDS(ens, "Output/NHS2016_processed.rds")
saveRDS(ens_numeric, "Output/NHS2016_processed_numeric.rds")

## Dataset preprocesing for stata databse format (pending)
# file.edit("Code/Assets/00e_data_for_stata.r")

## Cleaning workspace ##
rm(list = ls()); gc()




