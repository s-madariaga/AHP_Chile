
# Efficient method to install and load packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(haven, data.table, tidyverse, e1071,
               texreg, rockchalk, labelled, 
               mltools)

# Load dataset
ens_master <- read_stata("Input/NHS2016-17_newvars.dta")

# Subsetting dataset
variables <- c("ld1", "ld2", "ld3", "prev_week", "hincpc_cat", "tobacco",
          "sex", "obesity", "diabetes", "smet", "depression", 
          "health_insurance", "support", "rural",
          "education", "partner", "dld", "ageg")

ens <- ens_master %>% 
  select(all_of(vars)) %>% 
  as.data.table() %>%
  rename(man = sex)

# Custom the labels (avoid repeated labels)
Hmisc::label(ens$ld1) <- "Liver Damage 1"
Hmisc::label(ens$ld2) <- "Liver Damage 2"
Hmisc::label(ens$ld3) <- "Liver Damage 3"
Hmisc::label(ens$man) <- "Man"
Hmisc::label(ens$diabetes) <- "Diabetes"
Hmisc::label(ens$depression) <- "Depression"
Hmisc::label(ens$rural) <- "Rural area"

# Saving labels from ENS
labelsENS <- sapply(ens, Hmisc::label)

# Variable treatment
is.Bool <- function(x) all(na.omit(x) %in% c(0, 1))
is.not.Bool <- function(x) !is.Bool(x)

ens <- ens %>% 
  mutate_if(is.Bool,
            ~as.numeric(as.character(.))) %>% 
  mutate_if(is.not.Bool,
            ~factor(., levels = attr(., "labels"), label = names(attr(., "labels"))))

# Descriptive table
ens %>% 
  setNames(labelsENS) %>% 
  one_hot(.) %>% 
  summarise_all(
    list(
      Valid = ~length(na.omit(.)),
      `Mean /\nProp.` = ~mean(., na.rm = TRUE),
      `Desviación estándar` = ~sd(., na.rm = TRUE),
      Kurtosis = ~kurtosis(., na.rm = TRUE),
      Skewness = ~skewness(., na.rm = TRUE),
      Mínimum = ~min(., na.rm = TRUE),
      q25 = ~quantile(., 0.25, na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      q75 = ~quantile(., 0.75, na.rm = TRUE),
      Maximum = ~max(., na.rm = TRUE)
    )
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "^(.*)_([^_]+)$") %>% 
  flextable::flextable()