
# Start R sesion
rm(list = ls()); gc()
cat("\14")

# Set options
options(digits = 2)

# Efficient method to install and load packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(haven, data.table, tidyverse, e1071, visdat,
               texreg, rockchalk, labelled, mltools, gt, gtExtras,
               DescTools)

# Load dataset
ens_master <- read_stata("Input/NHS2016-17_newvars.dta")


# Creating new variables to add
ens_master <- ens_master |>
  mutate(dias_consumo_anio = 
           case_when(m7p9 == 1 ~ 0, m7p9 == 2 ~ 6, m7p9 == 3 ~ 36, m7p9 == 4 ~ 130, m7p9 == 5 ~ 287),
         tragos_dia = ifelse(m7p9 == 1, 0, m7p10b),
         net_consumption = dias_consumo_anio*tragos_dia,
         # risk_consumption,
         hed1 = case_when(
           (m7p11b %in% c(4, 5)  & sex == 0) ~ 0,
           (m7p11b %in% c(1, 2, 3) & sex == 0) ~ 1,
           (m7p11c %in% c(4, 5)  & sex == 1) ~ 0,
           (m7p11c %in% c(1, 2, 3) & sex == 1) ~ 1
         ),
         hed2 = case_when(
           sex == 0 ~ m7p11b,
           sex == 1 ~ m7p11c
         ),
         former_drinkers = as.integer(m7p9 == 1 & m7p17 %in% c(2, 3))) |>
  rename(man = sex,
         frequency_consumption = m7p9)

# Variables to choose in new dataset (by type)

variables <- list(

  # Sociodemographic variables
  sociodemographic_factors = c("man", "age", "ageg", "rural", "education", "household_income", "hincpc_cat", "private_health", "health_insurance"),
  
  # Alcohol consumption
  alcohol_consumption = c("prev_year", "prev_month", "prev_week", "sum_audit", "aud", "net_consumption", "frequency_consumption", "hed1", "hed2", "former_drinkers"), 
  
  # Comorbilities
  comorbilities = c("obesity", "waist", "diabetes", "tobacco", "smet", "depression"),
  
  # Social Networks
  social_networks = c("partner", "support"),
  
  # Dependent variable
  dependent_variable = c("ld1", "ld2", "ld3", "dld")
)


# Subsetting dataset with this variables
ens <- ens_master %>% 
  select(all_of(as.vector(unlist(variables)))) %>% 
  as.data.table()

# Custom the labels (avoid repeated labels)
Hmisc::label(ens$ld1) <- "Liver Damage 1"
Hmisc::label(ens$ld2) <- "Liver Damage 2"
Hmisc::label(ens$ld3) <- "Liver Damage 3"
Hmisc::label(ens$man) <- "Man"
Hmisc::label(ens$rural) <- "Rural area"
Hmisc::label(ens$hincpc_cat) <- "Per capita household income"
Hmisc::label(ens$sum_audit) <- "Audit"
Hmisc::label(ens$net_consumption) <- "Net consumption"
Hmisc::label(ens$hed1) <- "Heavy Episodic Drinking (binary)"
Hmisc::label(ens$hed2) <- "Heavy Episodic Drinking (categoric)"
Hmisc::label(ens$former_drinkers) <- "Former drinkers"
Hmisc::label(ens$waist) <- "Waist circumference"
Hmisc::label(ens$diabetes) <- "Diabetes"
Hmisc::label(ens$depression) <- "Depression"
Hmisc::label(ens$dld) <- "Diagnosis of Liver Damage"


# Saving labels from ENS
labelsENS <- sapply(ens, Hmisc::label)

# Missings ----------------------------------------------------------------

## First, check for codes of missing case tipology in data (e.g. -9999)
lapply(ens, unique)

## Missing data frequency (using processing data)
table_missings <- ens %>% 
  setNames(paste0(labelsENS, " (", names(.), ")")) %>% 
  map_df(function(x) round(sum(is.na(x))*100 / nrow(.))) %>%  
  gather("Variable", "% Missings")

## Rasterplot Missing cases by id_row
ens |> vis_miss()
ens |> naniar::gg_miss_var()

# Remove missings
ens <- na.omit(ens)


# Variables treatment -----------------------------------------------------

# Variable treatment
is.Bool <- function(x) all(na.omit(x) %in% c(0, 1))
is.not.Bool <- function(x) !is.Bool(x)

ens <- ens %>% 
  mutate_if(is.Bool,
            ~as.numeric(as.character(.))) %>% 
  mutate_if(is.not.Bool,
            ~factor(., levels = attr(., "labels"), label = names(attr(., "labels"))))


# Descriptive table -------------------------------------------------------

# Prop function
prop <- function(x){sum(x, na.rm = TRUE) / length(x)}

# Descriptive table
table_1 <- ens %>% 
  setNames(labelsENS) %>% 
  one_hot(.) %>%
  summarise_all(
    list(
      Valid = ~length(na.omit(.)),
      Percentage = ~prop(.)
    )
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "^(.*)_([^_]+)$")

# Visualization
source("Code/table_1.r")

# Cramers V ---------------------------------------------------------------

cramers_matrix <- matrix(NA, ncol(ens), ncol(ens), dimnames = list(names(ens), names(ens)))

for(i in 1:ncol(ens)) {
  for(j in 1:ncol(ens)) {
    cramers_matrix[i,j] <- DescTools::CramerV(ens[[i]], ens[[j]])
  }
}

# Mostrar la matriz de Cramer's V
cramers_melted <- reshape2::melt(cramers_matrix)

# Crear el gráfico de calor con una paleta de colores invertida
ggplot(cramers_melted, aes(Var1, Var2, fill=value)) +
  geom_tile(linewidth=0.25, color="gray70") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(fill = "Cramer's V \n") +
  theme_light() + 
  theme(legend.position = "right",
        panel.grid=element_blank(),
        legend.title = element_text(size=10),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.spacing = unit(0, "lines"),  # Eliminar espacios entre los cuadrados
        plot.margin = unit(c(0, 0, 0, 0), "cm")
        ) 

# Crear el gráfico de calor con una paleta de colores invertida
{
  gcramer <- ggplot(cramers_melted, aes(Var1, Var2, fill=value)) +
    geom_tile(linewidth=0.25, color="gray70") +
    scale_fill_gradient(low = "white", high = "blue") +
    geom_text(aes(label = ifelse(value > 0.2, round(value, 2), ""))) +
    labs(fill = "Cramer's V \n", 
         title = "Cramer's V correlation matrix",
         subtitle = expression(Phi[c] > 0.2)) +
    theme_light() + 
    theme(legend.position = "right",
          plot.title = element_text(face = "bold", size = 15),
          panel.grid=element_blank(),
          legend.title = element_text(size=10),
          axis.title.y  = element_blank(),
          axis.title.x  = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
          panel.spacing = unit(0, "lines")) 
          # plot.margin = unit(c(0, 0, 0, 0), "cm")
     

  gcramer
  }


