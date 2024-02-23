
# Start R sesion
rm(list = ls())
gc()
cat("\14")

# Efficient method to install and load packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(haven, data.table, tidyverse, e1071, visdat,
               texreg, rockchalk, labelled, mltools, gt, gtExtras,
               DescTools)

# Load dataset
ens_master <- read_stata("Input/NHS2016-17_newvars.dta")

# Subsetting dataset
variables <- c("ld1", "ld2", "ld3", "prev_week", "hincpc_cat", "tobacco",
          "sex", "obesity", "diabetes", "smet", "depression", 
          "health_insurance", "support", "rural",
          "education", "partner", "dld", "ageg")

ens <- ens_master %>% 
  select(all_of(variables)) %>% 
  as.data.table() %>%
  # Change the name of the variable "sex"
  rename(man = sex)

# Custom the labels (avoid repeated labels)
Hmisc::label(ens$ld1) <- "Liver Damage 1"
Hmisc::label(ens$ld2) <- "Liver Damage 2"
Hmisc::label(ens$ld3) <- "Liver Damage 3"
Hmisc::label(ens$man) <- "Man"
Hmisc::label(ens$diabetes) <- "Diabetes"
Hmisc::label(ens$depression) <- "Depression"
Hmisc::label(ens$rural) <- "Rural area"
Hmisc::label(ens$hincpc_cat) <- "Per capita household income"

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


