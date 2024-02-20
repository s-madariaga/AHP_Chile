
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

# Variable treatment
is.Bool <- function(x) all(na.omit(x) %in% c(0, 1))
is.not.Bool <- function(x) !is.Bool(x)

ens <- ens %>% 
  mutate_if(is.Bool,
            ~as.numeric(as.character(.))) %>% 
  mutate_if(is.not.Bool,
            ~factor(., levels = attr(., "labels"), label = names(attr(., "labels"))))

# Prop function
prop <- function(x){sum(x, na.rm = TRUE) / length(x)}

# Descriptive table
table_1 <- ens %>% 
  setNames(labelsENS) %>% 
  one_hot(.) %>% 
  summarise_all(
    list(
      Valid = ~length(na.omit(.)),
      Proportion = ~prop(.)
      # `Standard deviation` = ~sd(., na.rm = TRUE),
      # Kurtosis = ~kurtosis(., na.rm = TRUE),
      # Skewness = ~skewness(., na.rm = TRUE),
      # Mínimum = ~min(., na.rm = TRUE),
      # q25 = ~quantile(., 0.25, na.rm = TRUE),
      # Median = ~median(., na.rm = TRUE),
      # q75 = ~quantile(., 0.75, na.rm = TRUE),
      # Maximum = ~max(., na.rm = TRUE)
    )
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "^(.*)_([^_]+)$")

# Visualization
table_1 %>% 
  gt() %>% 
  tab_header(
    title = "Descriptive statistics",
    subtitle = paste0("Encuesta Nacional de Salud, 2016, (N = ", nrow(ens),")")
  ) %>% 
  fmt_percent(columns = "Proportion") %>% 
  gt_theme_538()


# Missing Data Cases and Distribution

## Back to the original dataset
ens_missAnalysis <- ens_master %>% select(all_of(variables))

## First, check for codes of missing case tipology in data (e.g. -9999)
lapply(mutate_all(ens_missAnalysis, ~as.numeric(as.character(.))), unique)

## Missing data frequency (using processing data)
table_missings <- ens %>% 
  setNames(paste0(labelsENS, " (", names(.), ")")) %>% 
  map_df(function(x) round(sum(is.na(x))*100 / nrow(.))) %>%  
  gather("Variable", "% Missings")

## Rasterplot Missing cases by id_row
ens |> vis_miss()

ens |> naniar::gg_miss_var()

ens |> naniar::gg_miss_var(man)
ens |> naniar::gg_miss_var(ageg)
ens |> naniar::gg_miss_var(rural)
ens |> naniar::gg_miss_var(education)

ens |> naniar::gg_miss_var(prev_week)

ens |> naniar::gg_miss_var(ld1)
ens |> naniar::gg_miss_var(ld2)
ens |> naniar::gg_miss_var(ld3)

# V-cramer

cramers_matrix <- matrix(NA, ncol(ens), ncol(ens), dimnames = list(names(ens), names(ens)))

for(i in 1:ncol(ens)) {
  for(j in 1:ncol(ens)) {
    cramers_matrix[i,j] <- DescTools::CramerV(ens[[i]], ens[[j]])
  }
}

# Mostrar la matriz de Cramer's V
cramers_matrix %>% 
  as.data.frame() %>%
  round(2) %>% 
  setNames(labelsENS) %>%
  mutate(variable = labelsENS) %>% 
  relocate(variable) %>% 
  gt() %>% 
  tab_header(
    title = "Cramer's V correlation matrix",
    subtitle = paste0("Encuesta Nacional de Salud, 2016, (N = ", nrow(ens),")")
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
          panel.spacing = unit(0, "lines"))  # Eliminar espacios entre los cuadrados
          # plot.margin = unit(c(0, 0, 0, 0), "cm")
     

  gcramer
}

