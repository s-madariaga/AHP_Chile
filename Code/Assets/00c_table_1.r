features <- variables |> unlist() |> as.vector()

table_1_html <- table_1 %>% 
  mutate(
    type = case_when(str_extract(Variable, "^[^_]+") %in% 
                       labelsENS[dependent_variables] ~ 1,
                     str_extract(Variable, "^[^_]+") %in% 
                       labelsENS[risk_factors] ~ 2,
                     str_extract(Variable, "^[^_]+") %in% 
                       labelsENS[comorbidities] ~ 3,
                     str_extract(Variable, "^[^_]+") %in% 
                       labelsENS[c(sociodemographic_factors, social_networks)] ~ 4),
    type = factor(type, level = 1:4, labels = c("Dependent variables (Damage)",
                                                "Risk factors",
                                                "Comorbilities",
                                                "Sociodemographic variables"))
  ) %>% 
  arrange(type) %>%
  mutate(var1 = str_extract(Variable, "(?<=_).*$"),
         var1 = ifelse(is.na(var1), "", var1),
         Variable = str_extract(Variable, "^[^_]+")) %>% 
  rename(" "=var1) %>% 
  relocate(1, ncol(.), 2:(ncol(.)-1)) %>%
  gt(groupname_col = "type",
     rowname_col = "Variable"
     ) %>% 
  tab_header(
    title = "Descriptive statistics",
    subtitle = paste0("Encuesta Nacional de Salud, 2016, (N = ", nrow(ens),")")
  ) %>% 
  fmt_percent(columns = "Proportion") %>% 
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_row_groups()
  ) %>% 
  tab_options(
    data_row.padding = px(2),
    summary_row.padding = px(3),
    row_group.padding = px(4),
    heading.align = 'left',
    heading.title.font.size = px(20),
    row_group.border.top.style = 'none',
    table_body.border.top.style = 'none',
    
    # Lines
    table.font.names = 'Merriweather',
    table_body.hlines.color =  'white',
  )


# Save the table
gtsave(data = table_1_html, filename = "Table_1.html", path = "Output/")

table_1_html %>% print

rm(table_1_html)