## Selection of relevant variables (by type)
variables <- list(dependent_variables = c("ld1", "ld2", "ld3", "dld"),
                  alcohol_consumption = c("prevalence", "prev_week", "prev_month",
                                          "prev_year", "former_drinker", 
                                          "consumption_frequency", "consumption_frequency_high",
                                          "consumption_intensity", "consumption_intensity_cat",
                                          "consumption_intensity_high", "net_consumption",
                                          "aud1", "aud2", "aud3", "hed_cat5",
                                          "hed_cat3", "hed_dummy"),
               income = c("hincpc", "hincpc_cat"),
               educaion = "education",
               acess_health = c("private_health", "health_insurance"),
               comorbidities = c("diabetes", "hypertension", "smet", "obesity"),
               substance_use = "tobacco",
               mental_health = "depression",
               social_capital = c("support", "partner"),
               rurality = "rural",
               age = c("age", "ageg"),
               sex = "male",
               weights = c("fw", "pw"))