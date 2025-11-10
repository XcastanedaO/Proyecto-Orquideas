names(non_fatal_2024)[names(non_fatal_2024) %in% c("life_cycle", "event_scenario", "event_zone",
                                                   "alleged_aggressor","alleged_aggressor_sex")] <- c("life_cycle_stage", 
                                                                                                      "setting_of_event",  "geographical_area", 
                                                                                                      "suspect_agressor", "suspect_aggressor_sex")
names(sexual_2015_2023)[names(sexual_2015_2023) %in% c("life_cycle", "event_scenario", "event_zone",
                                                       "alleged_aggressor","alleged_aggressor_sex")] <- c("life_cycle_stage", 
                                                                                                          "setting_of_event",  "geographical_area", 
                                                                                                          "suspect_agressor", "suspect_aggressor_sex")
names(interpersonal_2015_2023)[names(interpersonal_2015_2023) %in% c("life_cycle", "event_scenario", "event_zone",
                                                                     "alleged_aggressor","alleged_aggressor_sex")] <- c("life_cycle_stage", 
                                                                                                                        "setting_of_event",  "geographical_area", 
                                                                                                                        "suspect_agressor", "suspect_aggressor_sex")
names(domestic_2015_2023)[names(domestic_2015_2023) %in% c("life_cycle", "event_scenario", "event_zone",
                                                           "alleged_aggressor","alleged_aggressor_sex")] <- c("life_cycle_stage", 
                                                                                                              "setting_of_event",  "geographical_area", 
                                                                                                              "suspect_agressor", "suspect_aggressor_sex")