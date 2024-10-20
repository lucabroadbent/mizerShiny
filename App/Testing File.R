test <- harvestedshort

testparams <- guildparams%>%
  filter(Species=="Herring")
testparams <- testparams[1,]

for (i in 1:nrow(testparams)) {
  test2 <- test2 %>%
    mutate(
      Guild = ifelse(is.na(Guild) & w >= testparams$minw[i] & w < testparams$maxw[i], 
                     testparams$Feeding.guild[i], Guild)
    )
}

test <- test%>%
  group_by(Species)%>%
    for (i in 1:nrow(testparams)) {
      test2 <- test2 %>%
        mutate(
          Guild = ifelse(is.na(Guild) & w >= testparams$minw[i] & w < testparams$maxw[i], 
                         testparams$Feeding.guild[i], Guild)
        )
    }
  

assign_guild <- function(data, rules) {
  data <- data %>%
    mutate(Guild = NA_character_)  # Initialize Guild column with NA
  
  # Loop through each rule in the rules dataframe
  for (i in 1:nrow(rules)) {
    data <- data %>%
      mutate(
        #THIS CODE ASSUMES THAT ANYTHING UNDER W 0.05 IS PLANKTIVOROUS, AS IT IS VERY
        Guild = ifelse(w < 0.05, "Plank",
                       ifelse(
                       is.na(Guild) & w >= rules$minw[i] & w < rules$maxw[i], 
                       rules$Feeding.guild[i], Guild)
                       )
      )
  }
  
  return(data)
}


test <- test %>%
  group_by(Species) %>%
  group_modify(~ {
    species_data <- .x  
    
    species_name <- unique(species_data$Legend)
    
    species_rules <- guildparams %>%
      filter(Species == species_name)
    
    if (nrow(species_rules) == 0) {
      return(species_data)
    }
    
    assign_guild(species_data, species_rules)
    
  }) %>%
  ungroup() %>%
  #this next step takes out anything without an assigned guild, but you might not choose to do this
  #and then you can have a column of the change in biomass of species/sizes that we do not have guild rules for
  #this would be useful to observe where the biomass change is going, but would be confusing to interpret and explain.
  #(as its a possibility that all 3 guilds show a negative decrease, which looks like a decrease in biomass,
  #but may just be due to other sizes/species taking this biomass)
  drop_na(Guild)%>%
  group_by(Guild) %>%
  summarise(value = mean(value))
