library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(bslib)
library(plotly)
library(ggplot2)
library(gridlayout)
library(thematic)
library(tidyverse)
library(forcats)
library(shinyBS)
library(rintrojs)
library(patchwork)

server <- function(input, output, session) {
  
  
  observeEvent(input$start_tutorial, {
    req(input$bigtabpanel)  # Ensure bigtabpanel is not NULL
    req(input$mortnsp_tab)  # Ensure mortnsp_tab is not NULL
    req(input$breakpoint_tabpanel)  # Ensure breakpoint_tabpanel is not NULL
    
    intro_steps <- list()  # Initialize an empty list for steps
    
    if (input$bigtabpanel == "Single Species") {
      if (input$mortnsp_tab == "Biomass") {
        intro_steps <- list(
          list(element = "#species_slider", title = "Biomass Slider", 
               intro = "This slider will change the starting biomass of the 
                 species in the model. Changing the starting biomass intends to
                 show the role of different species within the ecosystem, 
                 as the knock on effect of a biomass change should be in seen 
                 in the plots. Hover over the information button to learn more."),
          list(element = "#yearspecies_slider", title = "Time Range Slider", 
               intro = "Changing the value of this slider will change the 
                 year that is plotted. It is useful to look at different time scales, 
                 as the impact of the imposed change may differ and it will show the 
                 oscillatory change in fish populations. The buttons below 
                 will set the year to 5, 15, or 30 years, which can be thought of 
                 as short, medium, and long term."),
          list(element = "#species_chose", title = "Species Selector", 
               intro = "Here is where you choose the species you want to investigate. 
                 It is possible to change all the species within the model."),
          list(element = "#goButton1", title = "Run Simulation", 
               intro = "Once you have chosen your settings in this configuration panel, 
                 press this button to run the simulation. It will take about 15 seconds.
                 "),
          list(element = "#select_species", title = "Species Order", 
               intro = "As some of the graphs show changes in each of the species within the model, 
            the order that these species are presented may allow for easier observation of 
            any general patterns. Hover over the information button to learn more."),
          list(element = "#plotTabs .nav-link[data-value='Species']", 
               intro = "The first plot presents the percentage change in each of the species. This percentage change is relative to an equal ecosystem,
      except without the change in the species you have decided to change. Each species has 3 bars, which indicate the species percentage change on 
      across a shorter timescale (a half of the chosen time), the chosen timescale and a longer timescale (double the chosen time)... 
      " ),
          list(element = "#plotTabs .nav-link[data-value='Species']", 
               intro = "Plotting all 3 timescales
      aids in understanding the oscillatory nature of fish populations, but also gives a greater resolution of the effect of the imposed change.
      It is possible to change the order of the species on the X axis by using the options in the configuration panel. 
      " ),
          list(element = "#plotTabs .nav-link[data-value='Size']", 
               intro = "The next plot shows the relative change in the size spectrum on a community level. Plotting the size spectrum informs
      the viewer of the change in community composition, more specifically, how the distribution of fish size has changed."
          ),
          list(element = "#plotTabs .nav-link[data-value='Guilds']", 
               intro = "The plot here showcases the change in the guilds within the species, which are fish in the ecosystem that share a distinct 
       feeding pattern in relation to other fish. Observing this plot helps to understand how the trophic dynamics of the ecosystem are changing.
      Additionally, the guilds are plotted as 3 bars of a short, chosen and long timescale."
          ),
          list(element = "#plotTabs .nav-link[data-value='Diet']", 
               intro = "The plot here is a matrix of each of the species on the X and Y axis, with the colour denoting the change
      in proportion of a given species (on the Y axis) in another species diet (on the X axis). Similar to the guild plot, this plot attempts to highlight the 
      trophic dynamics of the ecosystem and provides information as to why changes in species populations or the size spectrum may
      have occured."
          )
        )
      } else if (input$mortnsp_tab == "Mortality") {
        intro_steps <- list(
          list(element = "#mort_slider", title = "Mortality Slider", 
               intro = "The Mortality slider changes the mortality rate of a given species, 
                 across their entire size range. In mizer, species are separated into
      size bins, each with their own mortality rate. Each size bin can be thought of a 
      size of the species, so the small/medium/large individuals of the species.
      The value on this slider is multiplied by the rate of mortality that each size bin 
      has, then this new value is added on to the original mortality... 
      "
          ),
          list(element = "#mort_slider", title = "Mortality Slider", 
               intro = "
      Therefore, a value of 1.05 on the slider will increase the mortality of the species 
      by %5, similarly a value of 0.95 will decrease the mortality by
      5%. This change is imposed on a relative across all sizes of the given species, as smaller individuals of any fish
      species will have a higher mortality rate than larger individuals."
          ),
          list(element = "#yearspecies_slider_mort", title = "Time Range Slider", 
               intro = "Changing the value of this slider will change the 
                 year that is plotted. It is useful to look at different time scales, 
                 as the impact of the imposed change may differ and it will show the 
                 oscillatory change in fish populations. The buttons below 
                 will set the year to 5, 15, or 30 years, which can be thought of 
                 as short, medium, and long term."),
          list(element = "#species_choose_mort", title = "Species Selector", 
               intro = "Here is where you choose the species you want to investigate. 
                 It is possible to change all the species within the model."),
          list(element = "#goButton3", title = "Run Simulation", 
               intro = "Once you have chosen your settings in this configuration panel, 
                 press this button to run the simulation. It will take about 15 seconds.
                 "),
          list(element = "#select_species_mort", title = "Species Order", 
               intro = "As some of the graphs show changes in each of the species within the model, 
            the order that these species are presented may allow for easier observation of 
            any general patterns. Hover over the information button to learn more."),
          list(element = "#plotTabs_mort .nav-link[data-value='Species']", 
               intro = "The first plot presents the percentage change in each of the species. This percentage change is relative to an equal ecosystem,
      except without the change in the species you have decided to change. Each species has 3 bars, which indicate the species percentage change on 
      across a shorter timescale (a half of the chosen time), the chosen timescale and a longer timescale (double the chosen time)... 
      " ),
          list(element = "#plotTabs_mort .nav-link[data-value='Species']", 
               intro = "Plotting all 3 timescales
      aids in understanding the oscillatory nature of fish populations, but also gives a greater resolution of the effect of the imposed change.
      It is possible to change the order of the species on the X axis by using the options in the configuration panel. 
      " ),
          list(element = "#plotTabs_mort .nav-link[data-value='Size']", 
               intro = "The next plot shows the relative change in the size spectrum on a community level. Plotting the size spectrum informs
      the viewer of the change in community composition, more specifically, how the distribution of fish size has changed."
          ),
          list(element = "#plotTabs_mort .nav-link[data-value='Guilds']", 
               intro = "The plot here showcases the change in the guilds within the species, which are fish in the ecosystem that share a distinct 
       feeding pattern in relation to other fish. Observing this plot helps to understand how the trophic dynamics of the ecosystem are changing.
      Additionally, the guilds are plotted as 3 bars of a short, chosen and long timescale."
          ),
          list(element = "#plotTabs_mort .nav-link[data-value='Diet']", 
               intro = "The plot here is a matrix of each of the species on the X and Y axis, with the colour denoting the change
      in proportion of a given species (on the Y axis) in another species diet (on the X axis). Similar to the guild plot, this plot attempts to highlight the 
      trophic dynamics of the ecosystem and provides information as to why changes in species populations or the size spectrum may
      have occured."
          )
        )
      }
    } else if (input$bigtabpanel == "Breakpoint") {
      if (input$breakpoint_tabpanel == "Mortality") {
        intro_steps <-  list(
          list(element = "#breakmort", title = "Mortality Slider", 
               intro = "The mortality slider here works in the same way as the mortality
                 slider in the Single Species section of the app. The only difference is now
                 you choose a range of mortality values to investigate."
          ),
          list(element = "#breakyear_mort", title = "Year", 
               intro = "Similar to as seen prior, this slider chooses the year that you want to plot."
          ),
          list(element = "#breaknumber", title = "Break Number", 
               intro = "The number chosen here is the amount of simulations that you want to run
                 between the range of mortality values chosen on the slider. Each value of changed mortality 
                 is equidistant to others."),
          list(element = "#breakplotting_mort .nav-link[data-value='Scrollable Species']", title = "Scrollable Plot", 
               intro = "This is the first plot that you will see. It is the same as the species plots
               found in the Single Species section of the app, except that there is one plot for each
               simulation that you have ran, and these plots are placed on top of each other, so that it
               is possible to scroll down and observe the change."
          ),
          list(element = "#breakplotting_mort .nav-link[data-value='Line Graph']", title = "Line Breaks", 
               intro = "The next plot is of the same information, but plotted in a different format.
               This time, the X axis is the % change in mortality of the given species, and the Y
               axis details the % percentage change in each species in comparison to the current fishing scenario.
               As this is a fairly cluttered plot, it can be simplified by clicking on the species that you would 
               like to remove on the figure legend."
          )
        )
      } else if (input$breakpoint_tabpanel == "Biomass") {
        intro_steps <- list(
          list(element = "#breakspecies", title = "Biomass Slider", 
               intro = "The biomass slider here works in the same way as the biomass
                 slider in the Single Species section of the app. The only difference is now
                 you choose a range of biomass values to investigate."
          ),
          list(element = "#breakyear", title = "Year", 
               intro = "Similar to as seen prior, this slider chooses the year that you want to plot."
          ),
          list(element = "#breaknumber_species", title = "Break Number", 
               intro = "The number chosen here is the amount of simulations that you want to run
                 between the range of biomass values chosen on the slider. Each value of changed biomass
                 is equidistant to others."),
          list(element = "#breakplotting .nav-link[data-value='Scrollable Species']", title = "Scrollable Plot", 
               intro = "This is the first plot that you will see. It is the same as the species plots
               found in the Single Species section of the app, except that there is one plot for each
               simulation that you have ran, and these plots are placed on top of each other, so that it
               is possible to scroll down and observe the change."
          ),
          list(element = "#breakplotting .nav-link[data-value='Line Graph']", title = "Line Breaks", 
               intro = "The next plot is of the same information, but plotted in a different format.
               This time, the X axis is the % change in starting biomass of the given species, and the Y
               axis details the % percentage change in each species in comparison to the current fishing scenario.
               As this is a fairly cluttered plot, it can be simplified by clicking on the species that you would 
               like to remove on the figure legend."
          )
        )
      }
    } else if (input$bigtabpanel == "Fishery Strategy") {
      
      intro_steps <- list(
        list(element = "#fishyyear", title = "Year Range to Plot", 
             intro = "Choose a year within the simulation to plot, this can be across a range."
        ),
        list(element = "#fishery_sliders", title = "Fishery Sliders", 
             intro = "The sliders here change the fishing effort of each fishery. Each of these fisheries
             attempt to model fishing fleets with common characteristics. The fishing strategies
             seen here are considered the current fishing strategies (0/1/1/1, respectively). Any values
             higher than this increase the fishing effort imposed by the current fishery and any values lower
             is the converse."
        ),
        list(element = "#fishy_choose", title = "Choose Fish to Plot", 
             intro = "This choice is only relevant to the Single Diet plot, and changes the species
             that is plotted."),
        list(element = "#select_species_fishy", title = "Order of Species", 
             intro = "This choice changes the way that the species is ordered on the X axis 
             in certain plots."),
        list(element = "#fishy_plots .nav-link[data-value='Yield']", title = "Yield Plot", 
             intro = "This plot shows the yield over time of each given species"
        ),
        list(element = "#fishy_plots .nav-link[data-value='Species']", title = "Species Plot", 
             intro = "This plot shows the relative species biomass of each species (to the current fishing 
             scenario), at three timescales (the middle bar is the chosen time range, the lowest is 1/2 of this
             value, the highest is 2x this value)"
        ),
        list(element = "#fishy_plots .nav-link[data-value='Size']", title = "Relative Size Spectrum Plot", 
             intro = "This plot shows the community size spectrum, or the biomass of all fish for that given size,
             normalised to the current fishing scenario"
        ),
        list(element = "#fishy_plots .nav-link[data-value='Guild']", title = "Guild Plot", 
             intro = "This plot shows the biomass change in each separate feeding guild in comparison 
             to the current fishing scenario. A feeding guild is certain sizes and species of fish who feed on
             similar prey items. "
        ),
        list(element = "#fishy_plots .nav-link[data-value='Diet']", title = "Diet Plot", 
             intro = "This plot shows the proportion of the diet that a given prey item is making up in 
             a given predator. The colour denotes the difference in comparison to the current fishing scenario"
        ),
        list(element = "#fishy_plots .nav-link[data-value='Spectra']", title = "Size Spectrum Plot", 
             intro = "This plot is the size spectrum (the biomass distribution across sizes of fish) of each
             species of fish in the model"
        ),
        list(element = "#fishy_plots .nav-link[data-value='Single Diet']", title = "Single Diet Plot", 
             intro = "This plot shows the proportion of prey items in a predators diet and how it changes
             as they reach larger size classes."
        ),
        list(element = "#fishy_plots .nav-link[data-value='Biomass']", title = "Biomass Plot", 
             intro = "This plot shows the biomass of each species across time."
        )
        
      )
      
    }
    
    if (length(intro_steps) > 0) {
      introjs(session, options = list(steps = intro_steps))
    }
  })
  
  
  
  #loading in the model 
  celticsim <- readRDS("Lauria1.rds")
  
  
  #changing the species options to dynamically change depending on the model
  
  species_list <- reactive({
    setdiff(unique(celticsim@species_params$species), 
            c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
  })
  
  species_input_ids <- c("species_name_select", "name_select",
                         "breakname_select","fish_name_select", 
                         "breakname_select_mort")  
  
  observe({
    lapply(species_input_ids, function(id) {
      updateSelectInput(session, id, choices = species_list())
    })
  })
  
  
  #This section has all the buttons for the years to change
  #Right now, the time range that is plotted is a 3 year period around the year selected.
  #This includes the year chosen, so for example button 3 will plots the years 2,3,4.
  
  
  observeEvent(input$set_year_5, {
    updateSliderInput(session, "year", value = 3)
  })
  observeEvent(input$set_year_15, {
    updateSliderInput(session, "year", value = 6)
  })
  observeEvent(input$set_year_30, {
    updateSliderInput(session, "year", value = 12)
  })
  
  observeEvent(input$fish_set_year_5, {
    updateSliderInput(session, "fishyear", value = 3)
  })
  observeEvent(input$fish_set_year_15, {
    updateSliderInput(session, "fishyear", value = 6)
  })
  observeEvent(input$fish_set_year_30, {
    updateSliderInput(session, "fishyear", value = 12)
  })
  
  observeEvent(input$mortset_year_5, {
    updateSliderInput(session, "mortyear", value = 3)
  })
  observeEvent(input$mortset_year_15, {
    updateSliderInput(session, "mortyear", value = 6)
  })
  observeEvent(input$mortset_year_30, {
    updateSliderInput(session, "mortyear", value = 12)
  })
  
  observeEvent(input$breakset_year_5, {
    updateSliderInput(session, "breakyear", value = 3)
  })
  observeEvent(input$breakset_year_15, {
    updateSliderInput(session, "breakyear", value = 6)
  })
  observeEvent(input$breakset_year_30, {
    updateSliderInput(session, "breakyear", value = 12)
  })
  
  observeEvent(input$breakset_year_5_mort, {
    updateSliderInput(session, "mortbreakyear", value = 3)
  })
  observeEvent(input$breakset_year_15_mort, {
    updateSliderInput(session, "mortbreakyear", value = 6)
  })
  observeEvent(input$breakset_year_30_mort, {
    updateSliderInput(session, "mortbreakyear", value = 12)
  }) 
  
  #here is the unharvested simulation to be used throughout for comparison
  #tmax is 200 because the maximum time to set on the sliders is 100, and 
  #for the species plots with the time range, it needs to be 2 times this amount.
  #it uses fishing strategies of 0,1,1,1 for each fleet.
  
  #load("unharvestedprojection")
  #This doesnt work, because theyre adding a new model means it will need to be changed
  
  unharvestedprojection <- project(celticsim, t_max = 200,
                                   effort = celticsim@initial_effort)
  
  
  #This section contains all the functions to be used
  
  
  #Firstly, the function that plots the relative size spectrum plots between 2 
  #mizersim objects for a given year range - when the mizer sims differ in 
  #the starting biomass of a given species.
  
  #' MizerSim Relative Community Size Spectrum 
  #'
  #' This function plot the relative community size spectrum between two 
  #' mizerSim objects.
  #'
  #' @param object1 A mizerSim object, this is the sim you are comparing.
  #' @param object2 A mizerSim object, this is the sim you are comparing to.
  #'
  #' @return A community size spectrum - values are the relative abundance
  #' at a given size class.
  #'
  #' @examples
  #' # Compare between mizerSim objects differing in fishing strategy.
  #' 
  #' plotSpectraRelative(harvestedprojection, unharvestedprojection)
  #'
  #' @export
  plotSpectraRelative <- function(object1, object2, time1, time2) {
    
    sf1 <- mizer::plotSpectra(object1, return_data = TRUE, 
                              resource = FALSE, background = FALSE,
                              time_range = time1:time2)
    sf2 <- mizer::plotSpectra(object2, return_data = TRUE, 
                              resource = FALSE, background = FALSE,
                              time_range = time1:time2)
    
    sf <- left_join(sf2, sf1, by = c("w", "Legend")) |>
      group_by(w) |>
      summarise(x = sum(value.x, na.rm = TRUE),
                y = sum(value.y, na.rm = TRUE)) |>
      mutate(rel_diff = 2 * (y - x) / (x + y))
    
    sf <- ggplot() +
      geom_line(data = sf, 
                aes(x = w, y = rel_diff * 100), 
                color = "#2FA4E7") +
      geom_hline(yintercept = 0, linetype = 1,
                 colour = "dark grey", linewidth = 0.75) +
      labs(x = "Size (g)", 
           y = "Percentage Change") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    
    return(sf)
  }
  
  
  #' MizerSim Relative Biomass per Species 
  #'
  #' This calculates the percentage difference between the value of biomass 
  #' that are separated by a Species column.
  #'
  #' @param harvested An array (time x species)
  #' @param unharvested An array (time x species), the value you are comparing to.
  #'
  #' @return A dataframe of Species and Biomass. Biomass gives the percentage
  #' difference of the value of biomass between the harvested and unharvested
  #' mizerSim objects.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' percentdiff(harvested, unharvested)
  #'
  #' @export
  percentdiff <- function(harvested, unharvested) {
    harvested %>%
      left_join(unharvested, by = "Species") %>%
      mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
  }
  
  
  
  #This function plots the species plot - which the change in species for a given 
  #year, and also for 2x in future and 1/3 year in the past.
  
  
  #' Plot MizerSim Relative Biomass per Species Across Varying Timescales 
  #'
  #' This function takes two mizerSim objects and calculates the relative % 
  #' change in each given species in the chosen year, short term (1/3 of the 
  #' chosen year) and the long term (2x the chosen year) 
  #'
  #' @param harvested A mizerSim object
  #' @param unharvested A mizerSim object - to compare to.
  #' @param chosentime The year to plot 
  #'
  #' @return A ggplot object that plots 3 bars per species - in the short, 
  #' chosen and long time - it plots the relative biomass of each species in
  #' comparison to the unharvested.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' percentdiff(harvested, unharvested)
  #'
  #' @export
  plotSpeciesWithTimeRange <- function(harvestedprojection, unharvestedprojection, chosentime1, chosentime2) {
    
    #get the biomass of the species
    unharvestedbio <- getBiomass(unharvestedprojection) %>%
      .[chosentime1:chosentime2, ] %>% 
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))
    
    harvestedbio <- getBiomass(harvestedprojection) %>%
      .[chosentime1:chosentime2, ] %>% 
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))
    
    #calculate percentage change in species in the chosen year
    percentage_diff <-  harvestedbio %>%
      left_join(unharvestedbio, by = "sp") %>%
      mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
             Species = sp) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
      mutate(class = "chosen")
    
    calculate_biomass_triples <- function(unharvestedprojection, harvestedprojection, year1, year2) {
      
      # Calculate unharvested biomass at different time points
      unharvestedbiotriple <- getBiomass(unharvestedprojection)
      
      #the range has to be 1-2, becuase if it is 1-1 it messes with the way the data is formatted.
      lowunbiotrip <- unharvestedbiotriple[max(1, round(year1 * (1/2))):max(2, round(year2 * (1/2))), ] %>%
        melt() %>%
        group_by(sp) %>%
        summarize(value = mean(value, na.rm = TRUE))
      
      highunbiotrip <- unharvestedbiotriple[(year1 * 2):(year2 * 2), ] %>%
        melt() %>%
        group_by(sp) %>%
        summarize(value = mean(value, na.rm = TRUE))
      
      # Calculate harvested biomass at different time points
      harvestedbiotriple <- getBiomass(harvestedprojection)
      
      lowbiotrip <- harvestedbiotriple[max(1, round(year1 * (1/2))):max(2, round(year2 * (1/2))),] %>%
        melt() %>%
        group_by(sp) %>%
        summarize(value = mean(value, na.rm = TRUE))
      
      highbiotrip <- harvestedbiotriple[(year1 * 2):(year2 * 2), ] %>%
        melt() %>%
        group_by(sp) %>%
        summarize(value = mean(value, na.rm = TRUE))
      
      # Return the results as a list
      list(
        lowunbiotrip,
        highunbiotrip,
        lowbiotrip,
        highbiotrip
      )
    }
    
    #calculate percentage change in other years
    biorange <- calculate_biomass_triples(unharvestedprojection, harvestedprojection, chosentime1, chosentime2)
    
    #percentage_difflow <- percentdiff(biorange[[3]], biorange[[1]])
    #percentage_difflow$class <- "short"
    
    percentage_difflow <-  biorange[[3]] %>%
      left_join(biorange[[1]], by = "sp") %>%
      mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
             Species = sp) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
      mutate(class = "short")
    
    #percentage_diffhigh <- percentdiff(biorange[[4]], biorange[[2]])
    #percentage_diffhigh$class <- "long"
    
    percentage_diffhigh <-  biorange[[4]] %>%
      left_join(biorange[[2]], by = "sp") %>%
      mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
             Species = sp) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
      mutate(class = "long")
    
    percentage_diff <- rbind(percentage_difflow, percentage_diff, percentage_diffhigh)
    
    #now plot them together - the first lines sort out the colors of the bars
    percentage_diff$class <- factor(percentage_diff$class, levels = c("short", "chosen", "long"))
    percentage_diff$fill_group <- interaction(percentage_diff$percentage_diff >= 0, percentage_diff$class)
    
    ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = fill_group)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5)+
      labs(x = "Species", y = "Percentage Change") +
      scale_fill_manual(values = c(
        "FALSE.short" = "#E76F51",  
        "FALSE.chosen" = "#E98C6B",  
        "FALSE.long" = "#F2A488",   
        "TRUE.short" = "#2FA4E7", 
        "TRUE.chosen" = "#2FA4E7cc",
        "TRUE.long" = "#2FA4E799" 
      )) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
  }
  
  
  #THis function plots the the yields as a pie chart of the species / fleet aggregations
  #I should have a button underneath this to change it to plot on fleet basis - on species basis - separated fleet
  
  yieldplottingfunctions <- function(harvested, unharvested, timerange1, timerange2, plottype) {
    
    if (plottype == "fleet") {
      
      #getting yield per gear then reformating to work with rest of the code,
      #plus subsetting by year
      
      harv <- as.data.frame(as.table(getYieldGear(harvested)[c(timerange1, timerange2),,]))%>%
        group_by(gear)%>%
        summarise(value=mean(Freq))%>%
        subset(value>0)
      
      unharv <-as.data.frame(as.table(getYieldGear(unharvested)[c(timerange1, timerange2),,]))%>%
        group_by(gear)%>%
        summarise(value=mean(Freq))%>%
        subset(value>0)
      
      fig <- plot_ly()
      fig <- fig %>% add_pie(data = harv, labels = ~gear, values = ~value,
                             name = "harv", domain = list(row = 0, column = 0),
                             title = "Changed Strategy Yield")
      fig <- fig %>% add_pie(data = unharv, labels = ~gear, values = ~value,
                             name = "unharv", domain = list(row = 0, column = 1),
                             title = "Current Strategy Yield")
      
      fig <- fig %>% layout(showlegend = T,
                            grid = list(rows = 1, columns = 2),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig
      
    } else if (plottype == "species") {
      
      harv <- as.data.frame(as.table(getYield(harvested)[c(timerange1, timerange2),]))%>%
        group_by(sp)%>%
        summarise(value=mean(Freq))%>%
        rename(gear = sp)%>%
        subset(value>0)
      
      unharv <- as.data.frame(as.table(getYield(unharvested)[c(timerange1, timerange2),]))%>%
        group_by(sp)%>%
        summarise(value=mean(Freq))%>%
        rename(gear = sp)%>%
        subset(value>0)
      
      fig <- plot_ly()
      fig <- fig %>% add_pie(data = harv, labels = ~gear, values = ~value,
                             name = "harv", domain = list(row = 0, column = 0),
                             title = "Changed Strategy Yield")
      fig <- fig %>% add_pie(data = unharv, labels = ~gear, values = ~value,
                             name = "unharv", domain = list(row = 0, column = 1),
                             title = "Current Strategy Yield")
      
      
      fig <- fig %>% layout(showlegend = T,
                            grid = list(rows = 1, columns = 2),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(fig)
      
    } else if (plottype == "singlefleet") {
      
      harv <- as.data.frame(as.table(getYieldGear(harvested)[c(timerange1, timerange2),,]))
      unharv <-as.data.frame(as.table(getYieldGear(unharvested)[c(timerange1, timerange2),,]))
      
      harv <- split(harv, harv$gear)
      unharv <- split(unharv, unharv$gear)
      
      fig <- plot_ly()
      
      for (i in seq_along(harv)) {
        
        gear_name <- names(harv)[i] 
        
        averaged_data <- harv[[gear_name]] %>%
          group_by(sp) %>%
          summarise(value = mean(Freq))
        colnames(averaged_data) <- c("gear", "value")
        
        fig <- fig %>% add_pie(data = averaged_data, labels = ~gear, values = ~value,
                               title = list(text = paste("Changed", gear_name)),
                               textinfo = "none",
                               name = gear_name, domain = list(row = 0, column = i))
        
      }
      for (i in seq_along(unharv)) {
        
        gear_name <- names(unharv)[i] 
        
        averaged_data <- unharv[[gear_name]] %>%
          group_by(sp) %>%
          summarise(value = mean(Freq))
        colnames(averaged_data) <- c("gear", "value")
        
        
        fig <- fig %>% add_pie(data = averaged_data, labels = ~gear, values = ~value,
                               title = gear_name,
                               textinfo = "none",
                               name = gear_name, domain = list(row = 1, column = i))
        
      }
      
      fig <- fig %>% layout(showlegend = T,
                            grid=list(rows=2, columns=length(harv)),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig
      
    } else {
      
      stop("Invalid plot type specified")
    }
    
  }
  
  
  
  #' Plot Guild Relative Change Across Timescales  
  #'
  #' This function takes two mizerSim objects and calculates the relative % 
  #' change in each given feeding guilde in the chosen year, short term (1/3 of the 
  #' chosen year) and the long term (2x the chosen year) 
  #'
  #' @param harvested A mizerSim object
  #' @param unharvested A mizerSim object - to compare to.
  #' @param chosentime The year to plot 
  #'
  #' @return A ggplot object that plots 3 bars per species - in the short, 
  #' chosen and long time - it plots the relative biomass of each feeding guild 
  #' in comparison to the unharvested.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' guildplot(harvested, unharvested, 5)
  #'
  #' @export
  guildplot <- function(harvestedprojection, unharvestedprojection, year1, year2) {
    #remember to add the rule about the smallest sizes being plantivores.
    
    harvestedshort <- plotSpectra(harvestedprojection, time_range = max(1, round(year1 * (1/2))):max(1, round(year2 * (1/2))), return_data = TRUE)
    harvested <- plotSpectra(harvestedprojection, time_range = year1:year2, return_data = TRUE)
    harvestedlong <- plotSpectra(harvestedprojection, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
    
    unharvestedshort <- plotSpectra(unharvestedprojection, time_range = max(1, round(year1 * (1/2))):max(1, round(year2 * (1/2))), return_data = TRUE)
    unharvested <- plotSpectra(unharvestedprojection, time_range = year1:year2, return_data = TRUE)
    unharvestedlong <- plotSpectra(unharvestedprojection, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
    
    process_guilds <- function(mizerprojection) {
      
      
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
      
      
      mizerprojection <- mizerprojection %>%
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
      
      return(mizerprojection)
      
    }
    
    #for the harvested - 
    guildsshort <- process_guilds(harvestedshort)
    guilds <- process_guilds(harvested)
    guildslong <- process_guilds(harvestedlong)
    #for the unharvested - 
    unguildsshort <- process_guilds(unharvestedshort)
    unguilds <- process_guilds(unharvested)
    unguildslong <- process_guilds(unharvestedlong)
    
    #now joining them together
    guildsshort$time <- "short"
    guilds$time <- "chosen"
    guildslong$time <- "long"
    unguildsshort$time <- "short"
    unguilds$time <- "chosen"
    unguildslong$time <- "long"
    
    joinedguilds <- bind_rows(guildsshort, guilds, guildslong) %>%
      group_by(Guild, time) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    unjoinedguilds <- bind_rows(unguildsshort, unguilds, unguildslong) %>%
      group_by(Guild, time) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    joinedguilds <- joinedguilds%>%
      full_join(unjoinedguilds, by = c("Guild", "time"),relationship = "many-to-many") %>%
      mutate(percentage_diff = ((value.x-value.y)/value.y))%>%
      select(Guild, time, percentage_diff)
    
    #this sets the colours correctly
    joinedguilds$time <- factor(joinedguilds$time, levels = c("short", "chosen", "long"))
    joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0, joinedguilds$time)
    
    #plotting
    ggplot(joinedguilds, aes(x = Guild, y = percentage_diff, fill = fill_group)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = c(
        "FALSE.short" = "#E76F51",  
        "FALSE.chosen" = "#E98C6B",  
        "FALSE.long" = "#F2A488",   
        "TRUE.short" = "#2FA4E7", 
        "TRUE.chosen" = "#2FA4E7cc",
        "TRUE.long" = "#2FA4E799" 
      )) +
      labs(title = "Percentage Change by Guild", 
           x = "Guild", 
           y = "Percentage Change") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    
  }
  
  
  #This loads and formats in the data for the guilds
  
  guildinfo <- read.table("Guilds information/guild_cleaned.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  fish_names <- read.table("Guilds information/fishinfo.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  guildparams <- celticsim@species_params%>%
    select(species, a, b)%>%
    rename(Common_Name=species)%>%
    #this code has correctly formatted the species params
    inner_join(fish_names, by=c("Common_Name"))%>%
    #we have now joined species params to a table containing the scientific names
    rename(Species=Scientific_Name)%>%
    inner_join(
      guildinfo%>%
        filter(Species %in% fish_names$Scientific_Name), 
      by="Species")%>%
    #we have now joined the rows with the same scientific names - so 
    #we have joined the a and b values to the given species 
    
    #this is converting from length to weight
    mutate(maxw=a*Max.cm^b,
           minw=a*Min.cm^b)%>%
    select(Common_Name, maxw, minw, Feeding.guild)%>%
    rename(Species=Common_Name)
  
  #So what the code does above is take the table of the guild information,
  #from the pilot assessment by Murray Thompson, then you give it a table
  #containing the species common + scientific names, and this is all then 
  #joined together so you have the a and b values next to given species, 
  #so therefore we are able to convert from the length measurements to weight,
  #which can then be used in mizer to filter into the correct guilds.
  
  
  
  #This function plots the diet matrix from the mizersim objects.
  #' Plot Relative Diet Proportion of each Prey/Predator  
  #'
  #' This function takes two mizerSim objects and calculates the relative 
  #' change in the proportion of a given prey species in a predators diet. This 
  #' is done for every prey/predator in the model. 
  #'
  #' @param harvested A mizerSim object
  #' @param unharvested A mizerSim object - to compare to.
  #' @param chosentime The year to plot 
  #'
  #' @return A ggplot object of a matrix of predator species on the X axis, 
  #' prey species on the Y axis. The colour of the box indicates the change 
  #' of the proportion in the predator's diet of the given prey species.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' comparedietmatrix(harvested, unharvested, 5)
  #'
  #' @export
  comparedietmatrix <- function(unharvestedprojection, harvestedprojection, timerange){
    
    #THE TIMERANGE SHOULD BE X:Y
    dietunharv <- getDiet(unharvestedprojection@params, 
                          n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                          n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                          n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                          proportion = TRUE) %>%
      as.table()%>%
      as.data.frame()%>%
      group_by(predator, prey)%>%
      summarise(Proportion=mean(Freq))
    
    dietharv <- getDiet(unharvestedprojection@params, 
                        n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                        n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                        n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                        proportion = TRUE) %>%
      as.table()%>%
      as.data.frame()%>%
      group_by(predator, prey)%>%
      summarise(Proportion=mean(Freq))
    
    joindiet <- left_join(dietharv, dietunharv, by = c("prey", "predator"))%>%
      mutate(Difference = ((Proportion.x - Proportion.y) / Proportion.y) * 100) %>%  # Calculate percentage change
      select(predator, prey, Difference)%>%
      filter(!predator %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"), 
             !prey %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
    
    dietplot <- ggplot(joindiet, aes(x = predator, y = prey, fill = Difference)) +
      geom_tile() +  
      scale_fill_gradient2() +  
      labs(x = "Predator",
           y = "Prey",
           fill = "Difference") +  
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    
    
    return(dietplot)
    
  }
  
  
  #Here is the code that sets the ordering of the species on the plots.
  
  ordered_species <- reactive({
    if (input$species_order == "Alphabetical") {
      # Order species alphabetically
      as.data.frame(celticsim@species_params$species)%>%
        setNames("Species")%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
        pull(Species)
    } else if (input$species_order == "Guild") {
      #THIS WILL NOT WORK AUTOMATICALLY WITH NEW GUILDS/SPECIES, needs to be changed.
      c("Cod","Whiting", "European Hake","Monkfish","Haddock","Common Dab","Poor Cod",
        "Plaice","Megrim","Sole","Blue Whiting","Herring","Sprat","Norway Pout","Horse Mackerel",
        "Mackerel"
      )
    } else if (input$species_order == "Size") {
      #order by maturity size
      species_order <- data.frame(celticsim@species_params$species, celticsim@species_params$w_mat)%>%
        setNames(c("Species", "mat"))%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
        arrange(mat)%>%
        pull(Species)
      species_order
    }
  })
  
  #this code is for the breakpoints tab - the plotting function for the scrollable section
  create_species_level_plot <- function(data, plot_title) {
    ggplot(data, aes(x = Species, y = normalized_value, fill = Species)) +
      geom_bar(stat = "identity", fill="#2FA4E7") +
      labs(title = plot_title, x = "Species", y = "Percentage Change") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    
  }
  
  
  #This section is for the biomass change of species (tab = biomass)
  
  specieschange <- eventReactive(input$goButton1,{
    
    #load the simulation
    speciessim <- celticsim
    
    #set the time points correctly (and make sure it is minimum 1)
    time1 <- max(input$year - 1, 1)
    time2 <- input$year+1
    
    #This is making the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running simulation...", value = 0)
    
    # Define total number of steps to split progress
    total_steps <- 7
    
    
    # Step 1: Initial unharvested projection
    
    #removed, wasnt necessary, it is now loaded prior.
    
    # Step 2: Adjust biomass
    progress$inc(amount = 1 / total_steps, message = "Adjusting biomass...")
    
    speciessim@initial_n[input$species_name_select, ] <- speciessim@initial_n[input$species_name_select, ] * input$species
    
    # Step 3: Run harvested projection
    progress$inc(amount = 1 / total_steps, message = "Running harvested projection...")
    harvestedprojection <- project(speciessim,
                                   effort = celticsim@initial_effort,
                                   t_max = time2*2)
    
    # Step 4: Calculate size spectrum
    progress$inc(amount = 1 / total_steps, message = "Calculating size spectrum...")
    
    #this has been checked - it runs properly with time ranges
    sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Step 5: Calculate species level change
    progress$inc(amount = 1 / total_steps, message = "Calculating species level changes...")
    specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Step 6: Calculate guild level
    progress$inc(amount = 1 / total_steps, message = "Calculating guild level changes...")
    guildlevel <- guildplot(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Final step: Compare diet matrices
    progress$inc(amount = 1 / total_steps, message = "Comparing diet matrices...")
    dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, time1:time2)
    
    progress$inc(amount = 1 / total_steps, message = "Finalising plotting functions...")
    # Return the results
    list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel, dietplot = dietplot)
  })
  
  #Now this next bit of code takes the outputs from the biomass change section 
  #and plots them into the tabs / app
  
  output$speciesPlot <- renderPlotly({
    specieschange()$specieslevel+scale_x_discrete(limits = ordered_species())
  })
  output$sizePlot <- renderPlotly({
    specieschange()$sizelevel
  })
  output$guildPlot <- renderPlotly({
    specieschange()$guildlevel
  })
  output$dietPlot <- renderPlotly({
    specieschange()$dietplot+scale_x_discrete(limits = ordered_species())
  })
  
  
  
  
  #This next section is for the added mortality - everything is the same as 
  #above, except for the first section where the mortality is added
  mortspecieschange <- eventReactive(input$goButton3, {
    
    speciessim <- celticsim
    
    #set the time points correctly (and make sure it is minimum 1)
    time1 <- max(input$mortyear - 1, 1)
    time2 <- input$mortyear * 2
    
    # Initialize the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running simulation...", value = 0)
    
    # Define total number of steps to split progress
    total_steps <- 7
    
    # Step 1: Initial unharvested projection
    progress$inc(amount = 1 / total_steps, message = "Running unharvested projection...")
    
    # Step 2: Changing the mortality
    progress$inc(amount = 1 / total_steps, message = "Updating mortality...")
    extmort <- getExtMort(speciessim)
    totalmort <- getMort(speciessim)
    extmort[input$name_select,] <- extmort[input$name_select,] + (input$mortspecies * totalmort[input$name_select,])
    ext_mort(speciessim) <- extmort
    
    # Step 3: Run harvested projection
    progress$inc(amount = 1 / total_steps, message = "Running harvested projection...")
    harvestedprojection <- project(speciessim,
                                   effort = celticsim@initial_effort,
                                   t_max = time2 * 2)
    
    # Step 4: Plot relative size spectrum
    progress$inc(amount = 1 / total_steps, message = "Plotting relative size spectrum...")
    sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Step 5: Calculate species level change
    progress$inc(amount = 1 / total_steps, message = "Calculating species level changes...")
    specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Step 6: Calculate guild level
    progress$inc(amount = 1 / total_steps, message = "Calculating guild level changes...")
    guildlevel <- guildplot(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Step 7: Compare diet matrices
    progress$inc(amount = 1 / total_steps, message = "Comparing diet matrices...")
    dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, time1:time2)
    
    progress$inc(amount = 1 / total_steps, message = "Finalizing plotting functions...")
    # Return the results
    list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel, dietplot = dietplot)
    
  })
  
  #Plotting the outputs of the mortality decreases
  output$mortspeciesPlot <- renderPlotly({
    mortspecieschange()$specieslevel+scale_x_discrete(limits = ordered_species())
  })
  output$mortsizePlot <- renderPlotly({
    mortspecieschange()$sizelevel
  })
  output$mortguildPlot <- renderPlotly({
    mortspecieschange()$guildlevel
  })
  output$mortdietPlot <- renderPlotly({
    mortspecieschange()$dietplot+scale_x_discrete(limits = ordered_species())
  })
  
  
  #This section details the fishery strategy tab
  
  spectra <- eventReactive(input$goButton2, {
    
    time1 <- max(input$fishyear - 1, 1)
    time2 <- input$fishyear+1
    
    # Initialize progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running simulation...", value = 0)
    
    # Step 1: Running the simulation
    speciessim <- celticsim
    effort <- c(commercial = input$industrial, pelagic = input$pelagic, beam = input$beam, otter = input$otter)
    harvestedprojection <- project(speciessim, effort = effort, t_max = time2 * 2)
    
    # Update progress (20% complete)
    progress$inc(amount = 0.2, message = "Calculating size spectrum...")
    
    # Step 2: Plotting the relative size spectrum
    sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Update progress (40% complete)
    progress$inc(amount = 0.2, message = "Calculating species-level change...")
    
    # Step 3: Calculate species level change
    specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Update progress (55% complete)
    progress$inc(amount = 0.15, message = "Calculating guild-level change...")
    
    # Step 4: Calculate guilds
    guildlevel <- guildplot(harvestedprojection, unharvestedprojection, time1, time2)
    
    # Update progress (70% complete)
    progress$inc(amount = 0.15, message = "Plotting diet matrix...")
    
    # Step 5: Plot diet matrices
    dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, time1:time2)
    
    # Update progress (85% complete)
    progress$inc(amount = 0.15, message = "Plotting biomass and diet...")
    
    # Step 6: Biomass plotting function
    biomass <- plotBiomass(harvestedprojection, start_time = time1, end_time = time2)
    
    # Step 7: Single species diet function
    
    
    # Update progress (100% complete)
    progress$inc(amount = 0.15, message = "Finalizing...")
    
    # Return all results as a list
    list(
      harvestedprojection = harvestedprojection,
      sizelevel = sizelevel,
      specieslevel = specieslevel,
      guildlevel = guildlevel,
      dietplot = dietplot,
      spectrum = (plotSpectra(harvestedprojection, time_range = time1:time2)+theme_minimal()),
      yield = plotlyYield(harvestedprojection),
      biomass = biomass
    )
  })
  
  #this is required to be on its own so that it is able to be changed by fish name select without running everything else.
  dietsingle <- reactive({
    
    time1 <- max(input$fishyear - 1, 1)
    time2 <- input$fishyear+1
    
    harvestedprojection <- spectra()$harvestedprojection
    
    #subsetting to plot the correct year
    harvestedprojection@n <- harvestedprojection@n[time1:time2,,]
    harvestedprojection@n_pp <- harvestedprojection@n_pp[time1:time2,]
    harvestedprojection@n_other <- harvestedprojection@n_other[time1:time2,]
    
    plotDiet(harvestedprojection@params, species = input$fish_name_select)+theme_minimal()
  })
  
  #This is for the different plots of yield.
  output$yieldPlot <- renderPlotly({
    
    time1 <- max(input$fishyear - 1, 1)
    time2 <- input$fishyear+1
    
    if (input$YieldChoice == "Fleet") {
      yieldplottingfunctions(spectra()$harvestedprojection, unharvestedprojection, time1, time2, "fleet")
    } else if (input$YieldChoice == "SoloFleet") {
      yieldplottingfunctions(spectra()$harvestedprojection, unharvestedprojection, time1, time2, "singlefleet")
    } else if (input$YieldChoice == "Species") {
      yieldplottingfunctions(spectra()$harvestedprojection, unharvestedprojection, time1, time2, "species")
    }
  })
  
  #this is just plotting the outputs of spectra
  output$spectrumPlot <- renderPlotly({
    spectra()$spectrum
  })
  output$fishspeciesPlot <- renderPlotly({
    spectra()$specieslevel+scale_x_discrete(limits = ordered_species())
  })
  output$fishsizePlot <- renderPlotly({
    spectra()$sizelevel
  })
  output$fishguildPlot <- renderPlotly({
    spectra()$guildlevel
  })
  output$fishdietPlot <- renderPlotly({
    spectra()$dietplot+scale_x_discrete(limits = ordered_species())
  })
  output$fishbiomassPlot <- renderPlotly({
    spectra()$biomass
  })
  output$fishdietsinglePlot <- renderPlotly({
    dietsingle()
  })
  
  
  
  #Plotting breakpoints for the mortality increases
  #this generates the values of biomass/mortality change
  breaks <- eventReactive(input$goButton,{
    
    
    if (input$breakpoint_tabpanel == "Mortality") {
      
      time1 <- max(input$mortbreakyear - 1, 1)
      time2 <- input$mortbreakyear+1
      
      breaks_seq <- seq(input$breakrange[1], input$breakrange[2], 
                        by = (input$breakrange[2] - input$breakrange[1]) / (input$breaknumber - 1))
      databreak <- data.frame(mort = breaks_seq,  
                              time1 = time1, length(breaks_seq), 
                              time2 = time2, length(breaks_seq),
                              sim = seq_len(length(breaks_seq)))
      return(databreak)
    } else if (input$breakpoint_tabpanel == "Biomass") {
      
      time1 <- max(input$breakyear - 1, 1)
      time2 <- input$breakyear+1
      
      breaks_seq <- seq(input$breakspecies[1], input$breakspecies[2], 
                        by = (input$breakspecies[2] - input$breakspecies[1]) / (input$breaknumber - 1))
      databreak <- data.frame(mort = breaks_seq, 
                              time1 = time1,
                              time2 = time2, 
                              sim = seq_len(length(breaks_seq)))
      
      return(databreak)
    }
    
  })
  
  
  #this runs teh simulations with the changed parameters
  breaksim <- eventReactive(input$goButton,{
    
    breakpoints <- breaks()
    breaksim <- data.frame()
    
    celticsim <- readRDS("Celtic_16_untuned.rds")
    speciessim <- celticsim
    
    for (i in 1:nrow(breakpoints)) {
      speciessim <- celticsim
      
      if (input$breakpoint_tabpanel == "Mortality") {
        
        test <- getExtMort(speciessim)
        totalmort <- getMort(speciessim)
        test[input$breakname_select_mort, ] <- test[input$breakname_select_mort, ] + (breakpoints$mort[i] * totalmort[input$breakname_select_mort, ])
        ext_mort(speciessim) <- test
      } else if (input$breakpoint_tabpanel == "Biomass") {
        speciessim@initial_n[input$breakname_select, ] <- speciessim@initial_n[input$breakname_select, ] * breakpoints$mort[i]
      }
      
      harvestedprojection <- project(speciessim,
                                     effort = celticsim@initial_effort,
                                     t_max = breakpoints$time2[1])
      
      harvestedbio <- getBiomass(harvestedprojection)
      harvestedbio <- melt(harvestedbio[breakpoints$time1[1]:breakpoints$time2[1],])%>%
        group_by(sp)%>%
        summarise(value=mean(value))%>%
        ungroup%>%
        rename(Species=sp)
      
      harvestedbio$sim <- i
      
      breaksim <- rbind(breaksim, harvestedbio)
    }
    return(breaksim)
  })
  
  
  #this takes the sims and normalises to the unharvested!
  generateNormalizedData <- function() {
    
    sims <- breaksim()
    breakpoints <- breaks()
    
    unharvestedprojection <- project(celticsim,
                                     effort = celticsim@initial_effort,
                                     t_max = breakpoints$time2[1])
    
    unharvestedbio <- getBiomass(unharvestedprojection)
    
    unharvestedbio <- melt(unharvestedbio[breakpoints$time1[1]:breakpoints$time2[1],])%>%
      group_by(sp)%>%
      summarise(value = mean(value))%>%
      ungroup%>%
      rename(Species=sp)
    
    normalized_data <- sims %>%
      inner_join(unharvestedbio, by = "Species") %>%
      mutate(normalized_value = ((value.x / value.y) - 1) * 100) %>%
      select(Species, normalized_value, sim) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
    
    return(normalized_data)
  }
  
  
  #This takes the values and creates the plot and the dynamic ui - for the scrollable species tab
  # I cant find a way that works without repeating the code like this. I am sure its possible, but..
  
  generateSimulationPlots <- function(normalized_data,  ui_output_id = "plots_breaks") {
    
    data_list <- split(normalized_data, normalized_data$sim)
    
    num_plots <- length(data_list)
    lapply(1:num_plots, function(i) {
      local({
        my_i <- i
        plotname <- paste("plot_breaks", my_i, sep = "")
        output[[plotname]] <- renderPlotly({
          current_data <- data_list[[my_i]]
          mort_value <- breaks()$mort[my_i] * 100
          formatted_mort_value <- sprintf("%.2f%%", mort_value)
          plot_title <- paste(formatted_mort_value)
          create_species_level_plot(current_data, plot_title)
        })
      })
    })
    
    
    lapply(1:num_plots, function(i) {
      local({
        my_i <- i
        plotname <- paste("plot_biomass", my_i, sep = "")
        output[[plotname]] <- renderPlotly({
          current_data <- data_list[[my_i]]
          mort_value <- breaks()$mort[my_i] * 100
          formatted_mort_value <- sprintf("%.2f%%", mort_value)
          plot_title <- paste(formatted_mort_value)
          create_species_level_plot(current_data, plot_title)
        })
      })
    })
    
    output[["plots_breaks"]] <- renderUI({
      plot_output_list <- lapply(1:num_plots, function(i) {
        plotname <- paste("plot_breaks", i, sep = "")
        plotlyOutput(plotname, height = "400px")
      })
      do.call(tagList, plot_output_list)
    })
    
    output[["plots_breaks_biomass"]] <- renderUI({
      plot_output_list <- lapply(1:num_plots, function(i) {
        plotname <- paste("plot_biomass", i, sep = "")
        plotlyOutput(plotname, height = "400px")
      })
      do.call(tagList, plot_output_list)
    })
    #})
  }
  
  
  #this choses where to plot depending on what tab is open
  observeEvent(input$goButton, {
    if (input$breakpoint_tabpanel == "Mortality") {
      generateSimulationPlots(generateNormalizedData(), ui_output_id = "plots_breaks")
    }
  })
  observeEvent(input$goButton, {
    if (input$breakpoint_tabpanel == "Biomass") {
      generateSimulationPlots(generateNormalizedData(), ui_output_id = "plots_breaks_biomass")
    }
  })
  
  
  #now this next section generates the line graph
  linebreak <- reactive({
    generateNormalizedData()
  })
  #this is creating the linebreaks plot tab section
  observeEvent(input$goButton,{
    if (input$breakpoint_tabpanel == "Mortality") {
      output$line_breaks <- renderPlotly({
        data <- generateNormalizedData()
        sims <- breaks()
        data <- data %>%
          left_join(sims, by = "sim")
        
        p <- ggplot(data, aes(x = mort, y = normalized_value, color = Species)) +
          geom_line() +
          geom_point() +
          labs(
            title = "Normalized Value Across Simulations",
            x = "% Mortality Added",
            y = "Normalized Value (%)",
            color = "Species"
          ) +
          theme_minimal()
        
        ggplotly(p)
      })
    } else if (input$breakpoint_tabpanel == "Biomass") {
      output$line_breaks_biomass <- renderPlotly({
        data <- generateNormalizedData()
        sims <- breaks()
        data <- data %>%
          left_join(sims, by = "sim")
        
        p <- ggplot(data, aes(x = mort, y = normalized_value, color = Species)) +
          geom_line() +
          geom_point() +
          labs(
            title = "Normalized Biomass Across Simulations",
            x = "Biomass",
            y = "Normalized Value (%)",
            color = "Species"
          ) +
          theme_minimal()
        
        ggplotly(p)
      })
    }
  })
  
  
}





# NOTE - FOR UI, a code has tagAppendAttributes, which makes it confusing, but it is necessary 
#as you have to label the sections of the code to be able to put it into the tutorial of the app.

ui <- fluidPage(
  #this line enables the tutorial 
  introjsUI(), 
  tags$head(
    #all of this is css settings to make the app look nice
    tags$style(HTML("
      .btn-small {
        padding: 5px 10px;
          font-size: 12px;
        border-radius: 4px;
      }
      .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
        color: #ffffff;
        background-color: #2FA4E7;
        border-color: #2FA4E7 #2FA4E7 #2FA4E7;
      }
      .nav-tabs .nav-link {
        color: #2FA4E7;
        border: 1px solid transparent;
        border-top-left-radius: .25rem;
        border-top-right-radius: .25rem;
      }
      .nav-tabs .nav-link:hover {
        border-color: #e9ecef #e9ecef #ddd;
        color: #0056b3;
      }
      .card {
        border: 2px solid #2FA4E7;
        border-radius: .5rem;
      }
      .nav-tabs {
        margin-bottom: 30px;
      }
      .plots-container {
        padding-top: 30px;
      }
      .btn-info {
       padding: 2px 4px;
      background-image: linear-gradient(rgb(47, 164, 231), rgb(47, 164, 231) 60%, rgb(47, 164, 231));
      }
      #infoButtonSpecies {
        background-color: #2FA4E7 !important;
        color: white; 
        border: none; 
        font-size: 12px; 
        padding: 0px 7px; 
        border-radius: 10px;
        margin-left: 8px;
        margin-bottom: 4px;
        margin-top: 2px;
      }
      #infoButtonSpecies:hover {
        background-color: #2FA4E7; 
      }
        #infoButtonMort {
        background-color: #2FA4E7 !important;
        color: white; 
        border: none; 
        font-size: 12px; 
        padding: 0px 7px; 
        border-radius: 10px;
        margin-left: 8px;
        margin-bottom: 4px;
        margin-top: 2px;
      }
      #infoButtonMort:hover {
        background-color: #2FA4E7; 
      }
        #infoButtonOrder {
        background-color: #2FA4E7 !important;
        color: white; 
        border: none; 
        font-size: 12px; 
        padding: 0px 7px; 
        border-radius: 10px;
        margin-left: 8px;
        margin-bottom: 4px;
        margin-top: 2px;
        }
      .btn-primary {
    background-image: linear-gradient(#2FA4E7, #2fa4e7 60%, #2FA4E7);
}
    ")),
    #OKAY, IF THE CONTENT OF THE BUTTON CONTAINS ANY . / () , IT WONT WORK!!. (it just started working, if it stops again, try do the same without anything in content:)
    tags$script(HTML("
  // Function to initialize popovers for all elements with data-toggle='popover'
  function initializePopovers() {
    // Initialize all popovers based on the data-toggle attribute
    $('[data-toggle=\"popover\"]').each(function() {
      $(this).popover({
        title: $(this).data('title') || '',
        content: $(this).data('content') || '',
        placement: 'right',
        trigger: 'hover'
      });
    });
  }

  // Initialize popovers on initial page load
  $(document).on('shiny:connected', function() {
    initializePopovers(); // Initialize popovers when Shiny is connected
  });

  // Reinitialize popovers when new UI is rendered or tabs are changed
  $(document).on('shiny:value', function(event) {
    initializePopovers();
  });

  // Ensure popovers are initialized when switching between tabs
  $(document).on('shown.bs.tab', function (e) {
    initializePopovers();
  });
"))
  ),
  page_navbar(
    id="bigtabpanel",
    title = tagList(
      img(src = "mizer.png", height = "75px", style="vertical-align: middle; margin-right: 15px; margin-bottom: 5px; margin-top: 5px;"), 
      "Celtic Sea Mizer Model"
    ),
    selected = "Single Species",
    collapsible = TRUE,
    theme = bs_theme(bootswatch="cerulean"),
    
    tabPanel(
      title = "Single Species",
      tabsetPanel(
        id="mortnsp_tab",
        selected = "Biomass",
        tabPanel(
          title = "Biomass",
          grid_container(
            layout = c(
              "area1 area0"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.3fr",
              "1.7fr"
            ),
            gap_size = "10px",
            
            # Sidebar for Biomass
            grid_card(
              area = "area1",
              card_body(
                sliderInput(
                  inputId = "species",
                  label = HTML("Starting Biomass <button id='infoButtonSpecies' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' 
                               data-toggle='popover' data-title='' data-content='Slider value indicates the starting biomass of the species. Example: to
                               increase the starting population of a given species by 20%, set value on the slider to 1.2. To decrease by 20%, set value to 0.8.'><strong>?</strong></button>"),
                  min = 0,
                  max = 2,
                  value = 1,
                  step = 0.01,
                  width = "100%"
                )%>%tagAppendAttributes(id = "species_slider"),
                sliderInput(
                  inputId = "year",
                  label = "Time Range",
                  min = 1,
                  max = 100,
                  value = 5,
                  step = 1,
                  width = "100%"
                )%>%tagAppendAttributes(id = "yearspecies_slider"),
                selectInput(
                  inputId = "species_name_select",
                  label = "Select a Species:",
                  choices = NULL
                )%>%tagAppendAttributes(id = "species_chose"),
                actionButton(inputId = "set_year_5", label = "3 Years", class = "btn-small"),
                actionButton(inputId = "set_year_15", label = "6 Years", class = "btn-small"),
                actionButton(inputId = "set_year_30", label = "12 Years", class = "btn-small"),
                actionButton(inputId = "goButton1", label = "Run Simulation")
              )
            ),
            grid_card(
              area = "area0",
              card_body(
                tabsetPanel(
                  id = "plotTabs",  
                  tabPanel(title = "Species", plotlyOutput("speciesPlot")),
                  tabPanel(title = "Size", plotlyOutput("sizePlot")),
                  tabPanel(title = "Guilds", plotlyOutput("guildPlot")),
                  tabPanel(title = "Diet", plotlyOutput("dietPlot"))
                )
              ),
              card_body(
                # Figure legend for the "Species" tab
                conditionalPanel(
                  condition = "input.plotTabs == 'Species'",
                  h4("Legend"),
                  p("Change in biomass of each species across a time range.
                    The X axis on this plot shows the species, and the Y axis
                    is the percentage change in the biomass of the given species. This is 
                    in comparison to a identical mizer simulation, except without
                    the changed starting biomass.    
                    Each species is separated into three bars, the middle bar is the chosen 
                    time range as seen in the slider, the left bar is 1/2 of this value, and the right
                    bar is 2x this value. The colour of the bar (red/blue) indicates whether the change is positive
                    or negative, the shade of the colour indicates the time range that it represents, short/chosen/long = 
                    dark to light shading.
                    "),
                  br(),
                  h4(HTML("Species Order on Axis <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Select how you want the species to be ordered on the axis. Options include Alphabetical, Size, and Guild.'><strong>?</strong></button>")),
                  selectInput(
                    inputId = "species_order",
                    label = NULL,
                    choices = c("Alphabetical","Size","Guild")
                  )
                ),
                
                # Figure legend for the "Size" tab
                conditionalPanel(
                  condition = "input.plotTabs == 'Size'",
                  h4("Legend"),
                  p("Change in the community size spectrum in comparison to the unchanged scenario.
                  The community size spectrum is the biomass of all species in the ecosystem at that given size.
                  The blue line indicates this changed community size spectrum, the grey line indicates
                    the unchanged community size spectrum at the given time.
                    ")
                ),
                
                # Figure legend for the "Guilds" tab
                conditionalPanel(
                  condition = "input.plotTabs == 'Guilds'",
                  h4("Legend"),
                  p("Change in feeding guilds across the entire community in comparison to 
                  the unchanged model. Each three of the bars indicates a different time range;
                  left/darkest is 1/2 of the chosen time, middle is the chosen time, brighest is 2x
                  the chosen time.
                    Feeding guilds are groupings of fish based on diet and life stage. ")
                ),
                
                # Figure legend for the "Diet" tab
                conditionalPanel(
                  condition = "input.plotTabs == 'Diet'",
                  h4("Legend"),
                  p("Matrix which details the change in proportion of every species in the diet 
                    of a given predator. The colour indicates whether the proportion of the given 
                    species has decreased. Blue is increase, red is decrease. It is possible to change
                    the order of the species shown on the X axis by using the configuration panel."),
                  br(),
                  h4(HTML("Species Order on Axis <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Select how you want the species to be ordered on the axis. Options include Alphabetical, Size, and Guild.'><strong>?</strong></button>")),
                  selectInput(
                    inputId = "species_order",
                    label = NULL,
                    choices = c("Alphabetical","Size","Guild")
                  )
                )
              )
            )
          )
        ),
        
        #div(id="mort_panel",
        tabPanel("Mortality",
                 grid_container(
                   layout = c(
                     "area1 area0"
                   ),
                   row_sizes = c(
                     "1fr"
                   ),
                   col_sizes = c(
                     "0.3fr",
                     "1.7fr"
                   ),
                   gap_size = "10px",
                   grid_card(
                     area = "area1",
                     card_body(
                       sliderInput(
                         inputId = "mortspecies",
                         label = HTML("Mortality Change <button id='infoButtonMort' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Slider value indicates the change in mortality of a species. Example: to increase the mortality of a species by 1%, set the value of the slider to 0.01. This will change the mortality throughout the simulation to be 1% higher. If you want it to be a 1% decrease, set value to -0.01'><strong>?</strong></button>"),
                         min = -0.5,
                         max = 0.5,
                         value = 0,
                         step = 0.01,
                         width = "100%"
                       )%>%tagAppendAttributes(id = "mort_slider"), 
                       sliderInput(
                         inputId = "mortyear",
                         label = "Time Range",
                         min = 1,
                         max = 100,
                         value = 5,
                         step = 1,
                         width = "100%"
                       )%>%tagAppendAttributes(id="yearspecies_slider_mort"),
                       selectInput(
                         inputId = "name_select",
                         label = "Select a Species:",
                         choices = NULL
                       )%>%tagAppendAttributes(id="species_choose_mort"),
                       actionButton(inputId = "mortset_year_5", label = "3 Years", class = "btn-small"),
                       actionButton(inputId = "mortset_year_15", label = "6 Years", class = "btn-small"),
                       actionButton(inputId = "mortset_year_30", label = "12 Years", class = "btn-small"),
                       actionButton(inputId = "goButton3", label = "Run Simulation")
                     )
                   ),
                   
                   # Main Panel for Mortality
                   grid_card(
                     area = "area0",
                     card_body(
                       tabsetPanel(
                         id = "plotTabs_mort",  # Add an ID to reference in the conditional panels
                         tabPanel(title = "Species", plotlyOutput("mortspeciesPlot")),
                         tabPanel(title = "Size", plotlyOutput("mortsizePlot")),
                         tabPanel(title = "Guilds", plotlyOutput("mortguildPlot")),
                         tabPanel(title = "Diet", plotlyOutput("mortdietPlot"))
                       )
                     ),
                     card_body(
                       # Figure legend for the "Species" tab
                       conditionalPanel(
                         condition = "input.plotTabs_mort == 'Species'",
                         h4("Legend"),
                         p("Change in biomass of each species across a time range.
                    The X axis on this plot shows the species, and the Y axis
                    is the percentage change in the biomass of the given species. This is 
                    in comparison to a identical mizer simulation, except without
                    the changed mortality of the given species.    
                    Each species is separated into three bars, the middle bar is the chosen 
                    time range as seen in the slider, the left bar is 1/2 of this value, and the right
                    bar is 2x this value. The colour of the bar (red/blue) indicates whether the change is positive
                    or negative, the shade of the colour indicates the time range that it represents, short/chosen/long = 
                    dark to light shading."),
                         br(),
                         h4(HTML("Species Order on Axis <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Select how you want the species to be ordered on the axis. Options include Alphabetical, Size, and Guild.'><strong>?</strong></button>")),
                         selectInput(
                           inputId = "species_order",
                           label = NULL,
                           choices = c("Alphabetical","Size","Guild")
                         )
                       ),
                       
                       # Figure legend for the "Size" tab
                       conditionalPanel(
                         condition = "input.plotTabs_mort == 'Size'",
                         h4("Legend"),
                         p("Change in the community size spectrum in comparison to the unchanged scenario.
                  The community size spectrum is the biomass of all species in the ecosystem at that given size.
                  The blue line indicates this changed community size spectrum, the grey line indicates
                    the unchanged community size spectrum at the given time.")
                       ),
                       
                       # Figure legend for the "Guilds" tab
                       conditionalPanel(
                         condition = "input.plotTabs_mort == 'Guilds'",
                         h4("Legend"),
                         p("Change in feeding guilds across the entire community in comparison to 
                  the unchanged model. Each three of the bars indicates a different time range;
                  left/darkest is 1/2 of the chosen time, middle is the chosen time, brighest is 2x
                  the chosen time.
                    Feeding guilds are groupings of fish based on diet and life stage.")
                       ),
                       
                       # Figure legend for the "Diet" tab
                       conditionalPanel(
                         condition = "input.plotTabs_mort == 'Diet'",
                         h4("Legend"),
                         p("Matrix which details the change in proportion of every species in the diet 
                    of a given predator. The colour indicates whether the proportion of the given 
                    species has decreased. Blue is increase, red is decrease. It is possible to change
                    the order of the species shown on the X axis by using the configuration panel."),
                         br(),
                         h4(HTML("Species Order on Axis <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Select how you want the species to be ordered on the axis. Options include Alphabetical, Size, and Guild.'><strong>?</strong></button>")),
                         selectInput(
                           inputId = "species_order",
                           label = NULL,
                           choices = c("Alphabetical","Size","Guild")
                         )
                       )
                     )
                   )
                 )
        )%>%tagAppendAttributes(id = "mort_panel")
        #)
      )
      
    ),
    tabPanel(
      title="Breakpoint",
      tabsetPanel(
        id="breakpoint_tabpanel",
        selected="Biomass",
        tabPanel(
          title = "Biomass",
          value = "Biomass",
          grid_container(
            layout = c(
              "area1 area0"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.3fr",
              "1.7fr"
            ),
            gap_size = "10px",
            
            grid_card(
              area = "area1",
              card_body(
                sliderInput(
                  inputId = "breakspecies",
                  label = HTML("Starting Biomass <button id='infoButtonSpecies' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-title='' data-content='Slider value indicates the starting biomass of the species for breakpoints. Example: to increase the starting population of a given species by 20%, set value on the slider to 1.2. To decrease by 20%, set value to 0.8.'><strong>?</strong></button>"),
                  min = 0,
                  max = 2,
                  value = c(1,1),
                  step = 0.01,
                  width = "100%"
                )%>%tagAppendAttributes(id = "breakspecies"),
                sliderInput(
                  inputId = "breakyear",
                  label = "Year to Analyse",
                  min = 0,
                  max = 100,
                  value = 1,
                  step = 1,
                  width = "100%"
                )%>%tagAppendAttributes(id = "breakyear"),
                numericInput(
                  inputId = "breaknumber_species",
                  label = "Number of Simulations",
                  value = 10
                )%>%tagAppendAttributes(id = "breaknumber_species"),
                selectInput(
                  inputId = "breakname_select",
                  label = "Select a Species:",
                  choices = NULL
                )%>%tagAppendAttributes(id = "breakname_select"),
                actionButton(inputId = "breakset_year_5", label = "3 Years", class = "btn-small"),
                actionButton(inputId = "breakset_year_15", label = "6 Years", class = "btn-small"),
                actionButton(inputId = "breakset_year_30", label = "12 Years", class = "btn-small"),
                actionButton(inputId = "goButton", label = "Run Simulation")
              )
            ),
            
            # Main Panel for Breakpoint
            grid_card(
              area = "area0",
              card_body(
                tabsetPanel(
                  id="breakplotting",
                  tabPanel(title = "Scrollable Species", uiOutput("plots_breaks_biomass")),
                  tabPanel(title = "Line Graph", plotlyOutput("line_breaks_biomass"))
                )
              ),
              card_body(
                # Figure legend for the "Species" tab
                conditionalPanel(
                  condition = "input.breakplotting == 'Scrollable Species'",
                  h4("Legend"),
                  p("Percentage change in each species across the range of changed biomass values.
                Repeats one plot for every simulation, said plot has the X axis as the 
                species, and the Y axis as the percentage change in the species.")
                ),
                
                # Figure legend for the "Size" tab
                conditionalPanel(
                  condition = "input.breakplotting == 'Line Graph'",
                  h4("Legend"),
                  p("Line graph to show changes in biomass of each species as the starting
                biomass of the given species is changed. The X axis is the value of the starting biomass, 
                the Y axis is the % change in biomass in comparison to the unchanged fishing scenario.
                Each line indicates a different species.")
                )
              )
            )
          )
          #)
        ),
        tabPanel(
          title = "Mortality",
          value = "Mortality",
          grid_container(
            layout = c(
              "area1 area0"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.3fr",
              "1.7fr"
            ),
            gap_size = "10px",
            
            grid_card(
              area = "area1",
              card_body(
                sliderInput(
                  inputId = "breakrange",
                  label = HTML("Mortality Change <button id='infoButtonMort' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Slider value indicates the change in mortality of a species. Example: to increase the mortality of a species by 1%, set the value of the slider to 0.01. This will change the mortality throughout the simulation to be 1% higher. If you want it to be a 1% decrease, set value to -0.01'><strong>?</strong></button>"),
                  min = -0.5,
                  max = 0.5,
                  value = c(0,0),
                  step = 0.01,
                  width = "100%"
                )%>%tagAppendAttributes(id = "breakmort"),
                sliderInput(
                  inputId = "mortbreakyear",
                  label = "Year to Analyse",
                  min = 0,
                  max = 100,
                  value = 1,
                  step = 1,
                  width = "100%"
                )%>%tagAppendAttributes(id = "breakyear"),
                numericInput(
                  inputId = "breaknumber",
                  label = "Number of Simulations",
                  value = 10
                )%>%tagAppendAttributes(id = "breaknumber"),
                selectInput(
                  inputId = "breakname_select_mort",
                  label = "Select a Species:",
                  choices = NULL
                )%>%tagAppendAttributes(id = "breakname_select_mort"),
                actionButton(inputId = "breakset_year_5_mort", label = "3 Years", class = "btn-small"),
                actionButton(inputId = "breakset_year_15_mort", label = "6 Years", class = "btn-small"),
                actionButton(inputId = "breakset_year_30_mort", label = "12 Years", class = "btn-small"),
                actionButton(inputId = "goButton", label = "Run Simulation")
              )
            ),
            
            # Main Panel for Breakpoint
            grid_card(
              area = "area0",
              card_body(
                tabsetPanel(
                  id="breakplotting_mort",
                  tabPanel(title = "Scrollable Species", uiOutput("plots_breaks")),
                  tabPanel(title = "Line Graph", plotlyOutput("line_breaks"))
                )
              ),
              card_body(
                # Figure legend for the "Species" tab
                conditionalPanel(
                  condition = "input.breakplotting_mort == 'Scrollable Species'",
                  h4("Legend"),
                  p("Percentage change in each species across the range of changed mortality values.
                Repeats one plot for every simulation, said plot has the X axis as the 
                species, and the Y axis as the percentage change in the species.")
                ),
                
                # Figure legend for the "Size" tab
                conditionalPanel(
                  condition = "input.breakplotting_mort == 'Line Graph'",
                  h4("Legend"),
                  p("Line graph to show changes in biomass of each species as the 
                mortality of the given species is changed. The X axis is the value of the change in mortality, 
                the Y axis is the % change in biomass in comparison to the unchanged fishing scenario.
                Each line indicates a different species.")
                )
              )
            )
          )
        )
      )
    ),
    # Fishery Strategy Tab
    tabPanel(
      title = "Fishery Strategy",
      grid_container(
        layout = c(
          "area1 area0"
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          "0.3fr",
          "1.7fr"
        ),
        gap_size = "10px",
        
        # Sidebar for Fishery Strategy
        grid_card(
          area = "area1",
          card_body(
            sliderInput(
              inputId = "fishyear",
              label = "Time Range",
              min = 0,
              max = 100,
              value = 5,
              step = 1,
              width = "100%"
            )%>%tagAppendAttributes(id = "fishyyear"),
            div(
              id = "fishery_sliders",
              sliderInput(
                inputId = "industrial",
                label = "Commercial",
                min = 0,
                max = 2,
                value = 0,
                step = 0.1,
                width = "100%"
              ),
              sliderInput(
                inputId = "pelagic",
                label = "Pelagic",
                min = 0,
                max = 2,
                value = 1,
                step = 0.1,
                width = "100%"
              ),
              sliderInput(
                inputId = "beam",
                label = "Beam",
                min = 0,
                max = 2,
                value = 1,
                step = 0.1,
                width = "100%"
              ),
              sliderInput(
                inputId = "otter",
                label = "Otter",
                min = 0,
                max = 2,
                value = 1,
                step = 0.1,
                width = "100%"
              )),
            actionButton(inputId = "fish_set_year_5", label = "3 Years", class = "btn-small"),
            actionButton(inputId = "fish_set_year_15", label = "6 Years", class = "btn-small"),
            actionButton(inputId = "fish_set_year_30", label = "12 Years", class = "btn-small"),
            actionButton(inputId = "goButton2", label = "Run Simulation")
          )
          
        ),
        
        # Main Panel for Fishery Strategy
        grid_card(area = "area0",
                  card_body(
                    tabsetPanel(
                      id="fishy_plots",
                      tabPanel(title = "Yield", plotlyOutput("yieldPlot")),
                      tabPanel(title = "Species", plotlyOutput("fishspeciesPlot")),
                      tabPanel(title = "Size", plotlyOutput("fishsizePlot")),
                      tabPanel(title = "Guild", plotlyOutput("fishguildPlot")),
                      tabPanel(title = "Diet", plotlyOutput("fishdietPlot")),
                      tabPanel(title = "Spectra", plotlyOutput("spectrumPlot")),
                      tabPanel(title = "Single Diet", plotlyOutput("fishdietsinglePlot")),
                      tabPanel(title = "Biomass", plotlyOutput("fishbiomassPlot"))
                    )
                  ),
                  card_body(
                    # Conditional panel for "Yield"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Yield'",
                      h4("Legend"),
                      p("Yield of each of the species across time"),
                      br(),
                      h4(HTML("Yield Plot Showed <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Choose the yield plot to show - Fleet is the yield composition across each fleet.
                              Species is the total yield composition of species across fleets. SoloFleet is the species compositions in each given fleet. '><strong>?</strong></button>")),
                      selectInput(
                        inputId = "YieldChoice",
                        label = NULL,
                        choices = c("Fleet", "SoloFleet", "Species")
                      )
                    ),
                    
                    # Conditional panel for "Species"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Species'",
                      h4("Legend"),
                      p("Change in biomass of each species across a time range.
                    The X axis on this plot shows the species, and the Y axis
                    is the percentage change in the biomass of the given species. This is 
                    in comparison to a identical mizer simulation, except with the current 
                    fishing scenario (0/1/1/1 on the sliders).    
                    Each species is separated into three bars, the middle bar is the chosen 
                    time range as seen in the slider, the left bar is 1/2 of this value, and the right
                    bar is 2x this value. The colour of the bar (red/blue) indicates whether the change is positive
                    or negative, the shade of the colour indicates the time range that it represents, short/chosen/long = 
                    dark to light shading."),
                      br(),
                      h4(HTML("Species Order on Axis <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Select how you want the species to be ordered on the axis. Options include Alphabetical, Size, and Guild.'><strong>?</strong></button>")),
                      selectInput(
                        inputId = "species_order",
                        label = NULL,
                        choices = c("Alphabetical","Size","Guild")
                      )
                    ),
                    
                    # Conditional panel for "Size"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Size'",
                      h4("Legend"),
                      p("Change in the community size spectrum in comparison to the unchanged scenario.
                  The community size spectrum is the biomass of all species in the ecosystem at that given size.
                  The blue line indicates this changed community size spectrum, the grey line indicates
                    the current fishing scenario's community size spectrum at the given time.")
                    ),
                    
                    # Conditional panel for "Guild"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Guild'",
                      h4("Legend"),
                      p("Change in feeding guilds across the entire community in comparison to 
                  the current fishing scenario. Each three of the bars indicates a different time range;
                  left/darkest is 1/2 of the chosen time, middle is the chosen time, brighest is 2x
                  the chosen time.
                    Feeding guilds are groupings of fish based on diet and life stage. ")
                    ),
                    
                    # Conditional panel for "Diet"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Diet'",
                      h4("Legend"),
                      p("Matrix which details the change in proportion of every species in the diet 
                    of a given predator. The colour indicates whether the proportion of the given 
                    species has decreased. Blue is increase, red is decrease. It is possible to change
                    the order of the species shown on the X axis by using the configuration panel."),
                      br(),
                      h4(HTML("Species Order on Axis <button id='infoButtonOrder' class='btn btn-info btn-xs' type='button' style='padding-left: 7px;' data-toggle='popover' data-content='Select how you want the species to be ordered on the axis. Options include Alphabetical, Size, and Guild.'><strong>?</strong></button>")),
                      selectInput(
                        inputId = "species_order",
                        label = NULL,
                        choices = c("Alphabetical","Size","Guild")
                      )
                    ),
                    
                    # Conditional panel for "Spectra"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Spectra'",
                      h4("Legend"),
                      p("Biomass of a given species across their size range. The Y axis is the 
                        biomass of the given species, the X axis is the size class of the species, each 
                        of the lines are a species.")
                    ),
                    
                    # Conditional panel for "Single Diet"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Single Diet'",
                      h4("Legend"),
                      p("The contribution of each species in the diet of a given species across its size range.
                          The X axis is the size of the predator, and the Y axis is the proportion of the diet
                        that a given prey species makes up at that given predator size."),
                      br(),
                      h4(HTML("Select a Species to Plot")),
                      selectInput(
                        inputId = "fish_name_select",
                        label = NULL,
                        choices = NULL
                      )
                    ),
                    
                    # Conditional panel for "Biomass"
                    conditionalPanel(
                      condition = "input.fishy_plots == 'Biomass'",
                      h4("Legend"),
                      p("Biomass of species over time. Color indicates the species, X axis is the time,
                        Y axis is the value of biomass.")
                    )
                  )
        )
      )
    ),
    nav_spacer(),
    nav_item(
      actionButton("start_tutorial", "Guide", class = "btn btn-primary",
                   style = "margin-right: 20px;
                 padding: 5px 5px;")
    ),
  )
)

shinyApp(ui = ui, server = server)

