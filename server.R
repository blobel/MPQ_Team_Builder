shinyServer(function(input, output, session) {
  
  #### Download Results.csv ####
  output$downloadTeamBuilder_csv <- downloadHandler(
    filename = function() {
      paste('Team_Builds-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(table(), con)
    }
  )
  
  #### Download Teams.csv ####
  output$downloadTeams_csv <- downloadHandler(
    filename = function() {
      paste('Teams-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(table(), con)
    }
  )
  
  #### Download Characters.csv ####
  output$downloadCharacters_csv <- downloadHandler(
    filename = function() {
      paste('Characters-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(table(), con)
    }
  )
  
  #### Character Compendium Table ####
  output$team_compendium <- renderDataTable(arrange(a, desc(Actives), desc(Passives), desc(CombinedHP)))
  output$compendium <- renderDataTable(arrange(c, desc(Rarity), CharacterName))
  
  #### Team Selection Table ####
  output$mytable1 <- renderDataTable({ select(table(), Char1:Char3, Actives, Passives, "CombinedHP" = CombinedHP)
  }, options = list(lengthMenu = c(10, 20, 40), pageLength = 20))
  
  #### Reactive data filtering by character selection ####
  table <- reactive({
    data <- a %>% rarity_filter
    if (input$radio == 'Include:') {
      data %>% include_chars_filter %>% top_filter
    }
    else if (input$radio == "Select (up to 3):"){
      data %>% select_chars_filter %>% top_filter
    }
    else if (input$radio == "Exclude:") {
      data %>% exclude_chars_filter %>% top_filter
    }
  })
  
  # Preset filters
  observe({
    preset_team_selections()
  })
  
  # Set panels to match rarity filtered values
  observe({
    if(!is.null(input$select_rarity)){
      rarity.a <- rarity_filter(a)
      
      rarity.min.HP = select(rarity.a, MaxHP.Char1, MaxHP.Char2, MaxHP.Char3) %>% unlist %>% min(na.rm = T)
      rarity.max.HP = select(rarity.a, MaxHP.Char1, MaxHP.Char2, MaxHP.Char3) %>% unlist %>% max(na.rm = T)
      rarity.combined.min.HP = select(rarity.a, CombinedHP) %>% min(na.rm = T)
      rarity.combined.max.HP = select(rarity.a, CombinedHP) %>% max(na.rm = T)
      
      min.HP = select(rarity.a, MaxHP.Char1, MaxHP.Char2, MaxHP.Char3) %>% unlist %>% min(na.rm = T)
      max.HP = select(rarity.a, MaxHP.Char1, MaxHP.Char2, MaxHP.Char3) %>% unlist %>% max(na.rm = T)
      combined.min.HP = select(rarity.a, CombinedHP) %>% min(na.rm = T)
      combined.max.HP = select(rarity.a, CombinedHP) %>% max(na.rm = T)
      
      updateSliderInput(session, inputId = "MaxHP", value = c(min.HP, max.HP), min = rarity.min.HP -1000, max = rarity.max.HP +1000, step = 100)
      updateSliderInput(session, inputId = "CombinedHP", value = c(combined.min.HP, combined.max.HP), min = rarity.combined.min.HP-1000, max = rarity.combined.max.HP +1000, step = 100)
      
    }
  })
  
  #### Plots ####
  # Power Plots
  ## Damage Curves
  output$ggPlot1 <- renderPlot({
    data <- table()
    
    show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
    if(length(show_chars) == 0){ return() }
    
    data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
      merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
      filter(Type == "damage" & (Target == "team" | Target == "target")) %>%
      filter(variable != "MAX") %>%
      filter(!is.na(value) & value > 0)
    
    if(dim(data2)[1] == 0){ return() }
    
    ggplot(data2, aes(x = variable, y = value, group = Power, color = Color)) +
      geom_line() + 
      #       stat_summary(fun.y = mean, geom = "point", size = 1, aes(group = CharacterName1)) +
      facet_grid(Type + Target ~ Color, scales = "free") + 
      geom_point(size = 1) +
      geom_text(data = subset(data2, variable == "L5"), 
                aes(x = variable, y = value, group = Power, 
                    label = paste(Rarity, "* ", CharacterName1, " ", Damage_Range, sep = "")), hjust = 1, vjust = -0.2, size = 3) + 
      scale_color_manual(values = c("black" = "black", "blue" = "royalblue", "green" = "forestgreen", "purple" = "purple", "red" = "darkred", "yellow" = "goldenrod"),
                         limits = c("black", "blue", "green", "purple", "red", "yellow")) + 
      ylab("Damage") +
      xlab("Level") +
      theme_bw()
    
  }, height = 600)
  
  # Max Damage
  output$ggPlot2 <- renderPlot({
    
    data <- table()
    show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
    if(length(show_chars) == 0){ return() }
    
    data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
      merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
      filter(Type == "damage" & (Target == "team" | Target == "target")) %>%
      filter(variable == "MAX") %>%
      filter(!is.na(value) & value > 0)
    
    if(dim(data2)[1] == 0){ return() }
    
    ggplot(data2, aes(x = variable, y = value, group = Power, color = Color)) +
      #geom_point(size = 0.5) +
      facet_grid(Type + Target ~ Color, scales = "free") + 
      geom_text(aes(label = paste(Rarity, "* ", CharacterName1, " ", Damage_Range, sep = "")), hjust = 0.5, vjust = 0.5, size = 3) + 
      scale_color_manual(values = c("black" = "black", "blue" = "royalblue", "green" = "forestgreen", "purple" = "purple", "red" = "darkred", "yellow" = "goldenrod"),
                         limits = c("black", "blue", "green", "purple", "red", "yellow")) + 
      ylab("Damage") +
      xlab("Level") +
      theme_bw()
    
  }, height = 600)
  
  # Max Abilities
  output$ggPlot3 <- renderPlot({
    
    data <- table()
    show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
    if(length(show_chars) == 0){ return() }
    
    data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
      merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
      filter(Type != "damage") %>%
      filter(variable == "MAX") %>%
      filter(!is.na(value) & value > 0)
    if(dim(data2)[1] == 0){ return() }
    
    ggplot(data2, aes(x = variable, y = value, group = Power, color = Color)) +
      #geom_point(size = 0.5) + 
      facet_grid(Type + Target ~ Color, scales = "free") + 
      geom_text(aes(label = paste(Rarity, "* ", CharacterName1, " ", Damage_Range, sep = "")), hjust = 0.5, vjust = 0.5, size = 3) + 
      scale_color_manual(values = c("black" = "black", "blue" = "royalblue", "green" = "forestgreen", "purple" = "purple", "red" = "darkred", "yellow" = "goldenrod"),
                         limits = c("black", "blue", "green", "purple", "red", "yellow")) + 
      ylab("Damage") +
      xlab("Level") +
      theme_bw()
    
  }, height = 600)
  
#   #### Graphs ####
#   # Color wheel
#   colors <- c('black', 'blue', 'green', 'purple', 'red', 'yellow')
#   data <- a
#   show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
#   if(length(show_chars) == 0){ return() }
#   
#   data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
#     merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
#     filter(Type != "damage") %>%
#     filter(variable == "MAX") %>%
#     filter(!is.na(value) & value > 0)
#   if(dim(data2)[1] == 0){ return() }
#   
#   test <- ddply(data2, "CharacterName", summarize, Colors = paste(Color, collapse = ","))
#   test <- data2 %>% tbl_df %>% group_by("CharacterName1") %>% summarize(paste(Color, collapse = ","))
#   
#     
#   ggplot()
#   
  #### Functions ####
  #### Reset options function ####
  observeEvent(input$reset_input, {
    shinyjs::reset("options-reset")
    shinyjs::reset("sidebar-reset")
  })
  
  #### Option menu filter ####
  top_filter <- function(data) {
    
    if(is.null(data)){ data } else {
      # Playable Teams
      if(input$Unlocked_Characters == T){ data <- filter(data, Unlocked_Characters == 3) }
      
      # Health Filter
      data <- filter(data, (MaxHP.Char3 >= input$MaxHP[1] & MaxHP.Char3 <= input$MaxHP[2]) & (MaxHP.Char1 >= input$MaxHP[1] & MaxHP.Char1 <= input$MaxHP[2]) & (MaxHP.Char2 >= input$MaxHP[1] & MaxHP.Char2 <= input$MaxHP[2]))
      data <- filter(data, CombinedHP >= input$CombinedHP[1] & CombinedHP <= input$CombinedHP[2])
      
      # Option Filter Function
      filterOption <- function(data, option){
        if (!is.null(input[[option]]) && input[[option]] != 0){ data <- data[data[[option]] >= as.integer(input[[option]]),] }
        data
      }
      
      #### Options ####
      options <- c("Actives", "Passives", "Cheap", 
                   "Offensive", "Possible_Damage", "Direct_Damage", "Team_Damage",
                   "Defensive", "Stun", "Shake", "Healing",
                   "Manipulate_AP", "Steal_AP", "Gain_AP", "Destroy_AP",
                   "Add_Special_Tile", "Remove_Special_Tile", "Improve_Special_Tile", "Steal_Special_Tile",
                   "Convert_Tile_Color", "Add_CD_Tile", "Add_2_Turn_CD_Tile", "Add_Trap_Tile",
                   "Add_Strike_Tile", "Add_Attack_Tile", "Add_Protect_Tile", "Add_Charged_Tile", 
                   "Add_Critical_Tile", "Add_Locked_Tile", "Add_Invis_Tile", "Add_Web_Tile")
      
      for (i in 1:length(options)){
        data <- filterOption(data, options[i])
      }
      
      if(!is.null(data)){ data <- arrange(data, desc(Actives), desc(Passives), desc(CombinedHP)) }
      data
    }
  }
  
  # Rarity menu filter
  rarity_filter <- function(data){
    unchecked <- setdiff(allRarity, input$select_rarity)
    if(length(unchecked) == 0 ){ data } else {
      for(i in 1:length(unchecked)) {
        data <- filter(data, Rarity.Char1 != unchecked[i] & Rarity.Char2 != unchecked[i] & Rarity.Char3 != unchecked[i])
      }
    }
    data
  }
  
  # Include character filter
  include_chars_filter <- function(data){
    include_chars <- c(input$include_1chars, input$include_2chars, input$include_3chars, input$include_4chars, input$include_5chars)
    unchecked <- setdiff(allChars, include_chars)
    if(length(unchecked) == 0 ){ data } else {
      for(i in 1:length(unchecked)) {
        data <- filter(data, Char1 != unchecked[i] & Char2 != unchecked[i] & Char3 != unchecked[i])
      }
    }
    data
  }
  
  # Exclude character filter
  exclude_chars_filter <- function(data){
    exclude_chars <- c(input$exclude_1chars, input$exclude_2chars, input$exclude_3chars, input$exclude_4chars, input$exclude_5chars)
    if(!is.null(exclude_chars)){
      for(i in 1:length(exclude_chars)) {
        data <- filter(data, Char1 != exclude_chars[i] & Char2 != exclude_chars[i] & Char3 != exclude_chars[i])
      }
    }
    data
  }
  
  # Select character filter
  select_chars_filter <- function(data){
    select_chars <- c(input$select_1chars, input$select_2chars, input$select_3chars, input$select_4chars, input$select_5chars)
    if(!is.null(select_chars) && !is.null(data) && dim(data)[1] != 0) {
      
      select_chars.list <- list()
      for(i in 1:length(select_chars)) {
        select_chars.list[[i]] <- c(which(data$Char1 == select_chars[i]), 
                                    which(data$Char2 == select_chars[i]), 
                                    which(data$Char3 == select_chars[i]))
      }
      data[Reduce(intersect, select_chars.list), ]
    } else {
      data[0,]
    }
  }
  
  
  #### Preset Team Selections ####
  preset_team_selections <- function(){
    if(input$Preset_Teams == 0){
      
    }
    if(input$Preset_Teams == "3* / 4* Rarity - 6 Actives & 3 (or more) Passives"){
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Passives", selected = 3)
    }
    if(input$Preset_Teams == "2* / 3* Rarity - 6 Actives & 3 (or more) Passives"){
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
      updateSelectInput(session, inputId = 'select_rarity', selected = c("2","3"))
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Passives", selected = 3)
    }
    if(input$Preset_Teams == "Stunning!: 3 Stun Abilities in 3 different colors (or from passives)"){
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = "Stun", selected = 3)
    }
      
    if(input$Preset_Teams == "X-Force: Shred the Board (6 Active Colors & 5 Board Shake Abilities)"){
      
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Shake", selected = 5)
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = 'select_4chars', selected = "Wolverine (X-Force)")
      updateSelectInput(session, inputId = 'select_5chars', selected = "")
      updateSelectInput(session, inputId = 'select_3chars', selected = "")
      updateSelectInput(session, inputId = 'select_2chars', selected = "")
      updateSelectInput(session, inputId = 'select_1chars', selected = "")
    }
    
    if(input$Preset_Teams == "Kamala Khan: Heal and Deal (6 Active Colors & 3 Fast & Damaging Abilities)"){

      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Cheap", selected = 3)
      updateSelectInput(session, inputId = "Possible_Damage", selected = 3)
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'select_3chars', selected = "Kamala Khan (Ms. Marvel)")
      updateSelectInput(session, inputId = 'select_5chars', selected = "")
      updateSelectInput(session, inputId = 'select_4chars', selected = "")
      updateSelectInput(session, inputId = 'select_2chars', selected = "")
      updateSelectInput(session, inputId = 'select_1chars', selected = "")
    }
    
    if(input$Preset_Teams == "Anti-Kishu/Thug (1 Stun and 1 Special Tile Remove, Steal, Improve, and Protect)"){
      
      updateSelectInput(session, inputId = 'select_rarity', selected = "3")
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Remove_Special_Tile", selected = 1)
      updateSelectInput(session, inputId = "Improve_Special_Tile", selected = 1)
      updateSelectInput(session, inputId = "Steal_Special_Tile", selected = 1)
      updateSelectInput(session, inputId = "Add_Protect_Tile", selected = 1)
      updateSelectInput(session, inputId = "Stun", selected = 1)
    }
    
    if(input$Preset_Teams == "Kingpin: Poke (3 2-Turn CDs Abilies to Finger Poke)"){
      
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Add_2_Turn_CD_Tile", selected = 3)
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'select_4chars', selected = "Kingpin (Wilson Fisk)")
      updateSelectInput(session, inputId = 'select_5chars', selected = "")
      updateSelectInput(session, inputId = 'select_3chars', selected = "")
      updateSelectInput(session, inputId = 'select_2chars', selected = "")
      updateSelectInput(session, inputId = 'select_1chars', selected = "")
      
    }
    
    if(input$Preset_Teams == "Carnage: Stayin' Alive (5 Actives & 2 Healing Abilities)"){
      updateSelectInput(session, inputId = 'select_rarity', selected = c("4","3"))
      updateSelectInput(session, inputId = "Healing", selected = 2)
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'select_4chars', selected = "Carnage (Cletus Kasady)")
      
    }
    
    if(input$Preset_Teams == "Venom: Infinite Damage (6 Actives & 2 Web Tile Actives)"){
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","1"))
      updateSelectInput(session, inputId = "Add_Web_Tile", selected = 2)
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'select_1chars', selected = "Venom (Dark Avengers Spider-Man)")
      
    }
    if(input$Preset_Teams == "SKIP!"){
      updateSelectInput(session, inputId = 'select_rarity', selected = c("5"))
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
    }
    if(input$Preset_Teams == "Deadpool! (You can't play these teams!)"){

      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'Unlocked_Characters', selected = F)
      updateSelectInput(session, inputId = 'select_3chars', selected = "Deadpool (It's Me, Deadpool)")
      updateSelectInput(session, inputId = 'select_4chars', selected = "Deadpool (Uncanny X-Force)")
      updateSelectInput(session, inputId = 'select_5chars', selected = "")
      updateSelectInput(session, inputId = 'select_2chars', selected = "")
      updateSelectInput(session, inputId = 'select_1chars', selected = "")
    }
    
  }
}) #End Shiny