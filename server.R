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
  selected.options <- reactive({
  options <- a %>% select(Direct_Damage:Passives) %>% names
  options.used <- sapply(options, function(x){if(!is.null(input[[x]]) && input[[x]] != 0) { x }}) %>% unlist
  c("Char1", "Char2", "Char3", "Actives", "Passives", "CombinedHP", lapply(options.used, as.symbol))
  })
   
#   output$mytable1 <- renderDataTable({ select(table(), Char1:Char3, Actives, Passives, "CombinedHP" = CombinedHP)
  output$mytable1 <- renderDataTable({ table() %>% select_(.dots = selected.options())
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
  
  
  #### Observe ####
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
  #### Observe ####
  
  #### Plots ####
  #### Damage Curves ####
  output$ggPlot1 <- renderPlot({
    data <- table()
    
    show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
    if(length(show_chars) == 0){ return() }
    
    data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
      # data2 <- melt(c, measure.var = c("L1.AP", "L2.AP", "L3.AP", "L4.AP", "L5.AP")) %>%
      merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
      filter(Type == "damage" & (Target == "team" | Target == "target")) %>%
      filter(variable != "MAX") %>%
      filter(!is.na(value) & value > 0)
    
    if(dim(data2)[1] == 0){ return() }
    
    ggplot(unique(data2), aes(x = variable, y = value, group = Power, color = Color)) +
      geom_line() + 
      #       stat_summary(fun.y = mean, geom = "point", size = 1, aes(group = CharacterName1)) +
      facet_grid(Type + Target ~ Color, scales = "free") + 
      geom_point(size = 1) +
      geom_text(data = subset(data2, variable == "L5"), 
                aes(x = variable, y = value, group = Power, 
                    label = paste(Rarity, "* ", CharacterName1, " ", Damage_Range, sep = "")), hjust = 1, vjust = -0.2, size = 3) + 
      MPQ_color_scale + 
      scale_y_continuous(limits= c(0, NA)) +
      ylab("Damage") +
      xlab("Level") +
      theme_bw() + theme(legend.position="none")
    
  }, height = 600)
  
  #### Max Damage ####
  output$ggPlot2 <- renderPlot({
    
    data <- table()
    show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
    if(length(show_chars) == 0){ return() }
    
    data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
      
      merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
      filter(Type == "damage" & (Target == "team" | Target == "target")) %>%
      filter(variable == "MAX") %>%
      filter(!is.na(value) & value > 0) %>%
      select(CharacterName3, Damage_Range, Color, Power, Type, Target, Rarity, variable, value)
    
    if(dim(data2)[1] == 0){ return() }
    
    ggplot(unique(data2), aes(x = variable, y = value, group = Power, color = Color)) +
      #geom_point(size = 0.5) +
      facet_grid(Type + Target ~ Color, scales = "free") + 
      geom_text(aes(label = paste(Rarity, "* ", CharacterName3, " ", Damage_Range, sep = "")), hjust = 0.5, vjust = 0.5, size = 3) + 
      MPQ_color_scale + 
      scale_y_continuous(limits= c(0, NA)) +
      ylab("Damage") +
      xlab("Level") +
      theme_bw() + theme(legend.position="none")
    
  }, height = 600)
  
  #### Max Abilities ####
  output$ggPlot3 <- renderPlot({
    
    data <- table()
    show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
    if(length(show_chars) == 0){ return() }
    
    if(is.null(input$ability_effects_options)) {return()}
    data <- merge(c, input$ability_effects_options, by.y = 1, by.x = "Type")
#       data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
data2 <- melt(data, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX")) %>%
      merge(show_chars, by.y = 1, by.x = "CharacterName") %>%
#       filter(Type != "damage") %>%
      filter(variable == "MAX") %>%
      filter(!is.na(value) & value > 0)
    if(dim(data2)[1] == 0){ return() }
    
    ggplot(data2, aes(x = variable, y = value, group = Power, color = Color)) +
      facet_grid(Type + Target ~ Color, scales = "free") + 
      geom_text(aes(label = paste(Rarity, "* ", CharacterName1, " ", Damage_Range, sep = "")), hjust = 0.5, vjust = 0.5, size = 3) + 
      MPQ_color_scale +
      scale_y_continuous(limits= c(0, NA)) +
      ylab("Amount") +
      xlab("Level") +
      theme_bw() + theme(legend.position="none")
    
  }, height = 600)
  
  #### Visualizations ####
  # Color wheel
  output$color_wheel <- renderPlot({
    if(!is.null(input$select_rarity_color_wheel)){
      colors <- c('yellow', 'red', 'blue', 'purple', 'green', 'black')
      data <- table()
      show_chars <- unique(unlist(c(data[, 1], data[, 2], data[, 3])))
      if(length(show_chars) == 0){ return() }
      
      colors <- c('black', 'blue', 'green', 'purple', 'red', 'yellow')
      
      data2 <- melt(c, measure.var = c("L1", "L2", "L3", "L4", "L5", "MAX"))
      data2 <- merge(data2, as.integer(input$select_rarity_color_wheel), by.y = 1, by.x = "Rarity")
      
      data3 <- data2 %>% #filter(Rarity > 1) %>%
        mutate(CharacterName = str_replace(CharacterName, " \\[.*\\]$", ""),
               CharacterName1 = str_replace(CharacterName1, " \\[.*\\]$", "")) %>%
        group_by(CharacterName) %>% 
        select(Color, Rarity) %>%
        mutate(Color = paste(intersect(levels(factor(Color)), colors), collapse = ",")) %>%
        unique()
      
      color_wheel <- c("yellow,red,blue", "yellow,red,purple", "yellow,red,green", "yellow,red,black", "yellow,blue,purple", "yellow,blue,green", "yellow,blue,black", "yellow,purple,green", "yellow,purple,black", "yellow,green,black", "purple,green,black", "blue,green,black", "blue,purple,black", "blue,purple,green", "red,green,black", "red,purple,black", "red,purple,green", "red,blue,black", "red,blue,green", "red,blue,purple")
      new_color_wheel <- color_wheel %>% str_split(",") %>% lapply(function(x){paste(intersect(colors, x), collapse = ',')}) %>% unlist
      
      data3[["Color"]] <- factor(data3[["Color"]], levels = new_color_wheel)
      
      data4 <- bind_rows(data3, data3, data3)
      get_color <- function(y){data3[["Color"]] %>% str_split(",") %>% lapply(function(x){x[y]}) %>% unlist}
      Colors <- c(sapply(1:3, get_color))
      data5 <- na.omit(cbind(data4, Colors))
      if(dim(data5)[1] == 0) { return() }
      data5[["Color"]] <- factor(data5[["Color"]], levels = new_color_wheel)
      
      p <- ggplot(data = data5, aes(x = gsub(",", "\n", Color), fill = Colors)) + geom_bar() + coord_polar(theta = 'x'); 
      q <- p + MPQ_fill_scale +
        scale_x_discrete(limits = gsub(",", "\n",new_color_wheel)) +
        scale_y_continuous(breaks=seq(0, 99, 3), labels=seq(0, 99, 3) / 3) +    
        ylab("Character count") + xlab("Ability Colors") + theme_bw()
      q
    }
  })
  
  # Abilities by color
  ability_color_table <- reactive ({
    if(is.null(input$select_rarity_color_wheel)){ NULL } else {
      data <- select(c, Direct_Damage:Manipulate_AP) %>%
        mutate_each(funs(sapply_string2integer)) %>%
        bind_cols(select(c, CharacterName, Rarity, Color, AP, PowerName), .)
      
      data2 <- merge(data, input$select_rarity_color_wheel, by.x = "Rarity", by.y = 1, all.y = T) %>% tbl_df
      data3 <- data2 %>% group_by(PowerName, Rarity) %>% select(Direct_Damage:Manipulate_AP) %>% summarize_each(funs(sum)) %>% group_by(PowerName, Rarity) %>% mutate_each(funs(as.logical)) %>% mutate_each(funs(as.numeric))
      data4 <- merge(data3, select(c, CharacterName, Rarity, Color, AP, PowerName)) %>%
        mutate(CharacterName = str_replace(CharacterName, " \\[.*\\]$", "")) %>% select(CharacterName, PowerName, Rarity, Color, x = matches(paste("^", input$ability, "$", sep = ""))) %>% unique
      data4 %>% arrange(desc(Color))
    }
  })
  
  output$ability_color_table <- renderDataTable(if(!is.null(ability_color_table())) { ability_color_table() %>% filter(x == 1) %>% select(-x) %>% arrange(Color, CharacterName)} else {return()} )
  
  output$ability_effects <- renderPlot ({
    if(is.null(ability_color_table())){ return() }
    if(input$ability == 0){ return() }
    
    if(input$count_frequency == "Count"){
      data4 <- ability_color_table()
      ggplot(data4, aes(x = factor(x), fill = Color)) + geom_bar() + facet_grid(.~Color) +
        MPQ_fill_scale + 
        theme_bw() + theme(legend.position="none") +
        xlab(label = str_replace_all(input$ability, "_", " ")) +
        ylab("Abilities (count)") + 
        scale_x_discrete(breaks = c(1,0), labels = c("yes", "no"))
      
    } else if(input$count_frequency == "Frequency"){
      data4 <- ability_color_table()
      data5 <- data4 %>% select(Color, x) %>% group_by(Color, add = F) %>% 
        summarize(p.null = sum(data4$x)/dim(data4)[1],
                  p.value = binom.test(sum(x), length(x), sum(data4$x)/dim(data4)[1])$p.value,
                  conf.u = binom.test(sum(x), length(x), sum(data4$x)/dim(data4)[1])[[4]][1],
                  conf.l = binom.test(sum(x), length(x), sum(data4$x)/dim(data4)[1])[[4]][2],
                  frequency = binom.test(sum(x), length(x), sum(data4$x)/dim(data4)[1])[[5]]) %>% 
        unique %>% group_by(Color, add = F) %>% mutate(p.value_star = if(p.value < 0.00001){"****"} else if (p.value < 0.0001){"***"} else if(p.value < 0.001){"**"} else if (p.value < 0.05) {"*"} else {""})
      
      ggplot(data5, aes(x = Color, y = frequency, fill = Color)) + geom_hline(yintercept = unique(data5$p.null), linetype = "longdash") +
        geom_bar(stat = "identity") + #facet_grid(.~Color) +
        MPQ_fill_scale + 
        geom_text(aes(x = Color, y = frequency, label = p.value_star), vjust = 0.5, size = 10) +
        xlab(label = str_replace_all(input$ability, "_", " ")) +
        ylab("Abilities (frequency)") +
        ylim(0,1) + 
        geom_errorbar(aes(x = Color, ymin = conf.u, ymax = conf.l), color = "darkgray", alpha = 0.5, width = 0.25) +
        theme_bw() + theme(legend.position="none")
    }
  })
  
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
      
      #### Health Filter ####
      data <- filter(data, (MaxHP.Char3 >= input$MaxHP[1] & MaxHP.Char3 <= input$MaxHP[2]) & (MaxHP.Char1 >= input$MaxHP[1] & MaxHP.Char1 <= input$MaxHP[2]) & (MaxHP.Char2 >= input$MaxHP[1] & MaxHP.Char2 <= input$MaxHP[2]))
      data <- filter(data, CombinedHP >= input$CombinedHP[1] & CombinedHP <= input$CombinedHP[2])
      
      #### Option Filter Function ####
      filterOption <- function(data, option){
        if (!is.null(input[[option]]) && input[[option]] != 0){ data <- data[data[[option]] >= as.integer(input[[option]]),] }
        data
      }
      
      #### Options ####
      options <- data %>% select(Direct_Damage:Passives) %>% names
      
      for (i in 1:length(options)){
        data <- filterOption(data, options[i])
      }
      
if(!is.null(data)){ data <- arrange(data, desc(Actives), desc(Passives), desc(CombinedHP)) }
data

    }
  }
  
  #### Rarity menu filter ####
  rarity_filter <- function(data){
    unchecked <- setdiff(allRarity, input$select_rarity)
    if(length(unchecked) == 0 ){ data } else {
      for(i in 1:length(unchecked)) {
        data <- filter(data, Rarity.Char1 != unchecked[i] & Rarity.Char2 != unchecked[i] & Rarity.Char3 != unchecked[i])
      }
    }
    data
  }
  
  #### Include character filter ####
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
  onclick("Preset_Teams", shinyjs::reset("options-reset"))
  onclick("Preset_Teams", shinyjs::reset("sidebar-reset"), add = T)
  
  preset_team_selections <- function(){
    
    if(input$Preset_Teams == "3* / 4* Rarity Rainbow(+): 6 Actives & 3 (or more) Passives"){
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost"))
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Passives", selected = 3)
    }
    
    if(input$Preset_Teams == "2* / 3* Rarity Rainbow(+): 6 Actives & 3 (or more) Passives"){
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost"))
      updateSelectInput(session, inputId = 'select_rarity', selected = c("2","3"))
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Passives", selected = 3)
    }
    
    if(input$Preset_Teams == "Stunning!: 3 Stun Abilities in 3 different colors (or from passives)"){
      updateSelectInput(session, inputId = "radio", selected = "Exclude:")
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Effects"))
      updateSelectInput(session, inputId = "Stun", selected = 3)
      updateSelectInput(session, inputId = 'select_rarity', selected = c("3","4"))
    }
    
    if(input$Preset_Teams == "X-Force: Shred the Board (6 Active Colors & 5 Board Shake Abilities)"){
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost", "Effects"))
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
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost", "Damage"))
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
    
    if(input$Preset_Teams == "Anti-Kishu/Thug: (1 Stun and 1 Special Tile Remove, Steal, Improve, and Protect Abilities)"){
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost", "Special Tiles", "Effects"))
      updateSelectInput(session, inputId = 'select_rarity', selected = "3")
      updateSelectInput(session, inputId = "Actives", selected = 6)
      updateSelectInput(session, inputId = "Remove_Special_Tile", selected = 1)
      updateSelectInput(session, inputId = "Improve_Special_Tile", selected = 1)
      updateSelectInput(session, inputId = "Steal_Special_Tile", selected = 1)
      updateSelectInput(session, inputId = "Add_Protect_Tile", selected = 1)
      updateSelectInput(session, inputId = "Stun", selected = 1)
    }
    
    if(input$Preset_Teams == "Kingpin: Poke (3 2-Turn CDs Abilies to Finger Poke)"){
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost", "Misc. Tiles"))
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
    
    if(input$Preset_Teams == "Carnage: Stayin' Alive (5 Active Colors & 2 Healing Abilities)"){
      updateSelectInput(session, inputId = 'select_rarity', selected = c("4","3"))
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Active / Passive / AP Cost", "Effects"))
      updateSelectInput(session, inputId = "Actives", selected = 5)
      updateSelectInput(session, inputId = "Passives", selected = 2)
      updateSelectInput(session, inputId = "Healing", selected = 2)
      updateSelectInput(session, inputId = "radio", selected = "Select (up to 3):")
      updateSelectInput(session, inputId = 'select_4chars', selected = "Carnage (Cletus Kasady)")
    }
    
    if(input$Preset_Teams == "Venom: Infinite Damage (6 Active Colors & 2 Web Tile Abilities)"){
      updateSelectInput(session, inputId = "show_ability_options", selected = c("Misc. Tiles"))
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