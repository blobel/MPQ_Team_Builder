blank <- 0; names(blank) = "------ Preset Teams ------"
ability_effect_choices <- c(blank, names(select(a, Direct_Damage:Manipulate_AP))); names(ability_effect_choices) <- str_replace_all(ability_effect_choices, "_", " ")
ability_filter_choices <- c("Active / Passive / AP Cost", "Damage", "Effects", "AP", "Special Tiles", "Misc. Tiles", "Tile Color Conversion")
min.HP = select(a, MaxHP.Char1, MaxHP.Char2, MaxHP.Char3) %>% unlist %>% min(na.rm = T)
max.HP = select(a, MaxHP.Char1, MaxHP.Char2, MaxHP.Char3) %>% unlist %>% max(na.rm = T)
combined.min.HP = select(a, CombinedHP) %>% min(na.rm = T)
combined.max.HP = select(a, CombinedHP) %>% max(na.rm = T)

#### tablePullDownTab Function ####
tablePullDownTab <-  function(table, inputId = names(table)[1], label = str_replace(inputId, "_", " "), choices = table[[inputId]] %>% factor %>% levels, selected = 0, ...) {
  blank <- 0; names(blank) = " "
  selectInput(inputId = inputId,
              label = label,
              choices = c(blank, choices[choices != 0]),
              selected = selected
  )}

#### Rarity Panel function ####
Rarity_Panel <- function(inputId = 'select_rarity', inline = T){checkboxGroupInput(inputId = inputId,
                                                                                   inline = inline,
                                                                                   label = 'Rarity:',
                                                                                   choices = rev(allRarity),
                                                                                   selected = c(4,3)
)}

#### Main Panel function ####
Main_Panel <-  fluidPage(
  mainPanel(width = 9,
            tabsetPanel(
              tabPanel(title = 'Team', dataTableOutput(outputId = 'mytable1')),
              tabPanel(title = 'Damage Curves', plotOutput(outputId = "ggPlot1")),
              tabPanel(title = 'Max Damage', plotOutput(outputId = "ggPlot2")),
              tabPanel(title = 'Max Abilities', plotOutput(outputId = "ggPlot3"))
            )
  )
)

#### Option Panel ####

Option_Panel <- fluidPage(shinyjs::useShinyjs(), style = "padding: 5px;",
                          id = "options-reset",
                          
                          #### Download button ####
                          wellPanel(style = "padding: 5px;",
                                    fluidRow(column(width = 12, downloadButton(outputId = 'downloadTeamBuilder_csv', label = 'Download "Team_Builds.csv"')))
                          ),
                          
                          #### Preset Teams ####
                          fluidRow(
                            column(width = 2, actionButton(inputId = "reset_input", label = "Reset Settings")),
                            column(width = 10, 
                                   selectInput(inputId = "Preset_Teams", label = NULL, width = "100%",
                                               choices = c(blank,
                                                           "--- Rainbow Teams with 3 (or more) Passive Abilities ---",
                                                           "3* / 4* Rarity Rainbow(+): 6 Actives & 3 (or more) Passives",
                                                           "2* / 3* Rarity Rainbow(+): 6 Actives & 3 (or more) Passives",
                                                           "--- Ability Effect Teams ---",
                                                           "Stunning!: 3 Stun Abilities in 3 different colors (or from passives)",
                                                           "Anti-Kishu/Thug: (1 Stun and 1 Special Tile Remove, Steal, Improve, and Protect Abilities)",
                                                           "--- Required Character Teams ---",
                                                           "Venom: Infinite Damage (6 Active Colors & 2 Web Tile Abilities)",
                                                           "Carnage: Stayin' Alive (5 Active Colors & 2 Healing Abilities)",
                                                           "X-Force: Shred the Board 6 Active Colors & 5 Board Shake Abilities)",
                                                           "Kamala Khan: Heal and Deal (6 Active Colors & 3 Fast & 3 Damaging Abilities)",
                                                           "Kingpin: Poke (3 2-Turn CDs Abilies to Finger Poke)",
                                                           "Deadpool! (You can't play these teams!)",
                                                           "--- Other ---",
                                                           "SKIP!"
                                               ),
                                               selected = 0)
                            )
                          ),
                          helpText("Filter teams below using the Ability and Character filters. Try the Preset Teams (above) to get ideas!"),
                          
                          #### Options Panel Show/Hide ####
                          
                          fluidRow(column(width = 12,
                                          checkboxGroupInput(inputId = "show_ability_options",
                                                             inline = T,
                                                             label = 'Ability Filter Options:',
                                                             choices = ability_filter_choices,
                                                             selected = ability_filter_choices[1])
                          )),
                          
                          #### Active / Passive Options ####
                          conditionalPanel(condition = "input.show_ability_options.indexOf('Active / Passive / AP Cost') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("Active / Passive / AP Cost")
                                                       column(3, tablePullDownTab(a, inputId = "Actives", label = "Active Colors")),
                                                       column(3, tablePullDownTab(a, inputId = "Passives", label = "Passive Abilities")),
                                                       column(2, tablePullDownTab(a, inputId = "Cheap", label = "Fast: < 7 AP")),
                                                       column(2, tablePullDownTab(a, inputId = "Expenisive", label = "Slow: > 10 AP"))
                                                     )
                                           )
                          ),
                          
                          #### Abilities ####
                          #                           wellPanel(style = "padding: 5px;",
                          #                                     fluidRow(
                          #                                       column(2, tablePullDownTab(a, inputId = "Cheap", label = "Fast ( < 7 AP )")),
                          #                                       column(2, tablePullDownTab(a, inputId = "Expenisive", label = "Slow ( > 10 AP )"))
                          #                                     )
                          #                           ),
                          
                          conditionalPanel(condition = "input.show_ability_options.indexOf('Damage') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("Damage"),
                                                       column(2, tablePullDownTab(a, inputId = "Offensive", label = "Any Offensive")),
                                                       column(2, tablePullDownTab(a, inputId = "Possible_Damage", label = "Any Damage")),
                                                       column(2, tablePullDownTab(a, inputId = "Direct_Damage", label = "Direct")),
                                                       column(2, tablePullDownTab(a, inputId = "Team_Damage", label = "Team"))
                                                     )
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.show_ability_options.indexOf('Effects') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("Effects"),
                                                       column(2, tablePullDownTab(a, inputId = "Defensive", label = "Any Defensive")),
                                                       column(2, tablePullDownTab(a, inputId = "Stun", label = "Stun")),
                                                       column(2, tablePullDownTab(a, inputId = "Shake", label = "Shake")),
                                                       column(2, tablePullDownTab(a, inputId = "Healing", label = "Heal"))
                                                     )
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.show_ability_options.indexOf('AP') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("AP")
                                                       column(2, tablePullDownTab(a, inputId = "Manipulate_AP", label = "Any AP:")),
                                                       column(2, tablePullDownTab(a, inputId = "Steal_AP", label = "Steal")),
                                                       column(2, tablePullDownTab(a, inputId = "Gain_AP", label = "Gain")),
                                                       column(2, tablePullDownTab(a, inputId = "Destroy_AP", label = "Destroy"))
                                                     )
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.show_ability_options.indexOf('Special Tiles') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("Special Tiles"),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Special_Tile", label = "Add Any:")),
                                                       column(2, tablePullDownTab(a, inputId = "Remove_Special_Tile", label = "Remove")),
                                                       column(2, tablePullDownTab(a, inputId = "Improve_Special_Tile", label = "Improve")),
                                                       column(2, tablePullDownTab(a, inputId = "Steal_Special_Tile", label = "Steal"))
                                                     ),
                                                       fluidRow(#h5("Common Special Tiles")
                                                         column(2),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Strike_Tile", label = "Strike")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Attack_Tile", label = "Attack")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Protect_Tile", label = "Protect"))
                                                     )
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.show_ability_options.indexOf('Misc. Tiles') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("Tile:"),
                                                       column(2, tablePullDownTab(a, inputId = "Add_CD_Tile", label = "CD")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_2_Turn_CD_Tile", label = "2TurnCD")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Trap_Tile", label = "Trap")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Charged_Tile", label = "Charged")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Critical_Tile", label = "Critical")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Locked_Tile", label = "Locked")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Invis_Tile", label = "Invisible")),
                                                       column(2, tablePullDownTab(a, inputId = "Add_Web_Tile", label = "Web"))
                                                     )
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.show_ability_options.indexOf('Tile Color Conversion') > -1",
                                           wellPanel(style = "padding: 5px;",
                                                     fluidRow(#h5("Convert:"),
                                                       column(2, tablePullDownTab(a, inputId = "Convert_Tile_Color", label = "Convert"))
                                                     )
                                           )
                          ),
                          helpText("Ability filters select teams with AT LEAST that many non-redundant Abilities (Active Colors + Passives Abilities). For example, a team with 3 Stun will have AT LEAST 3 Stun Abilites in 3 different colors (or passives). The best teams have 6 Active Colors and multiple Passive Abilities."),
                          helpText("Choose 'Select (up to 3):' to build a team based around one or two specific characters (e.g. Featured or Boosted characters).")
)

Health_Panel <- fluidRow(#h5("Health"),
  column(6, 
         sliderInput(inputId = "MaxHP",
                     label =  h5("HP Range (individual):"),
                     min = min.HP,
                     max = max.HP,
                     value = c(min.HP, max.HP)
         )
  ),
  column(6, 
         sliderInput(inputId = "CombinedHP",
                     label =  h5("HP Range (combined):"),
                     min = combined.min.HP,
                     max = combined.max.HP,
                     value = c(combined.min.HP, combined.max.HP)
         )
  )
)

#### Sidebar Panel ####
Sidebar_Panel <- sidebarLayout(
  sidebarPanel(width = 3,
               shinyjs::useShinyjs(),
               id = "sidebar-reset",
               
               #### Unlocked Characters ####
               fluidRow(checkboxInput(inputId = "Unlocked_Characters", label = strong("Unlocked Teams Only"), value = T)),
               #### Rarity Panel ####
               Rarity_Panel('select_rarity'),
               
               fluidRow(radioButtons(inputId="radio", label="Character Select:", choices=list("Exclude:", "Select (up to 3):"), selected="Exclude:")),
               
               conditionalPanel(condition = "input.radio == 'Exclude:'",
                                
                                conditionalPanel(condition = "input.select_rarity.indexOf('5') > -1",
                                                 checkboxGroupInput(inputId = 'exclude_5chars',
                                                                    label =  '5* Characters',
                                                                    choices = all5Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('4') > -1",
                                                 checkboxGroupInput(inputId = 'exclude_4chars',
                                                                    label =  '4* Characters',
                                                                    choices = all4Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('3') > -1",
                                                 checkboxGroupInput(inputId = 'exclude_3chars',
                                                                    label =  '3* Characters',
                                                                    choices = all3Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('2') > -1",
                                                 checkboxGroupInput(inputId = 'exclude_2chars',
                                                                    label =  '2* Characters',
                                                                    choices = all2Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('1') > -1",
                                                 checkboxGroupInput(inputId = 'exclude_1chars',
                                                                    label =  '1* Characters',
                                                                    choices = all1Chars,
                                                                    selected = NULL
                                                 )
                                )
               ),
               
               conditionalPanel(condition = "input.radio == 'Select (up to 3):'", 
                                conditionalPanel(condition = "input.select_rarity.indexOf('5') > -1",
                                                 checkboxGroupInput(inputId = 'select_5chars',
                                                                    label =  '5* Characters',
                                                                    choices = all5Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('4') > -1",
                                                 checkboxGroupInput(inputId = 'select_4chars',
                                                                    label =  '4* Characters',
                                                                    choices = all4Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('3') > -1",
                                                 checkboxGroupInput(inputId = 'select_3chars',
                                                                    label =  '3* Characters',
                                                                    choices = all3Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('2') > -1",
                                                 checkboxGroupInput(inputId = 'select_2chars',
                                                                    label =  '2* Characters',
                                                                    choices = all2Chars,
                                                                    selected = NULL
                                                 )
                                ),
                                conditionalPanel(condition = "input.select_rarity.indexOf('1') > -1",
                                                 checkboxGroupInput(inputId = 'select_1chars',
                                                                    label =  '1* Characters',
                                                                    choices = all1Chars,
                                                                    selected = NULL
                                                 )
                                )
               )
  ), 
  Main_Panel
)


#### Layout 2 ####
shinyUI(
  fluidPage(
    titlePanel("MPQ Team Builder"),
    navbarPage(title = 'Apps',
               tabPanel(title = 'Team Builder', Option_Panel, Health_Panel, Sidebar_Panel),
               tabPanel(title = 'Visualizations (Under Construction)',
                        fluidRow(
                        column(width = 4,
                               Rarity_Panel('select_rarity_color_wheel', inline = T)
                        ),
                        column(width = 8,
                               conditionalPanel(condition = "input.visualization_tab == 'Ability Effects'",
                               tablePullDownTab(a, inputId = "ability", label = "Ability", choices = ability_effect_choices, selected = "Direct_Damage")
                               )
                        )
                        ),
                        sidebarLayout(position = "left",
                                      sidebarPanel(width = 2,
                                                   p("TBD")
                                      ),
                                      mainPanel(width = 10,
                                                tabsetPanel(id = "visualization_tab",
                                                  tabPanel(title = 'Color Wheel',
                                                           helpText("The Color Wheel illustrates each three color combination opposite of its complement. The more even the wheel, the more 6 Active Color Teams that can be constructed."),
                                                           helpText("Note: 1* Characters with less than 3 abilities are not shown"),
                                                           plotOutput(outputId = "color_wheel", height = 400)         
                                                  ),
                                                  tabPanel(title = 'Ability Effects',
                                                           fluidRow(column(10, radioButtons(inline = T, inputId="count_frequency", label=NULL, choices=list("Count", "Frequency"), selected="Frequency"))),
                                                           plotOutput(outputId = "ability_effects", height = 400),
                                                           renderDataTable(output$ability_color_table)
                                                  )
                                                )
                                      )
                        )
               ),
               tabPanel(title = 'Character Database',
                        wellPanel(style = "padding: 5px;", downloadButton(outputId = 'downloadCharacters_csv', label = 'Download "All Characters.csv"')),
                        dataTableOutput('compendium')
               ),
               tabPanel(title = 'Team Database',
                        wellPanel(style = "padding: 5px;", downloadButton(outputId = 'downloadTeams_csv', label = 'Download "All Teams.csv"')),
                        dataTableOutput('team_compendium')
                        #                )
               )
    )
  )
)# End Shiny