blank <- 0; names(blank) = "------ Preset Teams ------"
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
Rarity_Panel <- checkboxGroupInput(inputId = 'select_rarity',
                                   inline = F,
                                   label = 'Tier',
                                   choices = rev(allRarity),
                                   selected = c(4,3)
)

#### Main Panel function ####
Main_Panel <-  mainPanel(width = 9,
                         tabsetPanel(
                           tabPanel(title = 'Team', dataTableOutput(outputId = 'mytable1')),
                           tabPanel(title = 'Damage Curves', plotOutput(outputId = "ggPlot1")),
                           tabPanel(title = 'Max Damage', plotOutput(outputId = "ggPlot2")),
                           tabPanel(title = 'Max Abilities', plotOutput(outputId = "ggPlot3"))
                         )
)

#### Option Panel ####

Option_Panel <- fluidPage(shinyjs::useShinyjs(),
                          id = "options-reset",
                          fluidRow(
                            
                            #### Download button ####
                            column(width = 4, 
                                   wellPanel(downloadButton(outputId = 'downloadTeamBuilder_csv', label = 'Download "Team_Builds.csv"'))
                            ),
                            #### Preset Teams ####
                            column(width = 8, 
                                   selectInput(inputId = "Preset_Teams", label = NULL, width = "100%",
                                               choices = c(blank,
                                                           "--- Rainbow Teams with 3 (or more) Passive Abilities ---",
                                                           "3* / 4* Rarity - 6 Actives & 3 (or more) Passives",
                                                           "2* / 3* Rarity - 6 Actives & 3 (or more) Passives",
                                                           "--- Ability Effect Teams ---",
                                                           "Stunning!: 3 Stun Abilities in 3 different colors (or from passives)",
                                                           "Anti-Kishu/Thug (1 Stun and 1 Special Tile Remove, Steal, Improve, and Protect)",
                                                           "--- Required Character Teams ---",
                                                           "Venom: Infinite Damage (6 Active Colors & 2 Web Tile Actives)",
                                                           "Carnage: Stayin' Alive (5 Active Colors & 2 Healing Abilities)",
                                                           "X-Force: Shred the Board 6 Active Colors & 5 Board Shake Abilities)",
                                                           "Kamala Khan: Heal and Deal (6 Active Colors & 3 Fast & 3 Damaging Abilities)",
                                                           "Kingpin: Poke (3 2-Turn CDs Abilies to Finger Poke)",
                                                           "Deadpool! (You can't play these teams!)",
                                                           "--- Other ---",
                                                           "SKIP!"
                                               ),
                                               selected = 0)
                            ),
                            column(width = 6, 
                                   actionButton(inputId = "reset_input", label = "Reset Settings"))
                          ),
                          helpText("Filter teams below using the Rarity, Ability, and Character filters. Try the Preset Teams (above) to get ideas!"),
                          
                          #### Rarity Panel ####
                          fluidRow(width = 12, 
                                   column(width = 1, h5("Rarity:"),
                                          Rarity_Panel),
                                   #### Abilities ####
                                   column(width = 2, h5("Abilities:"),
                                          tablePullDownTab(a, inputId = "Actives", label = "Active Colors"),
                                          tablePullDownTab(a, inputId = "Passives", label = "Passive Abilities"),
                                          tablePullDownTab(a, inputId = "Cheap", label = "Fast ( < 7 AP )"),
                                          checkboxInput(inputId = "Unlocked_Characters", label = "Unlocked Teams Only", value = T)
                                          
                                   ),
                                   
                                   column(width = 1, h5("Damage:"),
                                          #            tablePullDownTab(a, inputId = "Offensive", label = "Any:"),
                                          tablePullDownTab(a, inputId = "Possible_Damage", label = "Any"),
                                          tablePullDownTab(a, inputId = "Direct_Damage", label = "Direct"),
                                          tablePullDownTab(a, inputId = "Team_Damage", label = "Team")
                                          
                                   ),
                                   
                                   column(width = 1, h5("Defense:"),
                                          tablePullDownTab(a, inputId = "Defensive", label = "Any"),
                                          tablePullDownTab(a, inputId = "Stun", label = "Stun"),
                                          tablePullDownTab(a, inputId = "Shake", label = "Shake"),
                                          tablePullDownTab(a, inputId = "Healing", label = "Heal")
                                   ),
                                   
                                   column(width = 1, h5("AP:"),
                                          tablePullDownTab(a, inputId = "Manipulate_AP", label = "Any:"),
                                          tablePullDownTab(a, inputId = "Steal_AP", label = "Steal"),
                                          tablePullDownTab(a, inputId = "Gain_AP", label = "Gain"),
                                          tablePullDownTab(a, inputId = "Destroy_AP", label = "Destroy")
                                   ),
                                   
                                   column(width = 1, h5("Special:"),
                                          tablePullDownTab(a, inputId = "Add_Special_Tile", label = "Add"),
                                          tablePullDownTab(a, inputId = "Remove_Special_Tile", label = "Remove"),
                                          tablePullDownTab(a, inputId = "Improve_Special_Tile", label = "Improve"),
                                          tablePullDownTab(a, inputId = "Steal_Special_Tile", label = "Steal")
                                   ),
                                   
                                   column(width = 1, h5("---"),
                                          tablePullDownTab(a, inputId = "Add_Strike_Tile", label = "Strike"),
                                          tablePullDownTab(a, inputId = "Add_Attack_Tile", label = "Attack"),
                                          tablePullDownTab(a, inputId = "Add_Protect_Tile", label = "Protect"),
                                          tablePullDownTab(a, inputId = "Add_Charged_Tile", label = "Charged")
                                   ),
                                   
                                   column(width = 1, h5("Tile:"),
                                          tablePullDownTab(a, inputId = "Convert_Tile_Color", label = "Convert"),
                                          tablePullDownTab(a, inputId = "Add_CD_Tile", label = "CD"),
                                          tablePullDownTab(a, inputId = "Add_2_Turn_CD_Tile", label = "2TurnCD"),
                                          tablePullDownTab(a, inputId = "Add_Trap_Tile", label = "Trap")
                                   ),
                                   
                                   column(width = 1, h5("---"),
                                          tablePullDownTab(a, inputId = "Add_Critical_Tile", label = "Critical"),
                                          tablePullDownTab(a, inputId = "Add_Locked_Tile", label = "Locked"),
                                          tablePullDownTab(a, inputId = "Add_Invis_Tile", label = "Invisible"),
                                          tablePullDownTab(a, inputId = "Add_Web_Tile", label = "Web")
                                   ),
                                   
                                   column(width = 2, h5("Health"),
                                          sliderInput(inputId = "MaxHP",
                                                      label =  h5("HP Range (individual):"),
                                                      min = min.HP,
                                                      max = max.HP,
                                                      value = c(min.HP, max.HP)
                                          ),
                                          sliderInput(inputId = "CombinedHP",
                                                      label =  h5("HP Range (combined):"),
                                                      min = combined.min.HP,
                                                      max = combined.max.HP,
                                                      value = c(combined.min.HP, combined.max.HP)
                                          )
                                   )
                          ),
                          helpText("Ability filters select teams with AT LEAST that many non-redundant Abilities (Active Colors + Passives Abilities). For example, a team with 3 Stun will have AT LEAST 3 Stun Abilites in 3 different colors (or passives). The best teams have 6 Active Colors and multiple Passive Abilities."),
                          helpText("Choose 'Select (up to 3):' to build a team based around one or two specific characters (e.g. Featured or Boosted characters).")
                          
) 
#### Sidebar Panel ####
Sidebar_Panel <- sidebarLayout(
  sidebarPanel(width = 3,
               shinyjs::useShinyjs(),
               id = "sidebar-reset",
               radioButtons(
                 inputId="radio",
                 label="Character Select:",
                 choices=list(
#                    "Include:",
                   "Exclude:",
                   "Select (up to 3):"
                 ),
                 selected="Exclude:"),
               
#                conditionalPanel(condition = "input.radio == 'Include:'",
#                                 conditionalPanel(condition = "input.select_rarity.indexOf('5') > -1",
#                                                  checkboxGroupInput(inputId = 'include_5chars',
#                                                                     label =  '5* Characters',
#                                                                     choices = all5Chars,
#                                                                     select = all5Chars
#                                                  )
#                                 ),
#                                 conditionalPanel(condition = "input.select_rarity.indexOf('4') > -1",
#                                                  checkboxGroupInput(inputId = 'include_4chars',
#                                                                     label =  '4* Characters',
#                                                                     choices = all4Chars,
#                                                                     select = all4Chars
#                                                  )
#                                 ),
#                                 conditionalPanel(condition = "input.select_rarity.indexOf('3') > -1",
#                                                  checkboxGroupInput(inputId = 'include_3chars',
#                                                                     label =  '3* Characters',
#                                                                     choices = all3Chars,
#                                                                     select = all3Chars
#                                                  )
#                                 ),
#                                 conditionalPanel(condition = "input.select_rarity.indexOf('2') > -1",
#                                                  checkboxGroupInput(inputId = 'include_2chars',
#                                                                     label =  '2* Characters',
#                                                                     choices = all2Chars,
#                                                                     select = all2Chars
#                                                  )
#                                 ),
#                                 conditionalPanel(condition = "input.select_rarity.indexOf('1') > -1",
#                                                  checkboxGroupInput(inputId = 'include_1chars',
#                                                                     label =  '1* Characters',
#                                                                     choices = all1Chars,
#                                                                     select = all1Chars
#                                                  )
#                                 )
#                ),
               
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
    navbarPage(
      title = 'Apps',
      tabPanel(title = 'Team Builder', Option_Panel, Sidebar_Panel),
      tabPanel(title = 'Graphs (Coming Soon)'),
      tabPanel(title = 'Character Database',
               wellPanel(downloadButton(outputId = 'downloadCharacters_csv', label = 'Download "All Characters.csv"')),
               dataTableOutput('compendium')
      ),
      tabPanel(title = 'Team Database',
               wellPanel(downloadButton(outputId = 'downloadTeams_csv', label = 'Download "All Teams.csv"')),
               dataTableOutput('team_compendium')
      )
    )
  )
) # End Shiny

