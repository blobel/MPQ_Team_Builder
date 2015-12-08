#shinyapps::deployApp('/Users/alexplocik/Desktop/MPQ/MPQ_Character_Selection_V3/')
#shinyapps::deployApp('/Users/alexplocik/Desktop/MPQ/MPQ_Team_Builder/')
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)

# load data
a <- read.csv("Teams_Compendium.csv", row.names = 1) %>%
  tbl_df()
# b <- read.csv("Teams.csv", row.names = 1) %>%
#   tbl_df()
c <- read.csv("Compendium.csv", row.names = 1) %>%
  tbl_df() %>%
  arrange(desc(Rarity), CharacterName)

# Factor levels for ui and server
allRarity <- levels(factor(c(a$Rarity.Char1, a$Rarity.Char2, a$Rarity.Char3)))
allChars <- c %>% select(CharacterName) %>% unique() %>% as.matrix()
all5Chars <- c %>% filter(Rarity == 5) %>% select(CharacterName) %>% unique() %>% as.matrix(); names(all5Chars) <- c %>% filter(Rarity == 5) %>% select(CharacterName1) %>% unique() %>% as.matrix()
all4Chars <- c %>% filter(Rarity == 4) %>% select(CharacterName) %>% unique() %>% as.matrix(); names(all4Chars) <- c %>% filter(Rarity == 4) %>% select(CharacterName1) %>% unique() %>% as.matrix()
all3Chars <- c %>% filter(Rarity == 3) %>% select(CharacterName) %>% unique() %>% as.matrix(); names(all3Chars) <- c %>% filter(Rarity == 3) %>% select(CharacterName1) %>% unique() %>% as.matrix()
all2Chars <- c %>% filter(Rarity == 2) %>% select(CharacterName) %>% unique() %>% as.matrix(); names(all2Chars) <- c %>% filter(Rarity == 2) %>% select(CharacterName1) %>% unique() %>% as.matrix()
all1Chars <- c %>% filter(Rarity == 1) %>% select(CharacterName) %>% unique() %>% as.matrix(); names(all1Chars) <- c %>% filter(Rarity == 1) %>% select(CharacterName1) %>% unique() %>% as.matrix()

#### Functions ####
string2integer <- function(x){if(x=="yes"){1}else{0}}
sapply_string2integer <- function(x){sapply(x, string2integer)}
#### Functions ####

MPQ_color_scale <- scale_color_manual(values = c("black" = "gray10", "blue" = "royalblue", "green" = "forestgreen", "purple" = "darkorchid", "red" = "darkred", "yellow" = "goldenrod"),
                                     limits = c("black", "blue", "green", "purple", "red", "yellow"), drop = FALSE)
MPQ_fill_scale <- scale_fill_manual(values = c("black" = "gray10", "blue" = "royalblue", "green" = "forestgreen", "purple" = "darkorchid", "red" = "darkred", "yellow" = "goldenrod"),
                                     limits = c("black", "blue", "green", "purple", "red", "yellow"), drop = FALSE)

