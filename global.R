#shinyapps::deployApp('/Users/alexplocik/Desktop/MPQ/MPQ_Character_Selection_V3/')
#shinyapps::deployApp('/Users/alexplocik/Desktop/MPQ/MPQ_Team_Builder/')
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(reshape2)

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

all5Chars <- c %>% filter(Rarity == 5) %>% select(CharacterName) %>% unique() %>% as.matrix()
names(all5Chars) <- c %>% filter(Rarity == 5) %>% select(CharacterName1) %>% unique() %>% as.matrix()

all4Chars <- c %>% filter(Rarity == 4) %>% select(CharacterName) %>% unique() %>% as.matrix()
names(all4Chars) <- c %>% filter(Rarity == 4) %>% select(CharacterName1) %>% unique() %>% as.matrix()

all3Chars <- c %>% filter(Rarity == 3) %>% select(CharacterName) %>% unique() %>% as.matrix()
names(all3Chars) <- c %>% filter(Rarity == 3) %>% select(CharacterName1) %>% unique() %>% as.matrix()

all2Chars <- c %>% filter(Rarity == 2) %>% select(CharacterName) %>% unique() %>% as.matrix()
names(all2Chars) <- c %>% filter(Rarity == 2) %>% select(CharacterName1) %>% unique() %>% as.matrix()

all1Chars <- c %>% filter(Rarity == 1) %>% select(CharacterName) %>% unique() %>% as.matrix()
names(all1Chars) <- c %>% filter(Rarity == 1) %>% select(CharacterName1) %>% unique() %>% as.matrix()



