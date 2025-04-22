
### Import Dataset ###
PlayerData <- read.csv("~/Desktop/Projects/NFL Draft/PlayerData.csv")
head(PlayerData)

### Load Necessary Packages ###
library(dplyr)
library(tibble)

#### Column Selection ###
numeric_cols <- PlayerData %>%
  select(Mocks, ESPNBB, PFFBB, Athleticism, Production, DraftScore, PFFGrade, 
         Age, Height, Weight, Attempts, Rec, Ypa)

### Standardizing Data ###
scaled_data <- scale(numeric_cols)
rownames(scaled_data) <- PlayerData$Player

### Euclidean Distance Matrix ###
distance_matrix <- as.matrix(dist(scaled_data, method = "euclidean"))

### Function to get similar players ###
get_comps <- function(player_name, k = 3) {
  if (!(player_name %in% rownames(distance_matrix))) {
    stop("Player not found.")
  }
  distances <- distance_matrix[player_name, ]
  sorted <- sort(distances)
  return(as_tibble(sorted[2:(k + 1)], rownames = "Player")) 
}

### Printing All Comps Individually for 2025 CLASS ###

get_comps("Ashton Jeanty", k = 5)
get_comps("Bhayshul Tuten", k = 5)
get_comps("Brashard Smith", k = 5)
get_comps("Cameron Skattebo", k = 5)
get_comps("Corey Kiner", k = 5)
get_comps("D.J. Giddens", k = 5)
get_comps("Damien Martinez", k = 5)
get_comps("Devin Neal", k = 5)
get_comps("Donovan Edwards", k = 5)
get_comps("Dylan Sampson", k = 5)
get_comps("Ja'Quinden Jackson", k = 5)
get_comps("Jarquez Hunter", k = 5)
get_comps("Jaydon Blue", k = 5)
get_comps("Jo'Quavious Marks", k = 5)
get_comps("Jordan James", k = 6)
get_comps("Kaleb Johnson", k = 5)
get_comps("Kalel Mullings", k = 5)
get_comps("Kyle Monangai", k = 5)
get_comps("LeQuint Allen Jr.", k = 5)
get_comps("Marcus Yarns", k = 5)
get_comps("Ollie Gordon II", k = 5)
get_comps("Omarion Hampton", k = 5)
get_comps("Quinshon Judkins", k = 5)
get_comps("R.J. Harvey Jr.", k = 5)
get_comps("Raheim Sanders", k = 5)
get_comps("Tahj Brooks", k = 5)
get_comps("TreVeyon Henderson", k = 5)
get_comps("Trevor Etienne", k = 5)

### Use euclidean distance values to create formula for similarity %: ###
# (Max Euclidean Distance - Player Distance)/(Max Euclidean Distance - Min Euclidean Distance)

# These calculations were done directly on the Google Sheet
