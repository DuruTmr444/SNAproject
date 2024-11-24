# Install R packages if not installed -------------------------------------

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidygraph)) install.packages("tidygraph")
if (!require(visNetwork)) install.packages("visNetwork")

# Load R packages ---------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(visNetwork)

# Read the dataset --------------------------------------------------------

data_url <- "https://raw.githubusercontent.com/katie-truong/Jupyter/master/movie_metadata.csv"

movies <- read_csv(data_url)
#glimpse(movies) 

# Clean the data -----------------------------------
critical_columns <- c("movie_title", "title_year", "imdb_score", "gross")
movies <- movies %>%
  distinct() %>%
  select(where(~ sum(is.na(.)) < 0.5 * nrow(movies))) %>%
  filter(across(all_of(critical_columns), ~ !is.na(.)))
glimpse(movies) 

# Select top 50 movies by gross revenue -----------------------------------

movies_top50 <- movies %>%
  filter(!is.na(gross)) %>%          # Remove movies with missing gross revenue
  arrange(desc(gross)) %>%           # Sort movies by descending gross revenue
  slice(1:50)                        # Select the top 50 movies

# Select relevant columns -------------------------------------------------

movies_sub <- movies_top50 %>%
  select(movie_title, actor_1_name, actor_2_name, actor_3_name)

# Create edge list: movie-actor pairs -------------------------------------

edges <- movies_sub %>%
  pivot_longer(cols = c(actor_1_name, actor_2_name, actor_3_name),
               names_to = "actor_number",
               values_to = "actor_name") %>%
  select(movie_title, actor_name) %>%
  filter(!is.na(actor_name))

# Create nodes ------------------------------------------------------------

actor_nodes <- edges %>%
  distinct(actor_name) %>%
  dplyr::rename(name = actor_name) %>%
  mutate(group = "Actor")

movie_nodes <- edges %>%
  distinct(movie_title) %>%
  dplyr::rename(name = movie_title) %>%
  mutate(group = "Movie")

nodes <- bind_rows(actor_nodes, movie_nodes) %>%
  mutate(id = row_number())

# Create edges -----------------------------------------------

name_to_id <- nodes %>%
  select(name, id)

edges <- edges %>%
  left_join(name_to_id, by = c("movie_title" = "name")) %>%
  rename(from = id) %>%
  left_join(name_to_id, by = c("actor_name" = "name")) %>%
  rename(to = id) %>%
  select(from, to)

# Prepare data for visNetwork ---------------------------------------------

nodes_vis <- nodes %>%
  select(id, label = name, group) %>%
  mutate(color = if_else(group == "Actor", "blue", "yellow"))

edges_vis <- edges %>%
  mutate(color = "darkblue")

# Create interactive network ----------------------------------------------

visNetwork(nodes = nodes_vis, edges = edges_vis, 
           width = "100%", height = "600px",
           main = "Actor-Movie Collaboration Network (Top 50 Movies)") %>%
  visLayout(randomSeed = 1000) %>%
  addFontAwesome()

# Create interactive network with symbols----------------------------------------------

visNetwork(nodes = nodes_vis, edges = edges_vis, 
           width = "100%", height = "600px",
           main = "Actor-Movie Collaboration Network (Top 50 Movies)") %>%
  visLayout(randomSeed = 1000) %>%
  addFontAwesome() %>%
  visGroups(groupname = "Movie", shape = "icon",
            icon = list(face = 'FontAwesome', code = "f008", color = "darkblue")) %>%
  visGroups(groupname = "Actor", shape = "icon",
            icon = list(face = 'FontAwesome', code = "f007", color = "red")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), 
             nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)

# Top 50 highest paid actors----------------------------------------------

top_actors <- movies %>%
  filter(!is.na(actor_1_facebook_likes)) %>%    # Remove entries with missing actor_1_facebook_likes
  group_by(actor_1_name) %>%                    # Group by actor name
  summarise(max_likes = max(actor_1_facebook_likes)) %>%  # Get the max likes for each actor
  arrange(desc(max_likes)) %>%                  # Sort actors by descending max_likes
  slice(1:20)                                   # Select the top 50 actors

# Filter movies that involve these top actors ------------------------------

movies_top_actors <- movies %>%
  filter(actor_1_name %in% top_actors$actor_1_name |
           actor_2_name %in% top_actors$actor_1_name |
           actor_3_name %in% top_actors$actor_1_name)

movies_sub2 <- movies_top_actors %>%
  select(movie_title, actor_1_name, actor_2_name, actor_3_name)

edges2 <- movies_sub2 %>%
  pivot_longer(cols = starts_with("actor_"),
               names_to = "actor_number",
               values_to = "actor_name") %>%
  select(movie_title, actor_name) %>%
  filter(!is.na(actor_name))

## Create nodes
actor_nodes2 <- edges2 %>%
  distinct(actor_name) %>%
  dplyr::rename(name = actor_name) %>%
  mutate(group = "Actor")

movie_nodes2 <- edges2 %>%
  distinct(movie_title) %>%
  dplyr::rename(name = movie_title) %>%
  mutate(group = "Movie")

nodes2 <- bind_rows(actor_nodes2, movie_nodes2) %>%
  mutate(id = row_number())

## Map names to IDs in edges
name_to_id2 <- nodes2 %>%
  select(name, id)

edges2 <- edges2 %>%
  left_join(name_to_id2, by = c("movie_title" = "name")) %>%
  rename(from = id) %>%
  left_join(name_to_id2, by = c("actor_name" = "name")) %>%
  rename(to = id) %>%
  select(from, to)

## Prepare data for visNetwork
nodes_vis2 <- nodes2 %>%
  select(id, label = name, group) %>%
  mutate(color = if_else(group == "Actor", "blue", "yellow"))

edges_vis2 <- edges2 %>%
  mutate(color = "darkblue")

# Create interactive network ----------------------------------------------

visNetwork(nodes = nodes_vis2, edges = edges_vis2, 
           width = "100%", height = "600px",
           main = "Actor-Movie Collaboration Network (Top 20 Actors)") %>%
  visLayout(randomSeed = 1000) %>%
  addFontAwesome()

#von den Einnahmen ausgehend die Likes als Gewichtung