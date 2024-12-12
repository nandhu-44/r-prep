######################################################################################################################################
# Load the igraph library
install.packages("igraph")
library(igraph)

# a) Create an adjacency list representation for an undirected graph
# Create an empty graph (undirected)
g <- make_empty_graph(directed = FALSE)

# Define vertices
vertices <- c("A", "B", "C", "D", "E")
g <- add_vertices(g, length(vertices), name = vertices)

# Print the adjacency list representation of the graph
print_adj_list <- function(graph) {
  adj_list <- as_adj_list(graph)
  for (i in seq_along(adj_list)) {
    cat(V(graph)[i]$name, "->", paste(V(graph)[adj_list[[i]]]$name, collapse = ", "), "\n") # nolint
  }
}

cat("Initial adjacency list (empty):\n")
print_adj_list(g)

# b) Implement a function to add an edge between two vertices
add_edge_to_graph <- function(graph, v1, v2) {
  graph <- add_edges(graph, c(v1, v2))
  return(graph)
}

# Add edges between vertices (example: A-B, A-C, B-D, C-E)
g <- add_edge_to_graph(g, "A", "B")
g <- add_edge_to_graph(g, "A", "C")
g <- add_edge_to_graph(g, "B", "D")
g <- add_edge_to_graph(g, "C", "E")

cat("\nAdjacency list after adding edges:\n")
print_adj_list(g)

# c) Write a function to perform DFS traversal
dfs_traversal <- function(graph, start_vertex) {
  visited <- rep(FALSE, vcount(graph))  # Create a visited array initialized to FALSE #nolint
  traversal <- c()
  
  # Helper function for DFS
  dfs <- function(v) {
    visited[v] <<- TRUE
    traversal <<- c(traversal, V(graph)[v]$name)
    
    # Visit all adjacent vertices that have not been visited
    neighbors <- neighbors(graph, v)
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        dfs(neighbor)
      }
    }
  }
  
  # Start DFS from the specified start vertex
  start_index <- which(V(graph)$name == start_vertex)
  dfs(start_index)
  
  return(traversal)
}

# Perform DFS traversal starting from vertex 'A'
cat("\nDFS traversal starting from 'A':\n")
dfs_result <- dfs_traversal(g, "A")
print(dfs_result)

######################################################################################################################################
# Graph
# Create adjacency list representation for an undirected graph
create_adjacency_list <- function(edges) {
  adj_list <- list()
  # Initialize adjacency list for each vertex in edges
  for (edge in edges) {
    v1 <- edge[1]
    v2 <- edge[2]
    # Initialize adjacency lists for each vertex if they don’t exist
    if (is.null(adj_list[[as.character(v1)]]))
      adj_list[[as.character(v1)]] <- c()
    if (is.null(adj_list[[as.character(v2)]]))
      adj_list[[as.character(v2)]] <- c()
    # Add edges (undirected)
    adj_list[[as.character(v1)]] <- c(adj_list[[as.character(v1)]], v2)
    adj_list[[as.character(v2)]] <- c(adj_list[[as.character(v2)]], v1)
  }
  return(adj_list)
}
# Function to add an edge between two vertices in the graph
add_edge <- function(adj_list, v1, v2) {
  # Initialize adjacency lists for each vertex if they don’t exist

  if (is.null(adj_list[[as.character(v1)]]))
    adj_list[[as.character(v1)]] <- c()
  if (is.null(adj_list[[as.character(v2)]]))
    adj_list[[as.character(v2)]] <- c()
  adj_list[[as.character(v1)]] <- c(adj_list[[as.character(v1)]], v2)
  adj_list[[as.character(v2)]] <- c(adj_list[[as.character(v2)]], v1)
  return(adj_list)
}
# DFS traversal on a graph starting from a specific vertex
dfs_traversal <- function(adj_list, start, visited = NULL) {
  # Convert start to character to match list indexing
  start <- as.character(start)
  # Initialize visited with enough entries to cover all
  vertices in adj_list
  if (is.null(visited)) visited <- setNames(rep(FALSE,
                                                length(adj_list)), names(adj_list))
  visited[start] <- TRUE
  cat(start, " ")
  for (neighbor in adj_list[[start]]) {
    neighbor <- as.character(neighbor)
    # Ensure neighbor is a character for indexing
    if (!visited[neighbor]) {
      visited <- dfs_traversal(adj_list, neighbor, visited)
    }
  }
  return(visited)
}
edges <- list(
  c(1, 2), c(1, 5), c(2, 3), c(2, 4),
  c(5, 6), c(6, 7), c(7, 8), c(8, 9),
  c(3, 9), c(3, 10), c(10, 11), c(11, 12),
  c(12, 1))
# Step 1: Create adjacency list
adj_list <- create_adjacency_list(edges)

print("Initial adjacency list:")
print(adj_list)
# Step 2: Add an additional edge to increase complexity
adj_list <- add_edge(adj_list, 9, 12)
print("Adjacency list after adding edge (9, 12):")
print(adj_list)
# Step 3: Perform DFS traversal starting from vertex 1
cat("DFS traversal starting from vertex 1:\n")
visited <- dfs_traversal(adj_list, 1)
