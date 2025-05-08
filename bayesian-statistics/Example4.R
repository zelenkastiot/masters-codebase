library(data.tree)
library(DiagrammeR)

# Create the root of the tree
tree <- Node$new("Start")

# Level 1: Coin choice
fair <- tree$AddChild("Fair Coin (θ=0.5)")
biased <- tree$AddChild("Biased Coin (θ=0.7)")
fair$prob <- 0.5
biased$prob <- 0.5

# Level 2: First Flip Outcomes
fair_H <- fair$AddChild("Head (0.5)")
fair_T <- fair$AddChild("Tail (0.5)")
fair_H$prob <- 0.5
fair_T$prob <- 0.5

biased_H <- biased$AddChild("Head (0.7)")
biased_T <- biased$AddChild("Tail (0.3)")
biased_H$prob <- 0.7
biased_T$prob <- 0.3

# Optional: You can also add second flip if you want a 3-level tree
# Skipping for clarity unless requested

# Print structure
tree$Do(function(node) {
  if (!node$isRoot) {
    node$label <- paste0(node$name, "\nP=", round(node$prob, 3))
  }
})

# Convert to DiagrammeR graph
SetGraphStyle(tree, rankdir = "LR")
SetEdgeStyle(tree, penwidth = 2)
SetNodeStyle(
  tree,
  style = "filled,rounded",
  shape = "box",
  fillcolor = "LightBlue"
)

plot(tree)
