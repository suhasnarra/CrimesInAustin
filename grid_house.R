library(tidyverse)
library(grid)

# 1. Create a new drawing page
grid.newpage()

# Define coordinates for the house elements on a conceptual grid (0 to 10 for simplicity)
# Note: In 'grid' default units (npc), 0 to 1 spans the viewport, but we can scale.
# For simplicity, we'll use a custom viewport to easily map 0-10 to the plot area.

# Create a custom viewport to establish a 10x10 coordinate system
pushViewport(viewport(
  x = 0.5, y = 0.5, width = 0.8, height = 0.8, # Centered, 80% of the page
  xscale = c(0, 10), yscale = c(0, 10) # Set coordinate limits from 0 to 10
))

# --- Draw the Grid Lines (Optional, but helpful for "grid drawing") ---
grid.xaxis(at = 0:10, label = FALSE, gp = gpar(col = "lightgray"))
grid.yaxis(at = 0:10, label = FALSE, gp = gpar(col = "lightgray"))

# --- Draw the House Elements ---

# 2. Draw the main body of the house (a rectangle)
grid.rect(
  x = 5, y = 3, # Center of the rectangle
  width = 6, height = 4, # Dimensions (e.g., from x=2 to x=8, y=1 to y=5)
  default.units = "native",
  gp = gpar(fill = "red", col = "black", lwd = 2)
)

# 3. Draw the roof (a polygon - triangle shape)
# Coordinates for the three corners of the triangle: (2,5), (8,5), (5,8)
x_roof <- c(2, 8, 5)
y_roof <- c(5, 5, 8)
grid.polygon(
  x = x_roof, y = y_roof,
  default.units = "native",
  gp = gpar(fill = "red", col = "black", lwd = 2)
)

# 4. Draw the door (a smaller rectangle)
grid.rect(
  x = 5, y = 1.75, # Center of the door
  width = 1.5, height = 1.5, # Dimensions
  default.units = "native",
  gp = gpar(fill = "darkgray", col = "black")
)

# 5. Draw a window (a square)
grid.rect(
  x = 3.5, y = 3.5, # Center of the window
  width = 1.5, height = 1.5, # Dimensions
  default.units = "native",
  gp = gpar(fill = "yellow", col = "black")
)

# 6. Go back to the original viewport
popViewport()