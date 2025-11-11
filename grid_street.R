library(grid)
# 1. Create a new drawing page
grid.newpage()

# Set up the 10x10 coordinate system (Viewport)
pushViewport(viewport(
  x = 0.5, y = 0.5, width = 0.9, height = 0.9,
  xscale = c(0, 10), yscale = c(0, 10)
))

# --- 2. Draw the Main Road Surface (A wide rectangle) ---
# The road will be horizontally centered (x=5) and take up a band in the middle (y=3 to y=5).
# Coordinates: xmin=0, xmax=10, ymin=3, ymax=5
grid.rect(
  x = 5,       # Center X
  y = 4,       # Center Y (midway between 3 and 5)
  width = 10,  # Full width of the plot
  height = 2,  # Height from y=3 to y=5
  default.units = "native",
  gp = gpar(fill = "darkgray", col = "black", lwd = 2) # Road color
)

# --- 3. Draw the Center Line (Dashed Segments) ---
# We will draw a series of short, dashed rectangles along the center of the road (y=4).

# Define the pattern for the dashed center line
line_segments_x <- seq(0.5, 9.5, by = 1) # Start X of each segment

# Draw four yellow dashed segments
for (start_x in line_segments_x) {
  grid.rect(
    x = start_x,         # Start X position
    y = 4,               # Center of the road
    width = 0.5,         # Length of the dash segment
    height = 0.1,        # Thickness of the dash
    just = "left",       # Justify to the left (so start_x is the start)
    default.units = "native",
    gp = gpar(fill = "yellow", col = NA) # Yellow fill, no border
  )
}

# --- 4. Draw a Simple Sidewalk (Optional, for context) ---
grid.rect(
  x = 5, y = 5.5, width = 10, height = 1,
  default.units = "native",
  gp = gpar(fill = "lightgray", col = "black")
)

# --- End Viewport ---
popViewport()