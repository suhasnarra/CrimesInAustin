library(ggplot2)
library(dplyr)
library(grid) # For unit()

# creates the base scene so we have some constants when the time of day changes
create_base_scene <- function() {
  grass_data <- data.frame(x = 0, y = 0) 
  
  base_plot <- ggplot(grass_data, aes(x = x, y = y)) +
    
    geom_rect(
      xmin = 0, xmax = 10, ymin = 0, ymax = 3,
      fill = "#4CAF50", color = NA 
    ) +
    
    coord_fixed(xlim = c(0, 10), ylim = c(0, 10), expand = FALSE) + 
    
    theme_void() + 
    theme(
      plot.background = element_blank(), 
      panel.border = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"), 
      panel.spacing = unit(c(0, 0, 0, 0), "cm"),
      plot.title = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), hjust = 0.02)
    )
  
  return(base_plot)
}

# function that creates sky for the given time of day
draw_scene_by_mood <- function(scene_name = "mid-day") {
  
  scene_name <- tolower(scene_name)
  p <- create_base_scene()
  
  # change aesthetic based on user input for time of day
  if (scene_name == "midnight") {
    
    sky_color <- "#191970" 
    p <- p + geom_point(
      aes(x = 5.0, y = 7.0), size = 50, shape = 16, color = "white", fill = "white", alpha = 0.8
    ) 
    
  } else if (scene_name == "pre-dawn") {
    
    sky_color <- "#30304D" 
    p <- p + geom_rect(
      xmin = 0, xmax = 10, ymin = 3, ymax = 4.0, fill = "#FF7F50", alpha = 0.1
    ) +
    geom_point(
        aes(x = 7.5, y = 6.0), size = 50, shape = 16, color = "white", fill = "white", alpha = 0.8
      ) 
    
  } else if (scene_name == "sunrise") {
    
    sky_color <- "#4682B4" 
    p <- p + geom_rect(
      xmin = 0, xmax = 10, ymin = 7.5, ymax = 10, fill = "#3B5998", alpha = 0.5
    ) +
      geom_point(
        aes(x = 3.0, y = 3.0), size = 70, shape = 16, color = "#FF4500", fill = "#FF4500", alpha = 0.9 
      )
    
  } else if (scene_name == "morning") {
    
    sky_color <- "#B0E0E6" 
    p <- p + geom_point(
      aes(x = 4.0, y = 7.0), size = 60, shape = 16, color = "yellow", fill = "yellow", alpha = 0.9
    )
    
  } else if (scene_name == "mid-day") {
    
    sky_color <- "#ADD8E6"
    p <- p + geom_point(
      aes(x = 5.0, y = 9.5), size = 60, shape = 16, color = "yellow", fill = "yellow", alpha = 0.9
    )
    
  } else if (scene_name == "afternoon") {
    
    sky_color <- "#C0E0E6" 
    p <- p + geom_point(
      aes(x = 6.0, y = 7.0), size = 60, shape = 16, color = "#FFD700", fill = "#FFD700", alpha = 0.9
    )
    
  } else if (scene_name == "sunset") {
    
    sky_color <- "#FFDAB9" 
    p <- p + geom_rect(
      xmin = 0, xmax = 10, ymin = 3, ymax = 10, fill = "#FF6347", alpha = 0.5
    ) +
      geom_point(
        aes(x = 7.0, y = 3.0), size = 60, shape = 16, color = "#FF4500", fill = "#FF4500", alpha = 0.9
      )
    
  } else if (scene_name == "nightfall") {
    
    sky_color <- "#284664" 
    p <- p + geom_point(
      aes(x = 2.5, y = 6.0), size = 50, shape = 16, color = "#D3D3D3", fill = "#D3D3D3", alpha = 0.7
    )
    
  } else {
    stop("Invalid scene name. Please use one of the eight valid names.")
  }
  p <- p + theme(
    panel.background = element_rect(fill = sky_color, color = NA)
  )
  
  p <- p + labs(title = paste("Scene:", toupper(scene_name)))
  
  return(p)
}

# run function to produce plot
draw_scene_by_mood("nightfall")
