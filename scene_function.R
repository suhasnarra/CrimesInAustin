# Load required libraries
library(shiny)
library(ggplot2)
library(grid)
library(dplyr)
library(scales) # Needed for the color mapping logic from earlier steps

# --- Helper Functions (Provided by User) ---

# Invert color function to make adjusting based on crime frequency easy
invert_color <- function(color) {
  rgb_matrix <- col2rgb(color)
  inverted_matrix <- 255 - rgb_matrix
  new_hex <- rgb(
    red = inverted_matrix[1,],
    green = inverted_matrix[2,],
    blue = inverted_matrix[3,],
    maxColorValue = 255
  )
  return(new_hex)
}

# creates the base scene so we have some constants when the time of day changes
create_base_scene <- function() {
  grass_data <- data.frame(x = 0, y = 0) 
  
  # create grass area
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
draw_scene_by_mood <- function(scene_name = "Mid-Day") {
  
  # set the name and implement base scene that's constant no matter what
  scene_name <- scene_name
  p <- create_base_scene()
  
  # midnight sky
  if (scene_name == "Midnight") {
    sky_color <- "#191970" 
    p <- p + geom_point(
      aes(x = 5.0, y = 7.0), size = 50, shape = 16, color = "white", fill = "white", alpha = 0.8
    ) 
    # pre dawn sky
  } else if (scene_name == "Pre-Dawn") {
    sky_color <- "#30304D" 
    p <- p + geom_rect(
      xmin = 0, xmax = 10, ymin = 3, ymax = 4.0, fill = "#FF7F50", alpha = 0.1
    ) +
      geom_point(
        aes(x = 7.5, y = 6.0), size = 50, shape = 16, color = "white", fill = "white", alpha = 0.8
      ) 
    # sunrise sky
  } else if (scene_name == "Sunrise") {
    sky_color <- "#4682B4" 
    p <- p + geom_rect(
      xmin = 0, xmax = 10, ymin = 7.5, ymax = 10, fill = "#3B5998", alpha = 0.5
    ) +
      geom_point(
        aes(x = 3.0, y = 3.0), size = 70, shape = 16, color = "#FF4500", fill = "#FF4500", alpha = 0.9 
      )
    # morning sky
  } else if (scene_name == "Morning") {
    sky_color <- "#B0E0E6" 
    p <- p + geom_point(
      aes(x = 4.0, y = 7.0), size = 60, shape = 16, color = "yellow", fill = "yellow", alpha = 0.9
    )
    # mid day sky
  } else if (scene_name == "Mid-Day") {
    sky_color <- "#ADD8E6"
    p <- p + geom_point(
      aes(x = 5.0, y = 9.5), size = 60, shape = 16, color = "yellow", fill = "yellow", alpha = 0.9
    )
    # afternoon sky  
  } else if (scene_name == "Afternoon") {
    sky_color <- "#C0E0E6" 
    p <- p + geom_point(
      aes(x = 6.0, y = 7.0), size = 60, shape = 16, color = "#FFD700", fill = "#FFD700", alpha = 0.9
    )
    # sunset sky  
  } else if (scene_name == "Sunset") {
    sky_color <- "#FFDAB9" 
    p <- p + geom_rect(
      xmin = 0, xmax = 10, ymin = 3, ymax = 10, fill = "#FF6347", alpha = 0.5
    ) +
      geom_point(
        aes(x = 7.0, y = 3.0), size = 60, shape = 16, color = "#FF4500", fill = "#FF4500", alpha = 0.9
      )
    # nightfall sky
  } else if (scene_name == "Nightfall") {
    sky_color <- "#284664" 
    p <- p + geom_point(
      aes(x = 2.5, y = 6.0), size = 50, shape = 16, color = "#D3D3D3", fill = "#D3D3D3", alpha = 0.7
    )
  } else {
    stop("Invalid scene name. Please use one of the eight valid names.")
  }
  
  # set the correct sky color
  p <- p + theme(
    panel.background = element_rect(fill = sky_color, color = NA)
  )
  
  # lable the scence for clarity
  p <- p + labs(title = paste("Scene:", scene_name))
  
  return(p)
}

# Parking sign drawing
draw_parking_sign <- function(primary_color = "#00539F", 
                              scale = 1.0, 
                              font_scale = 0.20) {
  
  # border dimensions
  base_width <- 0.45  
  base_height <- 0.45
  final_width <- base_width * scale
  final_height <- base_height * scale * 1.1  
  
  # design/coloring
  sign_white <- "white"
  sign_black <- "black"
  sign_font <- "Arial"
  
  pushViewport(viewport(width = final_width, height = final_height))
  
  grid.roundrect(
    gp = gpar(
      fill = primary_color, 
      col = sign_black,    
      lwd = 2           
    ),
    r = unit(0.05, "snpc") 
  )
  
  v_height_pts <- convertHeight(unit(1, "npc"), "points", valueOnly = TRUE)
  dynamic_fontsize <- .8 * v_height_pts * font_scale
  
  # parking label
  grid.text("PARKING",
            x = 0.5, y = 0.6,
            gp = gpar(
              col = "black",
              fontfamily = sign_font,
              fontface = "bold",
              fontsize = dynamic_fontsize
            )
  )
  
  popViewport()
}

# House
draw_house <- function(house_color = "red", scale = 1.0, show_grid = FALSE) {
  
  # main dimensions for house
  base_size <- 0.5
  final_size <- base_size * scale
  
  door_color <- "darkgray"
  window_color <- invert_color(house_color)
  sign_text_color <- "white"
  
  pushViewport(viewport(
    x = 0.5, y = 0.5, 
    width = final_size,
    height = final_size,
    xscale = c(0, 8),  
    yscale = c(0, 8)  
  ))
  
  if (show_grid) {
    grid.xaxis(at = 0:8, label = FALSE, gp = gpar(col = "lightgray"))
    grid.yaxis(at = 0:8, label = FALSE, gp = gpar(col = "lightgray"))
  }
  
  grid.rect(
    x = 4, y = 2.5,     
    width = 6, height = 4,
    default.units = "native",
    gp = gpar(fill = house_color, col = "black", lwd = 2)
  )
  
  x_roof <- c(1, 7, 4)  
  y_roof <- c(4.5, 4.5, 7)
  grid.polygon(
    x = x_roof, y = y_roof,
    default.units = "native",
    gp = gpar(fill = "brown", col = "black", lwd = 2)
  )
  
  sign_y_pos <- 7.5  
  sign_height <- 0.8  
  
  # main house rectangle
  grid.rect(
    x = 4,
    y = sign_y_pos,
    width = 4,
    height = sign_height,
    default.units = "native",
    gp = gpar(fill = house_color, col = "black", lwd = 2)  
  )
  
  # house label for clarity/understandability
  grid.text(
    "HOUSE",
    x = 4,
    y = sign_y_pos,
    default.units = "native",
    gp = gpar(col = "black", fontface = "bold", cex = .88 * scale)
  )
  
  # create door
  grid.rect(
    x = 4, y = 1.25,      
    width = 1.5, height = 1.5,
    default.units = "native",
    gp = gpar(fill = door_color, col = "black")
  )
  
  # create window
  grid.rect(
    x = 2.5, y = 3,       
    width = 1.5, height = 1.5,
    default.units = "native",
    gp = gpar(fill = window_color, col = "black")
  )
  
  popViewport()
}

# Create road function
draw_road <- function(road_color = "darkgray", scale = 1.0) {
  
  # main design details
  line_color <- ifelse(road_color == "yellow", "black", "yellow")
  base_size <- 0.5
  final_size <- base_size * scale
  
  # color for the sidewalk
  sidewalk_color <- "lightgray"
  
  pushViewport(viewport(
    x = 0.5, y = 0.5, 
    width = final_size,
    height = final_size,
    xscale = c(0, 10),
    yscale = c(0, 10)
  ))
  
  grid.rect(
    x = 5, y = 4, 
    width = 10, height = 2,
    default.units = "native",
    gp = gpar(fill = road_color, col = "black", lwd = 2)
  )
  
  line_segments_x <- seq(0.5, 9.5, by = 1)
  
  # dashes on the road
  for (start_x in line_segments_x) {
    grid.rect(
      x = start_x, 
      y = 4, 
      width = 0.5, 
      height = 0.1,
      just = "left", 
      default.units = "native",
      gp = gpar(fill = line_color, col = NA)
    )
  }
  
  grid.rect(
    x = 5, y = 5.5, width = 10, height = 1,
    default.units = "native",
    gp = gpar(fill = sidewalk_color, col = "black")
  )
  
  popViewport()
}

# Create hotel function
draw_hotel <- function(hotel_color = "#2B3A67", scale = 1.0) {
  
  # design for hotel
  building_color <- hotel_color
  roof_color <- "#888888"
  window_color <- "#FFD700"
  door_color <- "#5a3825"
  sign_text_color <- "white"
  
  center_x <- 0.5
  center_y <- 0.45
  building_width <- 0.5 * scale  
  building_height <- 0.5 * scale  
  
  # main part of hotel
  grid.rect(
    x = unit(center_x, "npc"), 
    y = unit(center_y, "npc"),
    width = unit(building_width, "npc"),
    height = unit(building_height, "npc"),
    gp = gpar(fill = building_color, col = "black")
  )
  
  # roof
  roof_y_base <- center_y + building_height/2
  roof_y_peak <- roof_y_base + 0.15 * scale
  
  grid.polygon(
    x = unit(c(center_x - building_width/2, center_x, center_x + building_width/2), "npc"),
    y = unit(c(roof_y_base, roof_y_peak, roof_y_base), "npc"),
    gp = gpar(fill = roof_color, col = "black")
  )
  
  door_y_center <- center_y - 0.175 * scale 
  
  grid.rect(
    x = unit(center_x, "npc"), 
    y = unit(door_y_center, "npc"),
    width = unit(0.1 * scale, "npc"), 
    height = unit(0.15 * scale, "npc"),
    gp = gpar(fill = door_color, col = "black")
  )
  
  # create windows
  x_positions <- seq(center_x - 0.15*scale, center_x + 0.15*scale, length.out = 3)
  y_positions <- seq(center_y + 0.05*scale, center_y + 0.18*scale, length.out = 2)
  
  for (x in x_positions) {
    for (y in y_positions) {
      grid.rect(
        x = unit(x, "npc"), 
        y = unit(y, "npc"),
        width = unit(0.08 * scale, "npc"), 
        height = unit(0.1 * scale, "npc"),
        gp = gpar(fill = window_color, col = "black")
      )
    }
  }
  
  sign_y_pos <- roof_y_peak - 0.05 * scale 
  
  grid.rect(
    x = unit(center_x, "npc"), 
    y = unit(sign_y_pos, "npc"),
    width = unit(0.3 * scale, "npc"), 
    height = unit(0.07 * scale, "npc"),
    gp = gpar(fill = building_color, col = "white", lwd = 2)
  )
  
  # label hotel
  grid.text(
    "HOTEL", 
    x = unit(center_x, "npc"), 
    y = unit(sign_y_pos, "npc"),
    gp = gpar(col = "black", fontface = "bold", cex = 1.4 * scale)
  )
}

# Create office function
draw_office <- function(office_color = "gray80", scale = 1.0) {
  
  # build the main building
  building_width <- 0.5 * scale  
  building_height <- 0.5 * scale 
  center_x <- 0.5
  center_y <- 0.5
  
  grid.rect(
    x = center_x, y = center_y,
    width = building_width, height = building_height,
    gp = gpar(fill = office_color, col = "black")
  )
  
  
  sign_height <- building_height * 0.12
  sign_y_pos <- center_y + building_height/2 + sign_height/2
  
  grid.rect(
    x = center_x,
    y = sign_y_pos,
    width = building_width * 0.7,
    height = sign_height,
    gp = gpar(fill = office_color, col = "black")
  )
  
  # office label
  grid.text(
    "OFFICE",
    x = center_x,
    y = sign_y_pos,
    gp = gpar(col = "black", fontface = "bold", fontsize = 14 * scale)
  )
  
  # window dimensions
  window_width <- building_width / 5
  window_height <- building_height / 6
  
  # Set up all the windowns in the correct locations
  for (i in 1:3) {
    for (j in 1:3) {
      window_x <- center_x - building_width/3 + (j-1)*(building_width/3)
      window_y <- center_y - building_height/4 + (i-1)*(building_height/4)
      
      grid.rect(
        x = window_x,
        y = window_y,
        width = window_width, 
        height = window_height,
        gp = gpar(fill = "lightblue", col = "black")
      )
    }
  }
}


# Main function to combine everything into one scene
compose_scene_by_time <- function(scene_name = "Afternoon", df = my_data) {
  
  # gets color and scale for an object
  get_object_value <- function(obj_name, column_name) {
    # Look at specific row (time, object)
    # Use sym() for column_name because it's passed as a string
    value <- df %>%
      filter(object == obj_name & timeOfDay == scene_name) %>%
      pull(!!sym(column_name))
    
    # Return values or error message if nothing found
    if (length(value) == 0) {
      # This happens if my_data is not set up correctly
      # Returning defaults to allow the app to run if data is missing
      if (column_name == "new_color") return("black")
      if (column_name == "scale") return(1.0)
    }
    return(value[1])
  }
  
  # Get specific color and scale for each object
  road_color <- get_object_value("road", "new_color")
  road_scale <- get_object_value("road", "scale")
  
  house_color <- get_object_value("house", "new_color")
  house_scale <- get_object_value("house", "scale")
  
  office_color <- get_object_value("office", "new_color")
  office_scale <- get_object_value("office", "scale")
  
  hotel_color <- get_object_value("hotel", "new_color")
  hotel_scale <- get_object_value("hotel", "scale")
  
  parking_color <- get_object_value("parking", "new_color")
  parking_scale <- get_object_value("parking", "scale")
  
  # Draw the full scene
  background <- draw_scene_by_mood(scene_name)
  
  # Use print(background) within a renderPlot to draw the ggplot object
  # Then use grid.force() and grid.draw() to add grid objects on top
  # We must use the 'grid' package drawing inside the renderPlot function below.
  
  # Return the ggplot object and the drawing instructions for use in the server
  
  return(list(
    background = background,
    draw_calls = function() {
      # Draw road
      # FIX: Reduced width from 1.5 to 0.8 and height to 0.2 to prevent bleed
      pushViewport(viewport(x = 0.5, y = 0.15, width = 1.2, height = 0.4)) 
      draw_road(road_color = road_color, scale = road_scale)
      popViewport()
      
      # Draw parking sign (width 0.12, height 0.12 is likely okay)
      pushViewport(viewport(x = 0.18, y = 0.32, width = 0.22, height = 0.22))
      draw_parking_sign(primary_color = parking_color, scale = parking_scale * 2)
      popViewport()
      
      # Draw house (width 0.18, height 0.25 is likely okay)
      pushViewport(viewport(x = 0.4, y = 0.34, width = 0.3, height = 0.36))
      draw_house(house_color = house_color, scale = house_scale)
      popViewport()
      
      # Draw office (width 0.18, height 0.28 is likely okay)
      pushViewport(viewport(x = 0.7, y = 0.32, width = 0.3, height = 0.4))
      draw_office(office_color = office_color, scale = office_scale)
      popViewport()
      
      # Draw hotel (width 0.18, height 0.28 is likely okay)
      pushViewport(viewport(x = 0.85, y = 0.33, width = 0.3, height = 0.4))
      draw_hotel(hotel_color = hotel_color, scale = hotel_scale)
      popViewport()
    }
  ))
}


# --- Shiny Application Function ---

run_crime_scene_app <- function(df_data) {
  
  word_levels <- c(
    "Pre-Dawn" = 1, "Sunrise" = 2, "Morning" = 3, "Mid-Day" = 4, 
    "Afternoon" = 5, "Sunset" = 6, "Nightfall" = 7, "Midnight" = 8
  )
  word_names <- names(word_levels)
  num_levels <- length(word_levels)
  
  # Calculate positions (0% to 100%) for 8 labels
  # (Example: 0, 14.28, 28.57, 42.85, 57.14, 71.42, 85.71, 100)
  label_positions <- seq(0, 100, length.out = num_levels)
  
  # Define UI
  ui <- fluidPage(
    
    titlePanel("Crime in Austin Killer Plot"),
    
    # --- 1. Plot Row (Top) ---
    fluidRow(
      # Set the column width to 12 (full width)
      column(8, offset = 2,
             # Increase the height of the plotOutput to make the image bigger
             plotOutput("distPlot", height = "850px") 
      )
    ),
    
    # --- 2. Controls Row (Bottom) ---
    fluidRow(
      column(6, offset = 3,
             sliderInput(
               inputId = "time_of_day", 
               label = "Select a time:",
               min = min(word_levels),
               max = max(word_levels),
               value = word_levels["Pre-Dawn"], 
               step = 1,
               # CRITICAL: Hides the default numeric ticks/labels
               ticks = FALSE, 
               width = "100%" 
             ),
             
             # 2. Display the word labels using a div
             tags$div(
               style = "width: 100%; position: relative; margin-top: -15px;", # Adjust margin to bring labels close
               lapply(1:num_levels, function(i) {
                 tags$div(
                   style = paste0(
                     "position: absolute; ",
                     "left: ", label_positions[i], "%; ",
                     "transform: translateX(-50%); ", # Center the text block over the position
                     "font-size: 10px; text-align: center; white-space: nowrap;"
                   ),
                   word_names[i]
                 )
               })
             )
      )
    )
  )
  
  # Define server logic
  server <- function(input, output, session) {
    
    # Reactive expression to map the numeric slider index to the word value
    selected_word_reactive <- reactive({
      # Get the numeric index (1-8)
      index <- input$time_of_day
      
      # Map the index to the corresponding word
      names(word_levels)[word_levels == index]
    })
    
    # Display the selected word
    output$selected_word_output <- renderText({
      selected_word_reactive()
    })
    
    # Render the combined plot
    output$distPlot <- renderPlot({
      
      current_time <- selected_word_reactive()
      
      # 1. Compose the scene based on the selected time and data
      scene_elements <- compose_scene_by_time(scene_name = current_time, df = df_data)
      
      # 2. Draw the ggplot base scene first
      print(scene_elements$background)
      
      # 3. Draw the grid objects on top of the ggplot object
      # You need to call grid.force() and grid.draw() to ensure the base plot is rendered
      # before adding the grid elements.
      grid.force()
      
      # Call the function containing all the grid drawing commands
      scene_elements$draw_calls()
      
    }, res = 96) # Standard resolution for web display
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
}

my_data <- read.csv("killerDF.csv")
run_crime_scene_app(df_data = my_data)

