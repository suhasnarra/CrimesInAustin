# Load required libraries
library(shiny)
library(grid)
library(dplyr)

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

# function that creates sky for the given time of day
draw_scene_by_mood <- function(scene_name = "Mid-Day") {
  
  # Create a new grid page
  grid::grid.newpage()
  
  # Set up viewport
  grid::pushViewport(grid::viewport(
    xscale = c(0, 10),
    yscale = c(0, 10),
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc")
  ))
  
  # Determine background based on scene
  if (scene_name == "Midnight") {
    sky_color <- "#191970"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw moon
    grid::grid.circle(
      x = grid::unit(5.0, "native"),
      y = grid::unit(7.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "white", col = NA, alpha = 0.8)
    )
    
  } else if (scene_name == "Pre-Dawn") {
    sky_color <- "#30304D"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw orange glow
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(3.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(1, "native"),
      gp = grid::gpar(fill = "#FF7F50", col = NA, alpha = 0.1)
    )
    
    # Draw moon
    grid::grid.circle(
      x = grid::unit(7.5, "native"),
      y = grid::unit(6.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "white", col = NA, alpha = 0.8)
    )
    
  } else if (scene_name == "Sunrise") {
    sky_color <- "#4682B4"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw darker blue top
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(8.75, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(2.5, "native"),
      gp = grid::gpar(fill = "#3B5998", col = NA, alpha = 0.5)
    )
    
    # Draw sun
    grid::grid.circle(
      x = grid::unit(3.0, "native"),
      y = grid::unit(3.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "#FF4500", col = NA, alpha = 0.9)
    )
    
  } else if (scene_name == "Morning") {
    sky_color <- "#B0E0E6"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw sun
    grid::grid.circle(
      x = grid::unit(4.0, "native"),
      y = grid::unit(7.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "yellow", col = NA, alpha = 0.9)
    )
    
  } else if (scene_name == "Mid-Day") {
    sky_color <- "#ADD8E6"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw sun
    grid::grid.circle(
      x = grid::unit(5.0, "native"),
      y = grid::unit(9.5, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "yellow", col = NA, alpha = 0.9)
    )
    
  } else if (scene_name == "Afternoon") {
    sky_color <- "#C0E0E6"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw sun
    grid::grid.circle(
      x = grid::unit(6.0, "native"),
      y = grid::unit(7.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "#FFD700", col = NA, alpha = 0.9)
    )
    
  } else if (scene_name == "Sunset") {
    sky_color <- "#FFDAB9"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw red-orange gradient
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(6.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(7, "native"),
      gp = grid::gpar(fill = "#FF6347", col = NA, alpha = 0.5)
    )
    
    # Draw sun
    grid::grid.circle(
      x = grid::unit(7.0, "native"),
      y = grid::unit(3.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "#FF4500", col = NA, alpha = 0.9)
    )
    
  } else if (scene_name == "Nightfall") {
    sky_color <- "#284664"
    
    # Draw sky background
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(10, "native"),
      gp = grid::gpar(fill = sky_color, col = NA)
    )
    
    # Draw grass
    grid::grid.rect(
      x = grid::unit(5, "native"),
      y = grid::unit(1.5, "native"),
      width = grid::unit(10, "native"),
      height = grid::unit(3, "native"),
      gp = grid::gpar(fill = "#4CAF50", col = NA)
    )
    
    # Draw moon
    grid::grid.circle(
      x = grid::unit(2.5, "native"),
      y = grid::unit(6.0, "native"),
      r = grid::unit(0.5, "npc") * 20/100,
      gp = grid::gpar(fill = "#D3D3D3", col = NA, alpha = 0.7)
    )
    
  } else {
    stop("Invalid scene name. Please use one of the eight valid names.")
  }
  
  # Draw title label
  grid::grid.text(
    paste("Scene:", scene_name),
    x = grid::unit(0.02, "npc"),
    y = grid::unit(0.98, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "white")
  )
  
  grid::popViewport()
  
  return(invisible(NULL))
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

# draw house function
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
    # get specific value from the right row
    value <- df %>%
      filter(object == obj_name & timeOfDay == scene_name) %>%
      pull(!!sym(column_name))
    
    # Return values or error message if nothing found
    if (length(value) == 0) {
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
  
  return(function() {
    # Draw the background scene first
    draw_scene_by_mood(scene_name)
    
    # Draw road
    pushViewport(viewport(x = 0.5, y = 0.15, width = 1.2, height = 0.4)) 
    draw_road(road_color = road_color, scale = road_scale)
    popViewport()
    
    # Draw parking sign
    pushViewport(viewport(x = 0.17, y = 0.32, width = 0.22, height = 0.22))
    draw_parking_sign(primary_color = parking_color, scale = parking_scale * 2)
    popViewport()
    
    # Draw house
    pushViewport(viewport(x = 0.4, y = 0.34, width = 0.3, height = 0.36))
    draw_house(house_color = house_color, scale = house_scale)
    popViewport()
    
    # Draw office
    pushViewport(viewport(x = 0.7, y = 0.32, width = 0.3, height = 0.4))
    draw_office(office_color = office_color, scale = office_scale)
    popViewport()
    
    # Draw hotel
    pushViewport(viewport(x = 0.85, y = 0.33, width = 0.3, height = 0.4))
    draw_hotel(hotel_color = hotel_color, scale = hotel_scale)
    popViewport()
  })
}


# run shiny app function
run_crime_scene_app <- function(df_data) {
  
  # slider labels
  word_levels <- c(
    "Pre-Dawn" = 1, "Sunrise" = 2, "Morning" = 3, "Mid-Day" = 4, 
    "Afternoon" = 5, "Sunset" = 6, "Nightfall" = 7, "Midnight" = 8
  )
  word_names <- names(word_levels)
  num_levels <- length(word_levels)
  
  # make evenly spaced labels on the slider
  label_positions <- seq(0, 100, length.out = num_levels)
  
  # Define UI
  ui <- fluidPage(
    # title
    titlePanel("Crime in Austin Killer Plot"),
    
    # the top row is where the plot goes
    fluidRow(
      column(8, offset = 2,
             plotOutput("distPlot", height = "850px") 
      )
    ),
    
    # Bottom slider labels and setup
    fluidRow(
      column(6, offset = 3,
             sliderInput(
               inputId = "time_of_day", 
               label = "Select a time:",
               min = min(word_levels),
               max = max(word_levels),
               value = word_levels["Pre-Dawn"], 
               step = 1,
               ticks = FALSE, 
               width = "100%" 
             ),
             
             # Display the word labels
             tags$div(
               style = "width: 100%; position: relative; margin-top: -15px;",
               lapply(1:num_levels, function(i) {
                 tags$div(
                   style = paste0(
                     "position: absolute; ",
                     "left: ", label_positions[i], "%; ",
                     "transform: translateX(-50%); ",
                     "font-size: 10px; text-align: center; white-space: nowrap;"
                   ),
                   word_names[i]
                 )
               })
             )
      )
    )
  )
  
  # Server definitions
  server <- function(input, output, session) {
    
    # Uses slider to get the time
    selected_word_reactive <- reactive({
      index <- input$time_of_day
      names(word_levels)[word_levels == index]
    })
    
    # produce combined plot
    output$distPlot <- renderPlot({
      # get current time
      current_time <- selected_word_reactive()
      
      # Get the drawing function for the current time
      draw_function <- compose_scene_by_time(scene_name = current_time, df = df_data)
      draw_function()
      
    }, res = 96)
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
}

# Load data and run app
my_data <- read.csv("killerDF.csv")
run_crime_scene_app(df_data = my_data)
