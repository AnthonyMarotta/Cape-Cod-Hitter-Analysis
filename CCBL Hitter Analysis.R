# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(ggrepel)
library(GeomMLBStadiums)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Apply a theme
  tags$head(
    tags$style(HTML("
      .centered {
        text-align: center;
      }
      .custom-panel {
        border-radius: 15px;
        border: 1px solid #ccc;
        padding: 15px;
        background-color: #f9f9f9;
        box-shadow: 2px 2px 12px rgba(0, 0, 0, 0.1);
      }
      .shiny-output-error-validation {
        color: red;
      }
      .scroll-table {
        height: 400px;  /* Adjust height as needed */
        overflow-y: scroll;
      }
    "))
  ),
  titlePanel("Cape Cod Baseball League Hitting Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      class = "custom-panel",
      style = "text-align: center;",  # Center align content in sidebar
      selectInput("team", "Select Team", choices = NULL),  # New input for team selection
      tags$br(),  # Add a line break for spacing
      selectInput("batter", "Select Batter", choices = NULL),
      tags$br(),  # Add a line break for spacing
      selectInput("pitchType", "Select Pitch Type", choices = NULL),  # Pitch type filter
      tags$br(),
      selectInput("pitcherThrows", "Pitcher's Throwing Hand", 
                  choices = c("All", "Left", "Right")),  # Add new input for Pitcher's Throwing Hand
      tags$br(),
      dateRangeInput("dateRange", "Select Date Range",
                     start = "2024-06-01", end = Sys.Date()),  # Date range input
      tags$br(),
      tags$img(src = "https://images.ctfassets.net/iiozhi00a8lc/4852OqYux6VPlGNVkd78dI/718866b7153eda424e697d94a022a381/565.svg", 
               height = 200, width = 200, style = "display: block; margin: 0 auto;")  # Center the image
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pitch Location Map",
                 plotOutput("pitch_location_map", height = "400px", width = "400px")),
        tabPanel("Pitch Heatmap",
                 div(
                   plotOutput("pitch_heatmap", height = "475px", width = "350px"),
                   class = "scroll-table")),
        tabPanel("Hit Chart",
                 plotOutput("hit_chart", height = "400px", width = "400px")),
        tabPanel("Game Statistics",
                 div(DT::dataTableOutput("game_table"), class = "scroll-table")),
        tabPanel("Season Statistics",
                 div(DT::dataTableOutput("statistics_table"), class = "scroll-table")),  # Add tab for calculated statistics
        tabPanel("Batter Hit Chart",
                 plotOutput("batter_hit_chart", height = "400px", width = "400px")),  # New tab for selected batter's hit chart
        tabPanel("Swing and Miss Chart",
                 plotOutput("strike_swinging_chart", height = "400px", width = "400px"))
      ),
      class = "custom-panel"  # Move class attribute here
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  setwd("~/Downloads/CCBLRCodes")
  # Load the CSV data directly
  data <- reactive({
    read.csv("SeasonCCBL.csv")
  })
  
  # Mapping of original team names to new names
  team_name_mapping <- c("BRE_WHI" = "Brewster Whitecaps", "BOU_BRA" = "Bourne Braves", "CHA_ANG" = "Chatham Anglers", "YAR_RED" = "Yarmouth-Dennis Red Sox", "FAL_COM" = "Falmouth Commodores", "WAR_GAT" = "Wareham Gateman", "HAR_MAR" = "Harwich Mariners", "HYA_HAR" = "Hyannis Harbor Hawks", "ORL_FIR" = "Orleans Firebirds", "COT_KET" = "Cotuit Kettleers")
  
  # Update team choices based on data
  observe({
    if (!is.null(data())) {
      updated_data <- data() %>%
        mutate(BatterTeam = ifelse(BatterTeam %in% names(team_name_mapping), team_name_mapping[BatterTeam], BatterTeam))
      
      updateSelectInput(session, "team", choices = unique(updated_data$BatterTeam))
    }
  })
  
  # Update batter and pitch type choices based on selected team
  observe({
    req(input$team)
    if (!is.null(data())) {
      filtered_data <- data() %>%
        mutate(BatterTeam = ifelse(BatterTeam %in% names(team_name_mapping), team_name_mapping[BatterTeam], BatterTeam)) %>%
        filter(BatterTeam == input$team & !is.na(AutoPitchType) & AutoPitchType != "")
      
      # Combine Four-Seam and Sinker data under Fastball
      filtered_data <- filtered_data %>%
        mutate(AutoPitchType = ifelse(AutoPitchType == "Sinker" | AutoPitchType == "Four-Seam", "Fastball", AutoPitchType))
      
      # Update batter choices
      updateSelectInput(session, "batter", choices = unique(filtered_data$Batter))
      
      # Update pitch type choices
      updateSelectInput(session, "pitchType", choices = c("All", unique(filtered_data$AutoPitchType)))
      
      # Assign filtered data to reactive value
      assign("filtered_data", filtered_data, envir = .GlobalEnv)
    }
  })
  
  # Filter the data based on selected batter, pitch type, pitcher's throwing hand, and date range
  dataFilter <- reactive({
    req(input$batter, input$pitchType, input$pitcherThrows, input$dateRange)
    filtered_data <- get("filtered_data", envir = .GlobalEnv)
    
    # Convert date column to Date type if necessary (replace 'DateColumn' with actual date column name)
    filtered_data <- filtered_data %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
    
    if (input$pitchType == "All") {
      if (input$pitcherThrows != "All") {
        filtered_data <- filtered_data %>%
          filter(Batter == input$batter & PitcherThrows == input$pitcherThrows &
                   Date >= input$dateRange[1] & Date <= input$dateRange[2])
      } else {
        filtered_data <- filtered_data %>%
          filter(Batter == input$batter & Date >= input$dateRange[1] & Date <= input$dateRange[2])
      }
    } else {
      if (input$pitcherThrows != "All") {
        filtered_data <- filtered_data %>%
          filter(Batter == input$batter & AutoPitchType == input$pitchType & 
                   PitcherThrows == input$pitcherThrows & Date >= input$dateRange[1] & Date <= input$dateRange[2])
      } else {
        filtered_data <- filtered_data %>%
          filter(Batter == input$batter & AutoPitchType == input$pitchType & 
                   Date >= input$dateRange[1] & Date <= input$dateRange[2])
      }
    }
    
    return(filtered_data)
  })
  
  
  # Filter data for the selected batter and create a hit chart
  output$batter_hit_chart <- renderPlot({
    req(input$batter)
    batter_data <- dataFilter() %>% 
      filter(Batter == input$batter & !(PlayResult %in% c("Out", "Undefined", "Sacrifice", "FieldersChoice", "StolenBase", "CaughtStealing", "Error")) &
               !is.na(ExitSpeed))  %>% # Filter out rows where ExitSpeed is NA
      # Calculate hc_x and hc_y
      mutate(hc_x = sin(Bearing * pi / 180) * Distance,
             hc_y = cos(Bearing * pi / 180) * Distance)
    # Ensure PlayResult is a factor with correct levels
    batter_data$PlayResult <- factor(batter_data$PlayResult, levels = c("Single", "Double", "Triple", "HomeRun"))
    
    plot17 <- ggplot(batter_data, aes(x = hc_x, y = hc_y)) + 
      geom_mlb_stadium(stadium_ids = 'dodgers',
                       stadium_transform_coords = TRUE, 
                       stadium_segments = 'all', 
                       linewidth = 0.5, 
                       color = 'black') + 
      theme_void() + 
      geom_point(aes(fill = PlayResult, color = PlayResult), 
                 shape = 21, 
                 colour = 'black', 
                 stroke = 0.5, 
                 size = 3,    # Adjust the size of points
                 alpha = 0.8) + 
      scale_fill_manual(values = c("Single" = "Blue",    # Red
                                   "Double" = "Red",   # Blue
                                   "Triple" = "Orange",
                                   "HomeRun" = "Green"),# Purple
                        breaks = c("Single", "Double", "Triple", "HomeRun"),
                        limits = c("Single", "Double", "Triple", "HomeRun")) +
      scale_color_manual(values = c("Single" = "black", "Double" = "black", "Triple" = "black", "HomeRun" = "black")) +  # Adjust point border colors
      theme(panel.background = element_rect(fill = 'white', colour = NA),  # Remove border by setting colour to NA
            plot.title = element_text(hjust = 0.5)) +
      coord_fixed() +
      labs(title = paste(input$batter, "Batted Ball Chart"),
           fill = 'Hit Type')
    
    # Add labels for ExitSpeed next to each point, rounded to one decimal and bold, with repulsion to prevent overlap
    plot17 <- plot17 +
      geom_text_repel(aes(label = paste(round(ExitSpeed, 1))), 
                      colour = "black", 
                      size = 3,   # Adjust the size of labels
                      box.padding = unit(0.25, "lines"),  # Padding around the label
                      point.padding = unit(1, "lines"),  # Minimum distance to points
                      segment.size = 0.2,  # Size of line segments
                      direction = "both",  # Direction of repulsion
                      force = 5,  # Strength of repulsion
                      fontface = "bold") +  # Make the text bold
      scale_color_manual(values = c("Single" = "black", "Double" = "black", "Triple" = "black", "HomeRun" = "black"))  # Adjust point border colors
    
    plot17
  })
  
  
  
  # Calculate statistics
  calculateStatistics <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    data_filtered <- data_filtered %>%
      mutate(
        InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
        Swing = PitchCall %in% c("FoulBallNotFieldable", "StrikeSwinging", "InPlay"),
        Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0)
      )
    
    statistics <- data_filtered %>%
      summarise(
        Pitches = n(),
        PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
        AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
        H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
        `1B` = sum(PlayResult == "Single"),
        `2B` = sum(PlayResult == "Double"),
        `3B` = sum(PlayResult == "Triple"),
        HR = sum(PlayResult == "HomeRun"),
        SO = sum(KorBB == "Strikeout"),
        BB = sum(KorBB == "Walk"),
        HBP = sum(PitchCall == "HitByPitch"),
        Whiffs = sum(PitchCall == "StrikeSwinging"),
        Swing = sum(Swing),
        GroundBall = sum(TaggedHitType == "GroundBall"),
        FlyBall = sum(TaggedHitType == "FlyBall"),
        LineDrive = sum(TaggedHitType == "LineDrive"),
        TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")),
        InStrikeZone = sum(InStrikeZone),
        Chase = sum(Chase),
        "Swing Percentage" = round(mean(Swing, na.rm = TRUE) / (Pitches), 1),
        "Whiff Percentage" = round(sum(Whiffs, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
        "Strikeout Percentage" = round(sum(SO, na.rm = TRUE) / sum(AB, na.rm = TRUE) * 100, 1),
        "Walk Percentage" = round(sum(BB, na.rm = TRUE) / sum(AB, na.rm = TRUE) * 100, 1),
        "GroundBall Percentage" = round(sum(GroundBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "FlyBall Percentage" = round(sum(FlyBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "LineDrive Percentage" = round(sum(LineDrive, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "Chase Percentage" = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE)*100, 1),
        AVG = round(sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
        OBP = round((sum(H, na.rm = TRUE) + sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE)) / sum(PA, na.rm = TRUE), 3),
        SLG = round((sum(`1B`, na.rm = TRUE) + 2*sum(`2B`, na.rm = TRUE) + 3*sum(`3B`, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE)) / sum(AB, na.rm = TRUE), 3),
        OPS = (OBP + SLG) 
      )
    
    return(statistics)
  })
  
  
  # Calculate game
  calculategame <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    # Define a mapping of BatterTeam to OpposingTeam
    opposing_team_mapping <- c("BRE_WHI" = "Brewster Whitecaps", "BOU_BRA" = "Bourne Braves", "CHA_ANG" = "Chatham Anglers", "YAR_RED" = "Yarmouth-Dennis Red Sox", "FAL_COM" = "Falmouth Commodores", "WAR_GAT" = "Wareham Gateman", "HAR_MAR" = "Harwich Mariners", "HYA_HAR" = "Hyannis Harbor Hawks", "ORL_FIR" = "Orleans Firebirds", "COT_KET" = "Cotuit Kettleers"
    )                          
    data_filtered <- data_filtered %>%
      mutate(
        InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
        Swing = PitchCall %in% c("FoulBallNotFieldable", "StrikeSwinging", "InPlay"),
        Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0),
        OpposingTeam = opposing_team_mapping[PitcherTeam]
      )
    
    statistics <- data_filtered %>%
      group_by(Date, OpposingTeam) %>%
      summarise(
        Pitches = n(),
        PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
        AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
        H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
        `1B` = sum(PlayResult == "Single"),
        `2B` = sum(PlayResult == "Double"),
        `3B` = sum(PlayResult == "Triple"),
        HR = sum(PlayResult == "HomeRun"),
        SO = sum(KorBB == "Strikeout"),
        BB = sum(KorBB == "Walk"),
        HBP = sum(PitchCall == "HitByPitch"),
        Whiffs = sum(PitchCall == "StrikeSwinging"),
        Swing = sum(Swing),
        GroundBall = sum(TaggedHitType == "GroundBall"),
        FlyBall = sum(TaggedHitType == "FlyBall"),
        LineDrive = sum(TaggedHitType == "LineDrive"),
        TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")),
        InStrikeZone = sum(InStrikeZone),
        Chase = sum(Chase),
        "Swing Percentage" = round(mean(Swing, na.rm = TRUE) / (Pitches), 1),
        "Whiff Percentage" = round(sum(Whiffs, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
        "Strikeout Percentage" = round(sum(SO, na.rm = TRUE) / sum(PA, na.rm = TRUE) * 100, 1),
        "Walk Percentage" = round(sum(BB, na.rm = TRUE) / sum(AB, na.rm = TRUE) * 100, 1),
        "GroundBall Percentage" = round(sum(GroundBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "FlyBall Percentage" = round(sum(FlyBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "LineDrive Percentage" = round(sum(LineDrive, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "Chase Percentage" = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE)*100, 1)
      )
    
    return(statistics)
  })
  # Render statistics table
  output$statistics_table <- DT::renderDataTable({
    DT::datatable(calculateStatistics(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Render game table
  output$game_table <- DT::renderDataTable({
    DT::datatable(calculategame(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Plot pitch location map
  output$pitch_location_map <- renderPlot({
    req(dataFilter())
    ggplot(dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoPitchType)) +
      geom_point(shape = 21, size = 3) +  # Adjust size as needed
      scale_fill_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
      labs(title = paste("Pitch Location Map for", input$pitcher),
           x = "Plate Location (Side)",
           y = "Plate Location (Height)",
           fill = "Pitch Type") +
      theme_minimal() +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4),
                fill = NA, color = "black", size = 1) +
      xlim(-1.8, 1.8) +
      ylim(1, 4)
  })
  
  # Pitch Heatmap chart
  output$pitch_heatmap <- renderPlot({
    req(dataFilter())
    ggplot(dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
      scale_fill_gradientn(colours = c("blue", "white", "red")) +
      annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4, fill = NA, color = "black", alpha = 0.1) +
      ylim(1, 4) + xlim(-1.8, 1.8) +
      theme_bw() + theme_classic() +
      xlab("Horizontal Pitch Location") +
      ylab("Vertical Pitch Location") +
      ggtitle("Pitch Location Heat Map", subtitle = "Pitcher's Perspective") +
      facet_wrap(~AutoPitchType, ncol = 2)
  }, height = function() {
    if (input$pitchType == "All") 650 else 475
  }, width = function() {
    if (input$pitchType == "All") 500 else 350
  })
  
  # Function to prepare data for Hit Chart
  hitChartData <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    # Filter out only relevant columns for Hit Chart and remove specific PlayResult values
    hit_data <- data_filtered %>%
      filter(!PlayResult %in% c("CaughtStealing", "FieldersChoice","Error", "StolenBase", "Sacrifice", "Undefined")) %>%
      select(PlateLocSide, PlateLocHeight, PlayResult, AutoPitchType)
    
    return(hit_data)
  })
  
  # Render Hit Chart
  output$hit_chart <- renderPlot({
    req(hitChartData())
    
    # Create a scatter plot for pitch locations with PlayResult and AutoPitchType
    ggplot(hitChartData(), aes(x = PlateLocSide, y = PlateLocHeight, color = PlayResult, shape = AutoPitchType)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "yellow", "black", "cyan", "magenta", "brown")) +
      labs(title = "Hit Chart",
           x = "Horizontal Pitch Location",
           y = "Vertical Pitch Location",
           color = "Play Result",
           shape = "Pitch Type") +
      theme_minimal() +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4),
                fill = NA, color = "black", size = 1) +
      xlim(-1.8, 1.8) +
      ylim(1, 4)
  })
  # Function to prepare data for StrikeSwinging Chart
  strikeSwingingChartData <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    # Filter data for StrikeSwinging in the KorBB column
    strike_swinging_data <- data_filtered %>%
      filter(PitchCall == "StrikeSwinging") %>%
      select(PlateLocSide, PlateLocHeight, PitchCall, AutoPitchType)
    
    return(strike_swinging_data)
  })
  
  # Render StrikeSwinging Chart
  output$strike_swinging_chart <- renderPlot({
    data <- strikeSwingingChartData()
    req(nrow(data) > 0)  # Ensure there's data to plot
    
    # Debugging: Print the first few rows of the data to the console
    print(head(data))
    
    # Create a scatter plot for pitch locations with KorBB and AutoPitchType
    ggplot(data, aes(x = PlateLocSide, y = PlateLocHeight, color = AutoPitchType, shape = PitchCall)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("Fastball" = "red", "Curveball" = "blue", "Slider" = "green", 
                                    "Changeup" = "purple", "Cutter" = "orange", "Splitter" = "Black")) +
      labs(title = "StrikeSwinging Chart",
           x = "Horizontal Pitch Location",
           y = "Vertical Pitch Location",
           color = "Pitch Type",
           shape = "Play Result") +
      theme_minimal() +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4),
                fill = NA, color = "black", size = 1) +
      xlim(-1.8, 1.8) +
      ylim(1, 4)
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

