# Interactive elements: 
# - slider with animation for years
# - check boxes (multiple choices possible) for color, representing fishery type
# - drop down menu for size (options to choose: MSY, catch, surplus production)
# - check boxes for stock ID to include (default is all)
# - hover text for each point with stock name


require(shiny)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(dplyr)

data <- read.csv("~/Desktop/Raw Data/Hilborn Lab/RAM Files (v4.44)/dynamic stock status/SummaryData.csv", header=T)
data$Region <- as.character(data$Region)
data1 <- data[which(data$Year >= 1950),] 
split <- split(data1, data1$Region)
test <- split[[19]]
colnames(test) = c('ID','Year','BoverBMSY','UoverUMSY','Catch',
                   'Surprod','Biomass','u','BMSY','UMSY','MSY',
                   'stockid','Fisherytype','region') 
for (j in 1:(length(test$ID))){
  test$BoverBMSY[j] = min(test$BoverBMSY[j],3)
  test$UoverUMSY[j] = min(test$UoverUMSY[j],3)
}

test <- test[!is.na(test$BoverBMSY) & 
               !is.na(test$UoverUMSY), ]

test$Fisherytype <- factor(test$Fisherytype, 
                           levels = as.character(unique(test$Fisherytype)))
names <- names(split)

# For custom colors for fishery type in ggplot 
# (keeps colors and legend consistent throughout animation)
named_FT_Colors <- c("Gadids" = "yellowgreen", "Flatfish" = "palegreen", "Rockfish" = "tomato", 
                     "Forage Fish" = "darkorange", "Tuna and Marlin" = "violet", 
                     "Sharks Rays and Skates" = "mediumpurple", "Other Marine" = "slategray1", 
                     "Pacific Salmon" = "firebrick3", "Invertebrate" = "gold")
named_custom_FT_Colors <- named_FT_Colors[names(named_FT_Colors) %in% levels(test$Fisherytype)]

# dataframe with options for point size scale with different inputs (Catch or Surprod)
size_scale <- data.frame(matrix(NA, nrow = 3, ncol = 2))
dimnames(size_scale) <- list(c("MSY", "Catch", "Productivity"), c("min", "max"))
size_scale[1, ] <- c(min(test$MSY, na.rm = T), max(test$MSY, na.rm = T))
size_scale[2, ] <- c(min(test$Catch, na.rm = T), max(test$Catch, na.rm = T))
size_scale[3, ] <- c(min(test$Surprod, na.rm = T), max(test$Surprod, na.rm = T))
size_scale <- round(size_scale, digits = -2)



require(shiny)

ui <- fluidPage(
  pageWithSidebar(
    titlePanel("Test (US West Coast)"), 
    mainPanel(
      plotOutput("plot", height = "600px")
    ),
    sidebarPanel(
      sliderInput("Year", "Year", 
                  min = min(test$Year), 
                  max = max(test$Year), 
                  value = min(test$Year),
                  step = 1,
                  sep = "",
                  animate = animationOptions(interval = 500)), 
      selectInput("size", "Choose variable for point size:",
                  c("MSY" = "MSY",
                    "Catch" = "Catch",
                    "Productivity" = "Surprod")),
      selectInput("stock", "Select stock to display:",
                  c("All",
                    as.character(unique(test$ID)))),
      checkboxGroupInput("fishery_type", "Fishery Type:",
                  c(as.character(unique(test$Fisherytype))),
                  selected = c(as.character(unique(test$Fisherytype))))
    )
  )
)

require(shiny)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

server <- function(input, output) {
  myYear <- reactive({
    input$Year
  })
  myFisherytype <- reactive({
    input$fishery_type
  })
  mySize <- reactive({
    input$size
  })
  myStock <- reactive({
    input$stock
  })
  
  min_size <- reactive({
    if (input$size == "MSY") {
      size_scale[1, 1]
    } else if (input$size == "Catch") {
      size_scale[2, 1]
    } else {
      size_scale[3, 1]
    }
  })
  max_size <- reactive({
    if (input$size == "MSY") {
      size_scale[1, 2]
    } else if (input$size == "Catch") {
      size_scale[2, 2] 
    }  else {
      size_scale[3, 2]
    }
  })
  size_legend_title <- reactive({
    if (input$size == "MSY") {
      rownames(size_scale)[1]
    } else if (input$size == "Catch") {
      rownames(size_scale)[2] 
    }  else {
      rownames(size_scale)[3]
    }
  })
  
  myData <- reactive({
    if (input$stock == "All") {
      data <- filter(test, 
                     Year > myYear() - 1,
                     Year < myYear() + 1,
                     Fisherytype %in% myFisherytype()
        )
      } else {
        data <- filter(test, 
                       Year > myYear() - 1,
                       Year < myYear() + 1,
                       Fisherytype %in% myFisherytype(),
                       ID %in% myStock())
      }
    data <- as.data.frame(data)
    data
  })
  
  output$plot <- renderPlot({
    ggplot(myData(), aes(x = BoverBMSY,
                         y = UoverUMSY,
                         color = Fisherytype)) +
      geom_point(aes(size = !!as.symbol(mySize())),
                 alpha = 0.7) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 1) +
      scale_size_continuous(name = size_legend_title(),
                            limits = c(min_size(), max_size()),
                            labels = scales::comma) +
      ylim(0, 3) +
      xlim(0, 3) +
      scale_color_manual(name = "Fishery Type", 
                         values = named_custom_FT_Colors,
                         drop = F) +
      theme_light() +
      labs(x = "B/BMSY", y = "U/UMSY") +
      theme(legend.position = "bottom", legend.box = "vertical")
  })
  
  
}

shinyApp(ui = ui, server = server)
