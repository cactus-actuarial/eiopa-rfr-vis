# todo:
# indicate data points + geom_points()
# line type instead of colour

library("readxl")
library("ggplot2")
library("scales")
library("tidyr")

# functions
umround <- function(x, base){ 
    base * ceiling(x/base) 
} 

lmround <- function(x, base){ 
    base * floor(x/base) 
}

# load data
data_raw <- read_excel("data//EIOPA_RFR_20181231_Term_Structures.xlsx", sheet = "RFR_spot_no_VA", col_types = "numeric")
data_wide <- data_raw[-c(1:8),]
colnames(data_wide)[colnames(data_wide) == "Main menu"] <- "time"
data <- gather(data_wide, country, RFR_spot, colnames(data_wide)[2]:colnames(data_wide)[ncol(data_wide)])

# ui
ui <- fluidPage(
    titlePanel("EIOPA RFR spot no VA, 31.12.2018"),
    
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "countries", label = "Choose country:", choices = unique(data$country), 
                           selected = unique(data$country)[1], multiple = TRUE),
            hr(),
            strong("Graph settings:")
        ),
        
        mainPanel(
            plotOutput("plot", height = "800px")
        )
    )
)

# server
server <- function(input, output) {
    
    chosen_countries <- reactive({
        input$countries
    })
    
    output$plot <- renderPlot(
        
        ggplot(subset(data, country %in% chosen_countries()), aes(x = time, y = RFR_spot, colour = country)) +
            geom_line() + 
            scale_x_continuous(breaks = c(1, seq(from = 0, to = max(data$time), by = 10)), expand = c(0, 0)) +
            scale_y_continuous(breaks = seq(from = lmround(min(subset(data, country %in% chosen_countries(), select = "RFR_spot")), 0.0025), 
                                            to   = umround(max(subset(data, country %in% chosen_countries(), select = "RFR_spot")), 0.0025), 
                                            by = 0.0025), 
                               labels = scales::percent_format()) +
            geom_hline(yintercept = 0, colour = "darkgray") +
            theme_light()
    )
}

shinyApp(ui = ui, server = server)
