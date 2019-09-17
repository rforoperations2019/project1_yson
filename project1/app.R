library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tools)
library(shinydashboard)
library(plotly)

# Read data
cope <- read.csv("visual.csv", head = T) 

# Selected columns of interest: six satisfaction + Demographic + light measures
cope <- na.omit(select(cope,c("Q1", "Q6","Q10", "Q16","Q18","Q28","Q22","Q23",
                              "LightMon.OFF","LightKey.OFF","LightSurf.OFF"))) 
names(cope) <- c("light_level_for_paper_work", "visual_privacy",
                 "light_level_for_computer_work", "access_to_a_seated_view",
                 "overall_lighting_quality","overall_environmental_satisfaction","age","gender","light_level_on_monitor",
                 "light_level_on_keyboard", "light_level_on_worksurface")

# Vector
for (i in 1:6){
    cope[,i] <- as.vector(cope[,i])
}

# Factor
for (i in 7:8){
    cope[,i] <- factor(cope[,i])
}

cope$gender <- ifelse(cope$gender == 1, "Female", "Male")

# Axis titles for plot
axisTitles.7sat <- c('1'='Very Dissatisfied','2'='Dissatisfied ',
                     '3'='Somewhat Dissatisfied', '4' = 'Neutral',
                     '5' = 'Somewhat Satisfied', '6'='Satisfied',
                     '7'='Very Satisfied')

# Application title 
header <- dashboardHeader(title = "Post Occupancy Evaluation",titleWidth = 280)

# Dashboard Sidebar 
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # Menu Items 
        menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
        menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
        
        # Inputs: select sample size for plot 
        sliderInput("n_samp", 
                    "Sample size:",
                    min = 50,
                    max = nrow(cope),
                    value = 100,
                    step = 1),
        
        # Inputs: select variable to plot
        selectInput("visualSat",
                    "Visual satisfaction response you are interested:",
                    choices = c("Satisfaction with light level for computer work" = "light_level_for_computer_work", 
                                "Satisfaction with seated view"= "access_to_a_seated_view",
                                "Satisfaction with visual privacy" = "visual_privacy",
                                "Satisfaction with light level for paper-based work" = "light_level_for_paper_work", 
                                "Satisfaction with overall lighting quality" = "overall_lighting_quality"), 
                    selected = "overall_lighting_quality"),
        
        # Inputs: select variable to plot
        selectInput("measure",
                    "Visual measure you are interested:",
                    choices = c("Light Level on Monitor" = "light_level_on_monitor","Light Level on Keyboard" =
                                    "light_level_on_keyboard","Light Level on Worksurface" = "light_level_on_worksurface"),
                    selected = "light_level_on_keyboard")   
    )
)

# Dashboard body 
body <- dashboardBody(tabItems(
    
    # Plot page 
    tabItem("plot",
            
            # Input and Value Boxes -
            fluidRow(
                valueBoxOutput("male"),
                valueBoxOutput("female"),
                infoBoxOutput("avgSatisfaction")
            ),
            
            # Plot 
            fluidRow(
                tabBox(title = "Plot",
                       width = 12,
                       tabPanel("Subjective Responses", plotlyOutput("plot_sat")),
                       tabPanel("Subjective Response vs. Measure", plotlyOutput("plot_measure")),
                       tabPanel("Subjective Response Distribution by Gender", plotlyOutput("plot_demo")))
            )
    ),
    
    # Data Table Page 
    tabItem("table",
            fluidPage(
                box(title = "Sampled data of the fields you are interested in", DT::dataTableOutput("table"), width = 12))
    )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function for plots and value boxes 
server <- function(input, output) {
    
    # Reactive n sampled dataframe
    cope_sample <- reactive({ 
        req(input$n_samp) # ensure availablity of value before proceeding
        sample_n(cope, input$n_samp)
    })    
    
    # A plot showing selected visual response vs. overall environmental satisfaction
    output$plot_sat <- renderPlotly({
        ggplot(data = cope_sample()) +
            geom_boxplot(aes_string(input$visualSat, "overall_environmental_satisfaction", group = input$visualSat)) +
            scale_x_discrete(paste("Satisfaction with", toTitleCase(str_replace_all(input$visualSat, "_", " "))),
                             labels= axisTitles.7sat, 
                             limit = c("1","2","3","4","5","6","7")) +
            scale_y_continuous("Overall Environmental Satisfaction",
                               breaks = c(1,2,3,4,5,6,7), label = axisTitles.7sat) +      
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Satisfaction Responses") + theme_classic()
    })
    
    # A plot showing selected visual response vs. selected light level (measure)
    output$plot_measure <- renderPlotly({
        ggplot(data = cope_sample()) +
            geom_boxplot(aes_string(input$visualSat, input$measure, group = input$visualSat)) +
            scale_x_discrete(paste("Satisfaction with", toTitleCase(str_replace_all(input$visualSat, "_", " "))),
                             labels= axisTitles.7sat, 
                             limit = c("1","2","3","4","5","6","7")) +
            scale_y_continuous(paste(toTitleCase(str_replace_all(input$measure, "_", " ")),"(lux)")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Satisfaction response vs. Measure") + theme_classic()
    })
    
    # A plot showing visual satisfaction distribution by gender
    output$plot_demo <- renderPlotly({
        ggplot(data = cope_sample()) +
            geom_density(aes_string(input$visualSat, fill = as.factor(cope_sample()$gender),alpha = 0.5)) +
            xlab(toTitleCase(str_replace_all(input$visualSat, "_", " "))) +
            theme(legend.position = 'none') +
            scale_fill_discrete(name = "Gender", breaks =c(1,2),labels =c("female","male"))+
            labs(title = "Visual satisfaction distribution by gender")
})  
    
    # Data table of sampled data
    output$table <- DT::renderDataTable({
        subset(cope_sample(), select = c(input$visualSat, input$measure))
    })
    
    # Male Count value box
    output$male <- renderValueBox({
        dat <- cope_sample()
        
        infoBox("Male Count: ", value = tapply(dat$gender,dat$gender,length)[[2]], color = "olive")
    })
    
    # Female Count value box
    output$female <- renderValueBox({
        dat <- cope_sample()
        
        infoBox("Female Count: ", value = tapply(dat$gender,dat$gender,length)[[1]], color = "yellow")
    })
    
    # Overall environmental satisfaction mean value box 
    output$avgSatisfaction <- renderValueBox({
        dat <- cope_sample()
        num <- round(mean(dat$overall_environmental_satisfaction, na.rm = T), 2)
        
        valueBox("Average overall environmental satisfaction (Range: 1-7, Neutral: 4)", value = num, color = "teal")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



