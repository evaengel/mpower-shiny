#I checked and this code produces correct results/dataframes
#library packages needed for app
library(shiny)
library(mpower)
library(readxl)
library(bws)
library(reshape2)
library(tidyverse)
library(DT)
ui <- fluidPage(
        #adds title
        titlePanel(h1("mpower Shiny App", align = "center")),
        #add sidebar 
        sidebarLayout(sidebarPanel(
                #Input select (select glm or bws model to display)
                selectInput(inputId = "sct", label = "Select Model", c("GLM","BWS")),
                #Input slider for sample size (goes in increments of 1)
                sliderInput(inputId = "num", label = "Select a Sample Size", value=20, min=20, max=2000, step = 1)
        ),
        #Output() function (creates datatable and plot as output)
        mainPanel(fluidRow(column(width = 6,dataTableOutput(outputId= "table")), column(width = 6,plotOutput(outputId = "plot"))))
        ))
#reads in csv for glm simulation results
models <- read.csv("glm-simulation-data.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
models3 <- models2[,2:4]
#renames columns in dataframe
colnames(models3) <- c("Chemical","Sample Size","Power")
#read in nhanes1 csv (for correlation matrix)
nhanes1 <- read.csv("nhanes-data.csv")
#remove X column from nhanes1
nhanes2 <- nhanes1[,c(2:10)]

#reads in csv for bws simulation results
modelsa <- read.csv("bws-simulation-data.csv")
#turns results into dataframe
modelsb <- as.data.frame(modelsa)
#selects useful columns in dataframe (sample size and power)
modelsc <- modelsb[,2:3]
#renames columns in dataframe
colnames(modelsc) <- c("Sample Size","Power")

server <- function(input, output){
        #if function uses models3 (glm simulation results) if glm is selected in selectInput
        #uses modelsc (bws simulation results) if bws is selected in selectInput
        output$table <- renderDataTable(if(input$sct == "GLM"){
                #create datatable for glm, highlight cell yellow if power is > 0.8
                datatable(models3[(models3[,"Sample Size"] == input$num),]) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))
                #create datatable for bws, highlight cell yellow if power is > 0.8
        }else{datatable(modelsc[(modelsc[,"Sample Size"] == input$num),]) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))});
        #uses nhanes2 to plot correlation matrix
        output$plot <- renderPlot(nhanes2 %>%
                                          cor(method = "spearman") %>%
                                          reshape2::melt() %>%
                                          ggplot(aes(!!sym("Var2"), !!sym("Var1"), fill = !!sym("value"))) + 
                                          geom_tile() +
                                          scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#d55E00",
                                                               limit = c(-1, 1), name = "Spearman\nCorrelation") + 
                                          theme(plot.title = element_text(hjust = 0.5),
                                                axis.text.x = element_text(angle = 90), 
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(), 
                                                panel.background = element_blank()) +
                                          coord_fixed() + labs(x = "", y = "", title = "Spearman Correlation Matrix of NHANES Data"))
        
}
#creates Shiny app
shinyApp(ui = ui, server = server)