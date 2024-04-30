##CSV description
#glm-interpolation-results-large.csv contains simulation results for GLM models 
        #with large effect size
#glm-interpolation-results-small.csv contains simulation results for GLM models 
        #with small effect size
#bws-interpolation-results-large.csv contains simulation results for BWS models
        #with large effect size
#bws-interpolation-results-small.csv contains simulation results for BWS models
        #with small effect size
#bkmr-interpolation-results-large.csv contains simulation results for BKMR models
        #with large effect size
#bkmr-interpolation-results-small.csv contains simulation results for BKMR models
        #with small effect size
#data/nhanes-data.csv contains NHANES data used to compute correlation matrix

##app description
#contains bws & glm & bkmr model simulation results
#allows user to select group of chemicals
#allows user to select effect
#allows user to select effect level
#displays true outcome model
#highlights cells with power > 0.8
#contains correlation matrix for NHANES variables UrinaryBisphenolA, 
        #UrinaryBenzophenone3, Methylparaben, Propylparaben, dichlorophenol25, 
        #dichlorophenol24, MBzP, MEP, MiBP


#libraries needed for app
library(shiny)
library(mpower)
library(readxl)
library(bws)
library(reshape2)
library(tidyverse)
library(DT)
library(dplyr)
ui <- fluidPage(
        #adds title
        titlePanel(h1("Power Analysis via Monte Carlo Simulation", align = "center")),
        #add sidebar 
        sidebarLayout(
                sidebarPanel(
                        #Input select (select glm or bws model to display)
                        selectInput(inputId = "sct", label = "Select Model", c("GLM","BWS","BKMR")),
                        #Input slider for sample size (goes in increments of 1)
                        sliderInput(inputId = "num", label = "Select a Sample Size", 
                                    value=20, min=20, max=2000, step = 1),
                        #sliderInput(inputId = "thres", label = "Select a 'Significance' Threshold",
                                    #value=0.05, min=0, max=1, step = 0.01),
                        selectInput(inputId = "group", label = "Select a Group of Chemicals",
                                    c("Consumer products chemicals","Organophosphate insecticides")),
                        selectInput(inputId = "effect", label = "Select an Effect",
                                    c("Linear, additive")),
                        selectInput(inputId = "effectsize", label = "Select an Effect Size", c("Small","Large")),
                ),
                #Output() function (creates datatable and plot as output)
                mainPanel(
                        fluidRow(
                                column(width = 6, dataTableOutput(outputId= "table"), textOutput(outputId = "description")),
                                withMathJax(),
                                column(width = 6,
                                       HTML("<b>Correlation Matrix of the Chemicals:</b>"),
                                       plotOutput(outputId = "plot"),
                                       HTML("<b>The True Outcome Model:</b>"),
                                       uiOutput(outputId = "formula"),
                                       HTML("<b>Description and Purpose of App:<b>")
                                )
                        )
                )
        )
)
#reads in csv for glm interpolation results for large effect size
models <- read.csv("glm-interpolation-results/glm-interpolation-results-large.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
glm_large <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(glm_large) <- c("Chemical","Sample Size","Power")
#read in nhanes1 csv (for correlation matrix)
nhanes1 <- read.csv("data/nhanes-data.csv")
#remove X column from nhanes1
nhanes2 <- nhanes1[,c(2:10)]

#reads in csv for glm interpolation results for small effect size
models <- read.csv("glm-interpolation-results/glm-interpolation-results-small.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
glm_small <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(glm_small) <- c("Chemical","Sample Size","Power")


#reads in csv for bws simulation results for large effect size
modelsa <- read.csv("bws-interpolation-results/bws-interpolation-results-large.csv") 
#turns results into dataframe
modelsb <- as.data.frame(modelsa)
#selects useful columns in dataframe (sample size and power)
bws_large <- modelsb[,2:3] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(bws_large) <- c("Sample Size", "Power")

#reads in csv for bws simulation results for small effect size
modelsa <- read.csv("bws-interpolation-results/bws-interpolation-results-small.csv") 
#turns results into dataframe
modelsb <- as.data.frame(modelsa)
#selects useful columns in dataframe (sample size and power)
bws_small <- modelsb[,2:3] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(bws_small) <- c("Sample Size", "Power")

#reads in csv for bkmr interpolation results for large effect size
models <- read.csv("bkmr-interpolation-results/bkmr-interpolation-results-large.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
bkmr_large <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(bkmr_large) <- c("Chemical","Sample Size","Power")

#reads in csv for bkmr interpolation results for small effect size
models <- read.csv("bkmr-interpolation-results/bkmr-interpolation-results-small.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
bkmr_small <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(bkmr_small) <- c("Chemical","Sample Size","Power")

#reads in csv for glm interpolation results for small effect size for OPD
models <- read.csv("opd-glm-interpolation-results/opd-glm-interpolation-results-small.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
opd_glm_small <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(opd_glm_small) <- c("Chemical","Sample Size","Power")
#put correct chemical names in
opd_glm_small$Chemical[opd_glm_small$Chemical == "URXOP1"] <- "Dimethylphosphate"
opd_glm_small$Chemical[opd_glm_small$Chemical == "URXOP2"] <- "Diethylphosphate"
opd_glm_small$Chemical[opd_glm_small$Chemical == "URXOP3"] <- "Dimethylthiophosphate"
opd_glm_small$Chemical[opd_glm_small$Chemical == "URXOP4"] <- "Diethylthiophosphate"
opd_glm_small$Chemical[opd_glm_small$Chemical == "URXOP5"] <- "Dimethyldithiophosphate"
opd_glm_small$Chemical[opd_glm_small$Chemical == "URXOP6"] <- "Diethyldithiophosphate"
#read in opd csv (for correlation matrix)
opd1 <- read.csv("data/opd.csv")
#remove X column from opd1
opd2 <- opd1[,c(2:7)]
#put in real chemical names
colnames(opd2) <- c("Dimethylphosphate","Diethylphosphate","Dimethylthiophosphate","Diethylthiophosphate","Dimethyldithiophosphate","Diethyldithiophosphate")

#reads in csv for glm interpolation results for large effect size for OPD
models <- read.csv("opd-glm-interpolation-results/opd-glm-interpolation-results-large.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
opd_glm_large <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(opd_glm_large) <- c("Chemical","Sample Size","Power")
#put correct chemical names in
opd_glm_large$Chemical[opd_glm_large$Chemical == "URXOP1"] <- "Dimethylphosphate"
opd_glm_large$Chemical[opd_glm_large$Chemical == "URXOP2"] <- "Diethylphosphate"
opd_glm_large$Chemical[opd_glm_large$Chemical == "URXOP3"] <- "Dimethylthiophosphate"
opd_glm_large$Chemical[opd_glm_large$Chemical == "URXOP4"] <- "Diethylthiophosphate"
opd_glm_large$Chemical[opd_glm_large$Chemical == "URXOP5"] <- "Dimethyldithiophosphate"
opd_glm_large$Chemical[opd_glm_large$Chemical == "URXOP6"] <- "Diethyldithiophosphate"


#reads in csv for bws interpolation results for small effect size for OPD
models <- read.csv("opd-bws-interpolation-results/opd-bws-interpolation-small.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
opd_bws_small <- models2[,2:3] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(opd_bws_small) <- c("Sample Size","Power")

#reads in csv for glm interpolation results for large effect size for OPD
models <- read.csv("opd-bws-interpolation-results/opd-bws-interpolation-large.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
opd_bws_large <- models2[,2:3] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(opd_bws_large) <- c("Sample Size","Power")

#reads in csv for bkmr interpolation results for small effect size for OPD
models <- read.csv("opd-bkmr-interpolation-results/opd-bkmr-interpolation-small.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
opd_bkmr_small <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(opd_bkmr_small) <- c("Chemical","Sample Size","Power")
#put correct chemical names in
opd_bkmr_small$Chemical[opd_bkmr_small$Chemical == "URXOP1"] <- "Dimethylphosphate"
opd_bkmr_small$Chemical[opd_bkmr_small$Chemical == "URXOP2"] <- "Diethylphosphate"
opd_bkmr_small$Chemical[opd_bkmr_small$Chemical == "URXOP3"] <- "Dimethylthiophosphate"
opd_bkmr_small$Chemical[opd_bkmr_small$Chemical == "URXOP4"] <- "Diethylthiophosphate"
opd_bkmr_small$Chemical[opd_bkmr_small$Chemical == "URXOP5"] <- "Dimethyldithiophosphate"
opd_bkmr_small$Chemical[opd_bkmr_small$Chemical == "URXOP6"] <- "Diethyldithiophosphate"


#reads in csv for bkmr interpolation results for large effect size for OPD
models <- read.csv("opd-bkmr-interpolation-results/opd-bkmr-interpolation-large.csv")
#turns results into dataframe
models2 <- as.data.frame(models)
#selects useful columns in dataframe (variable, sample size, power)
opd_bkmr_large <- models2[,2:4] %>%
        mutate_if(is.numeric, round, 2)
#renames columns in dataframe
colnames(opd_bkmr_large) <- c("Chemical","Sample Size","Power")
#put correct chemical names in
opd_bkmr_large$Chemical[opd_bkmr_large$Chemical == "URXOP1"] <- "Dimethylphosphate"
opd_bkmr_large$Chemical[opd_bkmr_large$Chemical == "URXOP2"] <- "Diethylphosphate"
opd_bkmr_large$Chemical[opd_bkmr_large$Chemical == "URXOP3"] <- "Dimethylthiophosphate"
opd_bkmr_large$Chemical[opd_bkmr_large$Chemical == "URXOP4"] <- "Diethylthiophosphate"
opd_bkmr_large$Chemical[opd_bkmr_large$Chemical == "URXOP5"] <- "Dimethyldithiophosphate"
opd_bkmr_large$Chemical[opd_bkmr_large$Chemical == "URXOP6"] <- "Diethyldithiophosphate"


server <- function(input, output){
        #displays results of interpolation on simulation results depending on 
        #model and effect level that user selects 
        output$table <- renderDataTable(if(input$sct == "GLM" && input$effectsize == "Large" && input$group == "Consumer products chemicals"){
                #create datatable for glm, highlight cell yellow if power is > 0.8
                datatable(glm_large[(glm_large[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))
                #create datatable for bws, highlight cell yellow if power is > 0.8
        } else if (input$sct == "GLM" && input$effectsize == "Small" && input$group == "Consumer products chemicals") {
                #create datatable for glm, highlight cell yellow if power is > 0.8
                datatable(glm_small[(glm_small[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))
        } else if (input$sct == "BWS" && input$effectsize == "Large" && input$group == "Consumer products chemicals") {
                datatable(bws_large[(bws_large[,"Sample Size"] == input$num),],
                        rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$sct == "BWS" && input$effectsize == "Small" && input$group == "Consumer products chemicals") {
                datatable(bws_small[(bws_small[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$sct == "BKMR" && input$effectsize == "Large" && input$num <= 600 && input$group == "Consumer products chemicals") {
                datatable(bkmr_large[(bkmr_large[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$sct == "BKMR" && input$effectsize == "Small" && input$num <= 1000 && input$group == "Consumer products chemicals") {
                datatable(bkmr_small[(bkmr_small[,"Sample Size"] == input$num),],
                        rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$group == "Consumer products chemicals") {
                print(dplyr::tibble("Issue" = "Cannot run BKMR simulations for sample sizes greater than 600 (large effect sizes) or 
                                    1000 (small effect sizes) due to computing power limitations."))}
        else if(input$sct == "GLM" && input$effectsize == "Large" && input$group == "Organophosphate insecticides"){
                #create datatable for glm, highlight cell yellow if power is > 0.8
                datatable(opd_glm_large[(opd_glm_large[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))
                #create datatable for bws, highlight cell yellow if power is > 0.8
        } else if (input$sct == "GLM" && input$effectsize == "Small" && input$group == "Organophosphate insecticides") {
                #create datatable for glm, highlight cell yellow if power is > 0.8
                datatable(opd_glm_small[(opd_glm_small[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))
        } else if (input$sct == "BWS" && input$effectsize == "Large" && input$group == "Organophosphate insecticides") {
                datatable(opd_bws_large[(opd_bws_large[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$sct == "BWS" && input$effectsize == "Small" && input$group == "Organophosphate insecticides") {
                datatable(opd_bws_small[(opd_bws_small[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$sct == "BKMR" && input$effectsize == "Large" && input$num <= 500 && input$group == "Organophosphate insecticides") {
                datatable(opd_bkmr_large[(opd_bkmr_large[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$sct == "BKMR" && input$effectsize == "Small" && input$num <= 500 && input$group == "Organophosphate insecticides") {
                datatable(opd_bkmr_small[(opd_bkmr_small[,"Sample Size"] == input$num),],
                          rownames = FALSE) %>% 
                        formatStyle('Power', backgroundColor = styleInterval(c(0.8,1.0), c("white","yellow","white")))}
        else if (input$group == "Organophosphate insecticides") {
                print(dplyr::tibble("Issue" = "Cannot run BKMR simulations for sample sizes greater than 500 due to computing power limitations."))});
        
        output$plot <- renderPlot(
                if(input$group == "Consumer products chemicals"){ #uses nhanes2 to plot correlation matrix
                        nhanes2 %>%
                                cor() %>%
                                reshape2::melt() %>%
                                ggplot(aes(!!sym("Var2"), !!sym("Var1"), fill = !!sym("value"))) + 
                                geom_tile() +
                                scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#d55E00",
                                                     limit = c(-1, 1), name = "Pearson\nCorrelation") + 
                                theme(plot.title = element_text(hjust = 0.5),
                                      axis.text.x = element_text(angle = 90), 
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), 
                                      panel.background = element_blank()) +
                                coord_fixed() + labs(x = "", y = "")
                } else { # use opd2 to plot correlation matrix
                        opd2 %>%
                                cor(use = "pairwise.complete.obs") %>%
                                reshape2::melt() %>%
                                ggplot(aes(!!sym("Var2"), !!sym("Var1"), fill = !!sym("value"))) + 
                                geom_tile() +
                                scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#d55E00",
                                                     limit = c(-1, 1), name = "Pearson\nCorrelation") + 
                                theme(plot.title = element_text(hjust = 0.5),
                                      axis.text.x = element_text(angle = 90), 
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), 
                                      panel.background = element_blank()) +
                                coord_fixed() + labs(x = "", y = "")});
        output$formula <- renderUI({
          text <- switch(input$group,
                        'Consumer products chemicals' = switch(input$effectsize,
                                           Small = "$$y = 0.16 x_{2,5DCP} + 0.12 x_{MEP} + \\epsilon$$ $$\\epsilon \\sim N(0, 1)$$",
                                           Large = "$$y = 0.32 x_{2,5DCP} + 0.24 x_{MEP} + \\epsilon$$ $$\\epsilon \\sim N(0, 1)$$"),
                         'Organophosphate insecticides' = switch(input$effectsize,
                                        Small = "$$y = -4.17 log_{10}(x_{DAP}) - 3.64 log_{10}(x_{DMP}) + \\epsilon$$ $$\\epsilon \\sim N(0, 1)$$\n 
                                        DAP = Diethylphosphate + Diethylthiophosphate\n
                                        + Diethyldithiophosphate + Dimethylphosphate\n
                                        + Dimethylthiophosphate + Dimethyldithiophosphate\n
                                        DMP = Dimethylphosphate + Dimethylthiophosphate\n
                                        + Dimethyldithiophosphate\n",
                                        Large = "$$y = -7.00 log_{10}(x_{DAP}) - 5.97 log_{10}(x_{DMP}) + \\epsilon$$ $$\\epsilon \\sim N(0, 1)$$\n
                                        DAP = Diethylphosphate + Diethylthiophosphate\n
                                        + Diethyldithiophosphate + Dimethylphosphate\n
                                        + Dimethylthiophosphate + Dimethyldithiophosphate\n
                                        DMP = Dimethylphosphate + Dimethylthiophosphate\n
                                        + Dimethyldithiophosphate\n"))
          withMathJax(text)
          });
}
#creates Shiny app
shinyApp(ui = ui, server = server)

