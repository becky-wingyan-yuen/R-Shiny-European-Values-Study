
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(haven)
library(dplyr)
library(ggfortify)
library(stringi)



EVS = read_sav("./EVS_shiny/data/EVS_data_cleaned.sav")


library(labelled)
country_list = val_labels(EVS$country)
null_country = c()

for( i in 1:length(country_list) ){
  if( dim(EVS[which(EVS$country==country_list[i]),])[1]==0 ){
    null_country = c(null_country,names(country_list)[i])
  }
}

if(is.null(null_country)==FALSE){
  country_list = country_list[-which(names(country_list)%in%null_country)]
}

intro_text <- "This is an interface for navigating the results of the European Values Study, 
                  we aim to investigate how Europeans think about family, work, religion, politics, 
                  and society. All data are downloaded from [European Values Study 2017: Integrated 
                  Dataset (EVS 2017)](https://search.gesis.org/research_data/ZA7500). You may choose 
                  the country, outcomues, and controls on the sidebars, the corresponding results will
                  be shown. By default it will show the overall analysis of all countries."


# Define the UI for the application
ui <- dashboardPage(
  
  dashboardHeader(title = "Analysis on European Values Study"),
  
  dashboardSidebar(
      selectInput(
        "country_chr",
        "Choose Country:",
        choices = c("Overall",country_list),
        selected = "Overall"
      ),
      selectInput(
        "outcome_chr",
        "Choose Outcome Variable:",
        choices = c("When a mother works for pay, do Europeans think the children suffer?", 
                    "When jobs are scarce, do Europeans think employers should give priority 
                    to local people over immigrants?"),
        selected = "When a mother works for pay, do Europeans think the children suffer?"
      ),
      checkboxGroupInput(
        "control_chr", "Choose Control Variable(s):",
        choices = c("age","sex","education"),
        selected = "age"
      ),
      numericInput(
        "agePoly",
        "Age Polynomial Index:",
        value = 1,
        min = 1,
        max = 5,
        step = 1
      ),
      menuItem("Overview", tabName = "intro"),
      menuItem("Exploration", tabName = "exploration",
               icon = icon("flask")),
      menuItem("Regression", tabName = "regression",
               icon = icon("book"))
  ),
  
  dashboardBody(
        tabItems(
          tabItem(tabName = "intro",
                  h3("Overview of the app"),
                  h5(intro_text)
          ),
          
          tabItem(tabName = "exploration",
                  h2("Graphical Description"),
                  fluidRow(
                    plotlyOutput("boxPlot_age"),
                    plotlyOutput("boxPlot_sex"),
                    plotlyOutput("boxPlot_education")
                  )
          ),
          
          tabItem(tabName = "regression",
                  h2("Regression Analysis"),
                  fluidRow(
                    plotOutput("scatterPlot"),
                    verbatimTextOutput("regSummary")
                  )
          )
        )
  )
  
)


# Define server logic
server <- function(input, output) {
  
  # Reactive expression to sample data based on user input
  
  sampledData <- reactive({
      req(input$country_chr)
      if( input$country_chr == "Overall" ){
        EVS
      }
      else
      {
        EVS[which(EVS$country == country_list[which(names(country_list) == input$question_chr)]),]
      }
  })
  
  
  #### Exploration ####
  
  output$boxPlot_age <- renderPlotly({
      req(input$outcome_chr)
      if( input$outcome_chr == "When a mother works for pay, do Europeans think the children suffer?" ){
        ggplot(sampledData(), aes(as.factor(v72), age)) + 
          geom_boxplot() + 
          labs(x = "When a mother works for pay, the children suffer", y = "Age (Years)") + 
          scale_x_discrete(labels = c("strongly agree (1)", "agree (2)", "disagree (3)", "strongly disagree (4)"))
      }
      else
      {
        ggplot(sampledData(), aes(as.factor(v80), age)) + 
          geom_boxplot() + 
          labs(x = "When jobs are scarce, give priority to local people over immigrants", 
               y = "Age (Years)") + 
          scale_x_discrete(labels = c("strongly agree (1)", "agree (2)", "neither agree nor disagree (3)", "disagree (4)", "strongly disagree (5)"))
      }
  })
  
  output$boxPlot_sex <- renderPlotly({
    req(input$outcome_chr)
    if( input$outcome_chr == "When a mother works for pay, do Europeans think the children suffer?" ){
      ggplot(sampledData(), aes(x = as.factor(sex), fill = as.factor(v72))) + 
        geom_bar(position = "dodge") +  
        labs(x = "Sex", y = "Count") + 
        scale_x_discrete(labels = c("Male", "Female"))  + 
        scale_fill_discrete(name = "Response",
                            labels = c("strongly agree", "agree", "disagree", "strongly disagree"))
    }
    else
    {
      ggplot(sampledData(), aes(x = as.factor(sex), fill = as.factor(v80))) + 
        geom_bar(position = "dodge") + 
        labs(x = "Sex", y = "Count") + 
        scale_x_discrete(labels = c("Male", "Female"))  + 
        scale_fill_discrete(name = "Response")
    }
  })
  
  output$boxPlot_education <- renderPlotly({
    req(input$outcome_chr)
    if( input$outcome_chr == "When a mother works for pay, do Europeans think the children suffer?" ){
      ggplot(sampledData(), aes(x = as.factor(education), fill = as.factor(v72))) + 
        geom_bar(position = "dodge")  + 
        labs(x = "Education (Levels)", y = "Count") + 
        scale_x_discrete(labels = c("Lower","Medium","Higher"))  + 
        scale_fill_discrete(name = "Response")
    }
    else
    {
      ggplot(sampledData(), aes(x = as.factor(education), fill = as.factor(v80))) + 
        geom_bar(position = "dodge")  + 
        labs(x = "Education (Levels)", y = "Count") + 
        scale_x_discrete(labels = c("Lower","Medium","Higher"))  + 
        scale_fill_discrete(name = "Response")
    }
  })
  
  
  #### Regression ####
  
  model <- reactive({
    
    if( input$agePoly > 1 ){
      if( input$outcome_chr == "When a mother works for pay, do Europeans think the children suffer?" ){
        lm(as.formula(paste("v72", "~", paste(input$control_chr, collapse = "+"), "+", stri_paste("age^", 2:input$agePoly, collapse = "+")
        )), data = sampledData())
      }else{
        lm(as.formula(paste("v80", "~", paste(input$control_chr, collapse = "+"), "+", stri_paste("age^", 2:input$agePoly, collapse = "+")
        )), data = sampledData())
      }
    } else {
      if( input$outcome_chr == "When a mother works for pay, do Europeans think the children suffer?" ){
        lm(as.formula(paste("v72", "~", paste(input$control_chr, collapse = "+")
        )), data = sampledData())
      }else{
        lm(as.formula(paste("v80", "~", paste(input$control_chr, collapse = "+")
        )), data = sampledData())
      }
    }
    
  })
  
  
  output$scatterPlot <- renderPlot({
    plot(fitted(model()), resid(model()), xlab = "Fitted Values", ylab = "Residuals")
  })
  
  output$regSummary <- renderPrint({
    summary(model())
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
