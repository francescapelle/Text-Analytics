#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


shinyUI(
    dashboardPage(skin = "black",
        dashboardHeader(title = "Team 9 NLP"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Insights",
                         tabName = "dashboard",
                         icon = icon("lightbulb")),
                menuItem("Question",
                         tabName = "Question",
                         icon = icon("line-chart")),
                menuItem("Word Cloud",
                         tabName = "wordcloud",
                         icon = icon("cloud")),
                menuItem("N-gram",
                         tabName = "N-gram",
                         icon = icon("calendar")),
                menuItem("Model",
                         tabName = "model",
                         icon = icon("rocket")),
                menuItem(imageOutput("image_2", "auto")))),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "dashboard",
                    fluidRow(
                        infoBox("","Mission: bring your favorite meals in the comfort of your own home", icon = icon("apple-alt"), width = 12),
                        infoBox("","Users customize their meals", icon = icon("bone"), width = 12),
                        infoBox("","We provide all the necessary ingredients plus disposable cooking tools", icon = icon("beer"), width = 12),
                        box(imageOutput("image_1"), width = 4, height = 550),
                        #box(imageOutput("image_2"))
                    )),
                tabItem(tabName = "Question",
                        fluidRow(
                            box(title = "Controls", background = "navy",
                                selectInput(inputId = "question", label = "Questions", choices = q_words_comb$question),
                                sliderInput("slider", "Number of words:", 1, 15, 10),
                                width = 3, height = 300),
                            box(title = "Question is :",textOutput("question_Exp"), background = "navy",width = 3, height = 300),
                            box(title = "DTM :", textOutput("DTM"), background = "blue", width = 3, height = 300),
                            box(title = "Sum of sentiment value :", textOutput("sentiment"), background = "light-blue", width = 3, height = 300),
                            box(title = "Sentiment words", background = "navy", solidHeader = TRUE, plotOutput("np_plot"), width = 6),
                            box(title = "TF-IDF", background = "navy", solidHeader = TRUE, plotOutput("tf_plot"), width = 6),
                            box(plotOutput("s_plot"), background = "aqua", width = 12)
                            )),
                tabItem(tabName = "wordcloud",
                        fluidRow(
                            box(title = "Controls", background = "navy",
                                selectInput(inputId = "question_3", label = "Questions", choices = q_words_comb$question),
                                textOutput("question_Exp_3"),
                                sliderInput("min_freq", label = "Min frequency", 1, 10, 1),
                                sliderInput("max_words", label = "Max words", 1, 100, 40),
                                sliderInput("slider_2", label = "number of words", 1, 15, 10),
                                width = 3, height = 500),
                            box(plotOutput("wordcloud"), background = "light-blue", width = 9, height = 420),
                            box(plotOutput("frequency"), background = "light-blue", width = 12)
                        )),
                tabItem(tabName = "N-gram",
                        fluidRow(
                            box(title = "Controls", background = "navy",
                                selectInput(inputId = "question_2", label = "Questions", choices = q_words_comb$question),
                                textOutput("question_Exp_2"),
                                sliderInput(inputId = "n-gram", label = "n of N-gram", 2,4,2),
                                width = 3, height = 300),
                            box(title = "N-grams", background = "light-blue", solidHeader = TRUE, plotOutput("n_plot"), width = 12, height = 500)
                        )),
                tabItem(tabName = "model",
                        fluidRow(
                            tabBox(title = "Supervisor model",
                                   tabPanel(title = "NB_classifier", solidHeader = TRUE, tableOutput("nb")),
                                   tabPanel(title = "Predict", solidHeader = TRUE, tableOutput("predict"))
                        )))
                )
            )
        
))

