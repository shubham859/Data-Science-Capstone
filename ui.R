#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#set working directory

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))

# Predict Next Word application UI defined 

# Navigation Bar - Title, Author, Date
shinyUI(navbarPage("Capstone: Course Final Project",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Shubham Patil </strong>"),
                            br(),
                            HTML("<em>Date: 05/12/2020</em>"),
                            br(),
                            #title for left side of page
                            titlePanel("User Interface"),
                            # Sidebar for user to enter part of a sentence 
                            sidebarLayout(
                              sidebarPanel(
                                helpText("This box is for the user to enter the words that will be used in the next word prediction."),
                                textInput("inputString", "Enter part of a sentence here",value = ""),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br()
                              ),
                              # Main panel to display the results of the word prediction
                              mainPanel(
                                h2("Next Word Prediction"),
                                verbatimTextOutput("prediction"),
                                strong("Here is what the user entered:"),
                                tags$style(type='text/css', '#text1 {background-color: rgba(150,200,255,0.50); color: black;}'),
                                textOutput('text1'),
                                br(),
                                br(),
                                br(),
                                br(),
                                helpText("Shiny is a product of R Studio")
                              )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                              br(),
                              br(),
                              includeMarkdown("about.md")
                            )
                   )
)
)