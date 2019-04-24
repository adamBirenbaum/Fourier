
library(shiny)
library(ggplot2)
library(gganimate)
library(shinydashboard)
library(latex2exp)

common_path <- "public/project/fourier/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_folder<<- paste0("/var/www/adambirenbaum.com/",common_path)
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_folder<<- "D:/abire/Documents/fourier/"
}else{
  path_to_folder <<- "~/fourier/"
  
}

successActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-success action-button btn-lg", label)
warningActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-warning action-button btn-lg", label)
infoActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-info action-button btn-lg", label)
dangerActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-danger action-button btn-lg", label)
primaryActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-primary action-button btn-lg", label)




dashboardPage(
  dashboardHeader(title = "Fourier"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fourier Series",
               menuSubItem("Square Wave", tabName = "square_fourier"),
               menuSubItem("Custom",tabName = "custom_fourier")
      ),
      menuItem("Animations",tabName = "animations")
      
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "square_fourier",
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Square Wave Parameters",solidHeader = T, status = "primary",width = NULL,
                    #sliderInput("square_amplitude", label = "Amplitude", min = 1, max = 10, value = 2),
                    sliderInput("square_terms",label = "Terms", min = 1, max = 100, value = 5),
                    primaryActionButton("square_enter",label = "Enter")
                    
                  )
                ),
                column(width = 8,
                       plotOutput("square_output")
                )
              )
      ),
      tabItem(tabName = "custom_fourier",
              
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Custom Function Paramaters",solidHeader = T, status = "primary",width = NULL,
                    textInput("custom_function",label = "Custom Function",value = "x^2"),
                    fluidRow(
                      column(width = 4,
                             textInput("custom_lb",label = "Lower Bound", value = 0)
                             ),
                      column(width = 4,
                             textInput("custom_up",label = 'Upper Bound', value = 5)
                             )
                    ),
                    sliderInput("custom_terms",label = "Terms", min = 1, max = 100, value = 5),
                    primaryActionButton("custom_enter",label = "Enter")
                  )
                ),
                column(width = 8,
                       plotOutput("custom_output")
                )
              )
              )
    )
  )
)

# 
# ui <- fluidPage(
#   fluidRow(
#     column(width = 3,
#            actionButton("enter","Enter")
#            )
#   ),
#   fluidRow(
#     column(width = 6,
#            imageOutput("anim1")
#            ),
#     column(width = 6,
#            imageOutput("anim2")
#            
#            )
#     
#   )
#   
#   
# )