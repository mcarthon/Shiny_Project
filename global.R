library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
#install.packages('plotly')
library(plotly)
library(shinythemes)
library(readbitmap)
library(shinydashboard)

Shiny_data = read.csv('Shiny_data.csv', stringsAsFactors = F, row.names = NULL)
View(Shiny_data)

