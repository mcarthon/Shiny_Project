# euro = Shiny_data %>%
#   filter(Continent.Name == 'Europe')
# 
# fluidPage(
#   theme = shinytheme('superhero'),
#   titlePanel('Mark Carthon - Starbucks Criterion'),
#   sidebarLayout(
#     sidebarPanel(
#       selectizeInput(inputId = 'country',label='Country Name',
#                      choices='All'),
#       # selectizeInput("dest", "Country Name",
#       #                choices = euro$Country.Name),
#       selectizeInput(inputId = 'var1'  ,label='Select an x variable',
#                      choices = colnames(Shiny_data)[-(1:2)]),
#       selectizeInput(inputId = 'var2'  ,label='Select a y variable',
#                      choices = colnames(Shiny_data)[-(1:2)])
#     ),
#     mainPanel(
#       img(src = 'ProfessionalPortait.jpg', height = '10%', width = '10%', align = 'right'),
#       tabsetPanel(
#         tabPanel("Plots",
#                  fluidRow(
#                    column(12, plotlyOutput("scatter"))
#                    # column(12, plotOutput("bar"))
#                  )),
#         tabPanel("Table", dataTableOutput('table'))
#       )
#     )
#   )
# )

dashboardPage(
  dashboardHeader(title='Starbucks Criterion'),
  dashboardSidebar(
    sidebarUserPanel('Mark Carthon',
                     image = 'ProfessionalPortait.jpg'),
    sidebarMenu(
      # menuItem('Welcome', tabName = 'welcome', icon = 'door-open'),
      menuItem('Data',tabName = 'data',icon = icon('database')),
      menuItem('Plot',tabName = 'plot',icon = icon('chart-area'))
    ),
     selectizeInput(inputId = 'country',label='Country Name',
                choices='All'),
#     selectizeInput("dest", "Country Name",
#               choices = euro$Country.Name),
      selectizeInput(inputId = 'var1'  ,label='Select an x variable',
                choices = colnames(Shiny_data)[-(1:2)]),
      selectizeInput(inputId = 'var2'  ,label='Select a y variable',
                choices = colnames(Shiny_data)[-(1:2)])
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'plot',
              fluidRow(
                column(12, plotlyOutput("scatter"))
              )
      ),
      # tabItem(tabName = 'welcome',
      #         'Then'
      #         ),
      tabItem(tabName = 'data',
              fluidRow(box(dataTableOutput('table'),width=12))
      )
    )
  )
)


