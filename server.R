
function(input,output,session) {
  
  starbucks = reactive({
    if (input$country == 'All'){
      df = Shiny_data
    }else{
      df = Shiny_data %>% 
        filter(Country.Name == input$country)
    }
    df
  } )
  
  
  # observe({
  #   country = starbucks()
  # updateSelectizeInput(session,'dest',
  #                   choices = cont$Country.Name)
  # })
  #print(session)
  
  output$scatter=renderPlotly({
    # if (input$Cont == 'All'){
    #   df = Shiny_data
    # }else{
    #   df = Shiny_data %>% 
    #     filter(Continent.Name == input$Cont)
    # }
    ggplotly(
    {starbucks() %>%
      ggplot(aes_string(x=input$var1, y = input$var2 , fill = 'red',
       text = 'Country.Name'))+geom_point(fill='lightblue')},
    tooltip='text'
    )
    })
  
  # output$bar=renderPlot(
  #   starbucks() %>%
  #     mutate(color = Country.Name == input$dest) %>%
  #     # gather(key='type',value='delay',arrival,departure) %>%
  #     
  #     ggplot(aes(x=Country.Name, y = Store.Count, fill =color))+
  #     geom_bar(stat = 'identity') + 
  #     coord_flip() + 
  #     theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
  # )
  
  output$table=renderDataTable(
    DT::datatable(Shiny_data)
  )
  
}
