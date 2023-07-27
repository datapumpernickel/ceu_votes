#### REBECCA MAJEWSKI
#### 29.06.2023
#### CONSENSUS MODULE

  
get_config_filter <- function(input, config_values){
  if(is.null(input)){
    return("")
  } else {
    return(input)
  }
}

consensus_nav <- function(id, label = "consensus_type"){
  ns <- NS(id)
  tagList(fluidRow(
    shinybrowser::detect(),
    use_prompt(),
    column(
      4,
      add_prompt(
      sliderInput(
        ns("year_type"),
        slider_label,
        min = lubridate::ymd("2009-01-01"),
        max = lubridate::ymd(Sys.Date()),
        value = c(lubridate::ymd("2009-01-01"),
                  lubridate::ymd(Sys.Date())),
        step = 1,
        timeFormat = "%Y"
      ),
      position = "top",
      message = slider_message
      )
    ),
    column(4,
           div(
             add_prompt(
               selectizeInput(
                 ns("policy_area_type"),
                 "Policy Area",
                 selected = "All", 
                 unique(c("All", sort(
                   unique(df_consensus$policy_area_label)
                 ))),
                 multiple = TRUE,
                 options = list(maxItems = 2)
           ),
           position = "top",
           message = selectize_policy_message
           )
  ),
  div(
    add_prompt(
      actionButton(ns("clear_button_policy"), clear_button_policy_label),
      position = "top",
      message = clear_button_policy_message
    )
  )
  ),
  card(
    card_body(
      min_height = 200,
      plotlyOutput(ns("plot"))
    ),
    style = "margin-top: 20px;"
  )
  ,
  card(
    card_header(card_header_plot),
    layout_sidebar(
      sidebar = sidebar(
        consensus_table_docu
      ),
      dataTableOutput(ns("table")),
      fillable = FALSE 
    ),
    style = "margin-top: 20px;")
  )
  )
}
#####

consensus_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filtered_data <- reactive({
        req(input$year_type, input$policy_area_type) 
        result <- df_consensus %>%
          filter(year %in% lubridate::year(lubridate::ymd(input$year_type[1])):
                   lubridate::year(lubridate::ymd(input$year_type)[2])) %>%
          filter(policy_area_label %in% get_config_filter(input$policy_area_type, unique(df_consensus$policy_area_label)))
        result
      }) %>%
        bindCache(input$year_type, input$policy_area_type)
      
      
      plot_data <- reactive ({
        req(filtered_data())
        data <- filtered_data() %>%
          group_by(year, policy_area_label) %>%
          mutate(perc = count_voting_type/count_year_policy) %>%
          ungroup() %>%
          complete(year, policy_area_label) %>%
          replace_na(list(perc = 0)) %>%
          ungroup()
        
        data
      }) %>%
        bindCache(input$year_type, input$policy_area_type)
      
      
      output$table <- DT::renderDataTable(server=FALSE,
                                          {
                                            datatable(
                                              filtered_data(),
                                              style = "bootstrap5",
                                              colnames = datatable_colnames_consensus, 
                                              extensions = "Buttons",
                                              options = list(
                                                scrollX = TRUE,
                                                order = list(list(0, "desc")),
                                                columnDefs = list(list(className = 'dt-right', width = "20%", targets = "_all")),
                                                dom = "Bfrtip",
                                                buttons = list(
                                                  list(
                                                    extend = "collection",
                                                    text = datatable_download_text,
                                                    buttons = list(
                                                      list(extend = "excel", filename = datatable_filename_consensus, exportOptions = list(modifiers = list(page = "all"))),
                                                      list(extend = "csv", filename = datatable_filename_consensus, exportOptions = list(modifiers = list(page = "all")))
                                                    )
                                                  )
                                                ),
                                                language = list(searchPlaceholder = datatable_search_placeholder),
                                                lengthMenu = list(c(-1), c("All")) 
                                              ),
                                              rownames = FALSE,
                                              class = "cell-border stripe hover",
                                              escape = FALSE
                                            ) 
                                          }
      ) %>%
        bindCache(input$year_type, input$policy_area_type)
      
      output$plot <- renderPlotly({
        unique_policy_areas <- plot_data()  %>%
          group_by(policy_area_label) %>%
          reframe(id = cur_group_id())
        
      #  nrows <- ifelse(shinybrowser::get_width() <= 600 && nrow(unique_policy_areas)>1, 2, 1)  # 480px is a common breakpoint for mobile devices
        
        plot_list <- unique_policy_areas %>%
          group_by(id) |>
          group_map(~ {
            current_id <- .y$id
            current_policy_area_label <- .x$policy_area_label
            colors <- c("#004778", "#CDAC62")
            plot_data() %>%
              filter(policy_area_label == current_policy_area_label) %>%
              plot_ly(x = ~year,
                      y = ~perc,
                      color = ~policy_area_label,
                      colors = colors[current_id],
                      type = "bar",
                      showlegend = ifelse(current_id == 1, TRUE, FALSE),
                      text = ~paste0(policy_area_label, ", ", round(perc, 1), "%"),
                     hoverinfo = "text") %>%
              layout(yaxis = list(title = 'Percentage', titlefont = list(size = 14), tickfont = list(size = 12), tickformat = ".1%"),
                     xaxis = list(tickangle = -45, tickfont = list(size = 12)), 
                     barmode = 'stack',
                     margin = list(t = 60),
                     modebar = list(orientation = "v", bgcolor = "rgba(255, 255, 255, 0.8)", activecolor = "#4C9C8B", buttons = list(list(name = "toImage", title = "Download plot as a png"))),
                     legend = list(font = list(size = 14), orientation = "h", xanchor = "center", x = 0.5, y = -0.15, traceorder = "normal", tracegroupgap = 10, bgcolor = "rgba(255, 255, 255, 0.8)"),
                     annotations = list(
                       x = 0.5,
                       y = 1.15,
                       xref = "paper",
                       yref = "paper",
                       text = current_policy_area_label,
                       showarrow = FALSE,
                       font = list(size = 16)
                     ),
                     legend = list(font = list(size = 12)),
                     separators = ",.")%>%
              plotly::config(displaylogo = FALSE,modeBarButtons = list(list("toImage")))
          })
        
        # Combine the plots in the list into a single plot with subplots
        final_plot <- subplot(plot_list, nrows = 1, margin = 0.05, shareY = T)
        final_plot
      }) |> bindCache(plot_data())
      
      observeEvent(input$clear_button_policy, {
        updateSelectizeInput(session, "policy_area_type", selected = "All")
      })
      
    })}
