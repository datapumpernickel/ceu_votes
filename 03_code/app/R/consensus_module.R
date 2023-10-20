get_config_filter <- function(input, config_values){
  if(is.null(input)){
    return("")
  } else {
    return(input)
  }
}

custom_color_scale <- list(
  c("#004778", "#336b91"),  
  c("#CDAC62", "#e8d6b0"),   
  "#B0B0B0"  
)

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
                 unique(c("All", "Missing", sort(
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
      
      
      selected_policy_areas <- reactive({
        req(input$policy_area_type)
        selected_areas <- input$policy_area_type
        selected_areas
      })
      
      plot_data <- reactive({
        req(filtered_data())
        data <- filtered_data() %>%
          group_by(year, policy_area_label) %>%
          mutate(perc = count_voting_type/count_year_policy) %>%
          ungroup() %>%
          complete(year, policy_area_label, voting_type) %>%
          replace_na(list(perc = 0)) %>%
          ungroup() %>%
          filter(!is.na(voting_type)) %>%
          filter(policy_area_label %in% selected_policy_areas()) %>%
          mutate(text_hover = paste(glue('<b>{voting_type}:</b>'),
                                    glue('{round_half_up(perc*100, digits = 1)}%')))
        
        data$policy_area_label <- factor(data$policy_area_label, levels = selected_policy_areas())
        
        data
      })
      
      
      output$table <- renderDT(server=FALSE,
                                          {
                                            datatable(
                                              filtered_data(),
                                              style = "bootstrap5",
                                              colnames = datatable_colnames_consensus, 
                                              extensions = "Buttons",
                                              options = list(
                                                scrollX = TRUE,
                                                order = list(list(0, "desc")),
                                                columnDefs = list(
                                                  list(className = "dt-right", targets = c(5))  
                                                ),
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
        unique_policy_areas <- plot_data() %>%
          group_by(policy_area_label) %>%
          reframe(id = cur_group_id())
        
        plot_list <- unique_policy_areas %>%
          group_by(id) |>
          group_map(~ {
            current_id <- .y$id
            current_policy_area_label <- .x$policy_area_label
            current_color_scale <- custom_color_scale[[current_id]]
            plot_data() %>%
              filter(policy_area_label == current_policy_area_label) %>%
              plot_ly(x = ~year,
                      y = ~perc,
                      color = ~voting_type,
                      colors = current_color_scale,
                      type = "bar",
                      showlegend = T,
                      legendgroup = as.character(current_id),
                      hoverinfo = "text",  
                      text = ~text_hover,  
                      textposition = "none") %>% 
              layout(yaxis = list(title = 'Percentage', titlefont = list(size = 16), tickfont = list(size = 14), tickformat = ".1%", range = c(0,1.1)),
                     xaxis = list(tickangle = -45, tickfont = list(size = 14)), 
                     barmode = 'stack',
                     margin = list(t = 60),
                     font = list(family = "Source Sans Pro", size = 14),
                     modebar = list(orientation = "v", bgcolor = "rgba(255, 255, 255, 0.8)", activecolor = "#4C9C8B", buttons = list(list(name = "toImage", title = "Download plot as a png"))),
                     legend = list(font = list(family = "Source Sans Pro", size = 14), orientation = "h", xanchor = "center", x = 0.5, y = -0.15, traceorder = "normal", tracegroupgap = 10, bgcolor = "rgba(255, 255, 255, 0.8)"),
                     annotations = list(
                       x = 0.5,
                       y = 1.15,
                       xref = "paper",
                       yref = "paper",
                       text = current_policy_area_label,
                       showarrow = FALSE,
                       font = list(size = 16)
                     ),
                     separators = ".,") %>%
              plotly::config(displaylogo = FALSE,modeBarButtons = list(list("toImage")))
          })
        
        final_plot <- subplot(plot_list, nrows = 1, margin = 0.05, shareY = T)
        final_plot
      }) |> bindCache(plot_data())
      
      observeEvent(input$clear_button_policy, {
        updateSelectizeInput(session, "policy_area_type", selected = "All")
      })
      
    })}

