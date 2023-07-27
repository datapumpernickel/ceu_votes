
get_country_filter <- function(input, country_values){
  if(is.null(input)){
    return(country_values)
  } else {
    return(input)
  }
}

get_config_filter <- function(input, config_values){
  if(is.null(input)){
    return("")
  } else {
    return(input)
  }
}


minority_nav <- function(id, label = "vote_type") {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinybrowser::detect(),
      use_prompt(),
      column(
        4,
        add_prompt(
          sliderInput(
            ns("date_vote_type"),
            slider_label,
            min = lubridate::ymd("2010-01-01"),
            max = lubridate::ymd(Sys.Date()),
            value = c(lubridate::ymd("2010-01-01"),
                      lubridate::ymd(Sys.Date())),
            step = 365,
            timeFormat = "%Y"
          ),
          position = "top",
          message = slider_message
        )
      ),
      column(
        4,
        div(
          add_prompt(
            selectizeInput(
              ns("ceu_config_type"),
              selectize_policy_label,
              selected = "All",
              unique(c("All", sort(
                unique(df_minority$policy_area_label)
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
      column(
        4,
        div(
          add_prompt(
            selectizeInput(
              ns("country_type"),
              selectize_country_label,
              selected = NULL,
              unique(df_minority$country),
              multiple = TRUE
            ),
            position = "top",
            message = selectize_country_message
          )
        ),
        div(
          add_prompt(
            actionButton(ns("clear_button_country"), clear_button_country_label),
            position = "top",
            message = clear_button_country_message
          )
        )
      )
    ), 
    card(
      card_body(
        min_height = 200,
        plotlyOutput(ns("plot"))
      ),
      style = "margin-top: 20px;"
    ),
    card(
      card_header(card_header_plot),
      layout_sidebar(
        sidebar = sidebar(
          minority_table_docu
        ),
        dataTableOutput(ns("table")),
        fillable = FALSE
      ),
      style = "margin-top: 20px;")
  )
}

minority_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filtered_data <- reactive({
        req(input$date_vote_type, input$ceu_config_type) 
        result <- df_minority %>%
          filter(year %in% lubridate::year(lubridate::ymd(input$date_vote_type[1])):
                   lubridate::year(lubridate::ymd(input$date_vote_type)[2])) %>% 
          filter(country %in% get_country_filter(input$country_type, unique(df_minority$country)))%>% 
          filter(policy_area_label %in% get_config_filter(input$ceu_config_type, unique(df_minority$policy_area_label)))
        
        result
      })   %>%
        bindCache(input$date_vote_type,input$ceu_config_type,input$country_type)
      
      plot_data <- reactive({
        req(filtered_data()) 
        data <- filtered_data() %>%
          pivot_longer(
            c(infavor, abstained, against, 'not participating'),
            names_to = "voting_position",
            values_to = "amount"
          )  %>%
          group_by(country, policy_area_label) %>% 
          mutate(total_country_config = sum(amount)) %>% 
          group_by(country,country_full, policy_area_label, voting_position,total_country_config) %>% 
          summarise(n = sum(amount),.groups = "drop") %>% 
          mutate(perc = n/total_country_config) |> 
          ungroup() %>%
          filter(!voting_position == "infavor")%>%
          mutate(text_hover = paste(glue('<b>{country_full}, {voting_position}:</b>'),
                                    glue('{round_half_up(perc*100, digits = 2)}%')))
        data
      })%>%
        bindCache(input$date_vote_type,input$ceu_config_type,input$country_type)
      
      
      
      
      ## base data
      
      output$table <- renderDT(server=FALSE,
                               {
                                 extensions <- "Buttons"  # Default extension
                                 if (shinybrowser::get_width() <= 600) {
                                   extensions <- c(extensions, "Responsive")
                                 }
                                 datatable(
                                   filtered_data() |> select(-country) |> 
                                     relocate(country_full, .before = "policy_area_label"),
                                   style = "bootstrap5",
                                   colnames = datatable_colnames, 
                                   extensions = extensions,
                                   options = list(
                                     scrollX = TRUE,
                                     order = list(list(5, "desc")),
                                     dom = "Bfrtip",
                                     buttons = list(
                                       list(
                                         extend = "collection",
                                         text = datatable_download_text,
                                         buttons = list(
                                           list(extend = "excel", filename = datatable_filename_minority, exportOptions = list(modifiers = list(page = "all"))),
                                           list(extend = "csv", filename = datatable_filename_minority, exportOptions = list(modifiers = list(page = "all")))
                                         )
                                       )
                                     ),
                                     language = list(searchPlaceholder = datatable_search_placeholder)
                                   ),
                                   rownames = FALSE,
                                   class = "cell-border stripe hover",
                                   escape = FALSE
                                 )
                               }
      ) %>%
        bindCache(input$date_vote_type,input$ceu_config_type,input$country_type)
      
      
      output$plot <- renderPlotly({
        unique_policy_areas <- plot_data()  %>%
          group_by(policy_area_label) %>%
          reframe(id = cur_group_id())
        
        nrows <- ifelse(shinybrowser::get_width() <= 600 && nrow(unique_policy_areas)>1, 2, 1)  # 480px is a common breakpoint for mobile devices
        
        # Generate the plots for each policy_area_label and store them in a list
        plot_list <- unique_policy_areas %>%
          group_by(id) |>
          group_map(~ {
            current_id <- .y$id
            current_policy_area_label <- .x$policy_area_label
            plot_data() %>%
              filter(policy_area_label == current_policy_area_label)  |> 
              plot_ly(x = ~country,
                      y = ~perc,
                      color = ~voting_position,
                      colors = c("#CDAC62", "#B07A91","#B0B0B0"),
                      text = ~text_hover,
                      textposition = "none",
                      hoverinfo = 'text',
                      type = "bar",
                      showlegend = ifelse(current_id == 1, TRUE, FALSE)) %>%
              layout(yaxis = list(title = 'Percentage', titlefont = list(size = 14), tickfont = list(size = 12), tickformat = ".1%"),
                     xaxis = list(tickangle = -45, tickfont = list(size = 12), autorange = "reversed"),
                     barmode = 'stack',
                     margin = list(t = 60),
                     modebar = list(orientation = "v", bgcolor = "rgba(255, 255, 255, 0.8)", activecolor = "#4C9C8B", buttons = list(list(name = "toImage", title = "Download plot as a png"))),
                     legend = list(font = list(size = 14), orientation = "h", xanchor = "center", x = 0.5, y = -0.15, traceorder = "normal", tracegroupgap = 10, bgcolor = "rgba(255, 255, 255, 0.8)"),
                     annotations = list(
                       x = 0.5,
                       y = ifelse(nrows >1, 0.9,1.15),
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
        final_plot <- subplot(plot_list, nrows = nrows, margin = 0.05, shareY = T)
        final_plot
      }) |> bindCache(plot_data())
      
      observeEvent(input$clear_button_country, {
        updateSelectizeInput(session, "country_type", choices = unique(df_minority$country), selected = NULL, server = TRUE)
      })      
      
      observeEvent(input$clear_button_policy, {
        updateSelectizeInput(session, "ceu_config_type", choices = unique(c("All",sort(unique(df_minority$policy_area_label)))), selected = "All", server = TRUE)
      })
    }
  )
}