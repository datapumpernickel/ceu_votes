

all_votes_nav <- function(id, label = "all_votes") {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        min_height = 200,
        plotlyOutput(ns("plot"))
      ),
      style = "margin-bottom: 20px;"
    ),
    card(
      card_header(card_header_plot),
      layout_sidebar(
        sidebar = sidebar(
          all_votes_table_docu
        ),
        dataTableOutput(ns("table")),
        fillable = FALSE
      ),
      style = "margin-top: 20px;"
    )
  )
}

all_votes_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ## base data

      output$table <- renderDT(server=FALSE,
                               {
                                 datatable(
                                   df_all_votes_table,
                                   style = "bootstrap5",
                                   extensions = c("Buttons","Responsive"),
                                   colnames = datatable_colnames_all, 
                                   options = list(
                                     scrollX = TRUE,
                                     order = list(list(0, "desc")),
                                     dom = "Bfrtip",
                                     buttons = list(
                                       list(
                                         extend = "collection",
                                         text = datatable_download_text,
                                         buttons = list(
                                           list(extend = "excel", filename = datatable_filename, exportOptions = list(modifiers = list(page = "all"))),
                                           list(extend = "csv", filename = datatable_filename, exportOptions = list(modifiers = list(page = "all")))
                                         )
                                       )
                                     ),
                                     language = list(searchPlaceholder = datatable_search_placeholder),
                                     initComplete = JS("function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#4C9C8B', 'color': '#fff'});",
                                                       "}")
                                   ),
                                   rownames = FALSE,
                                   class = "cell-border stripe hover",
                                   escape = FALSE
                                 )
                               }
      )

      output$plot <- renderPlotly({
        plot <-
          plot_ly(
            df_all_votes_plot,
            x = ~ year,
            y = ~ count_year,
            type = "bar",
            marker = list(color = "#4C9C8B")
          ) %>%
          layout(
            title = list(text = "Count by Year",
                         titlefont = list(size = 16)),
            xaxis = list(title = "Year", tickangle = -45),
            yaxis = list(title = "Count"),
            font = list(family = "Source Sans Pro", size = 14, color = "black"),
            margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
          ) %>%
          plotly::config(
            displayModeBar = F
          )
        
        plot
       
      }) 
    }
  )
}
