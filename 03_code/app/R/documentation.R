## konventionen für groß- und kleinschreibung, ggf. andere, z.b. britisch/amerikanisch


consensus_table_docu <- list(
  tags$p(
    tags$b("Consensus"),br(),
    "No participating member states opposed the adoption"
  ),
  tags$p(
    tags$b("Policy Areas"),br(),
    "Policy Areas are constructed and defined as per EuroVoc's guidelines and serve as the primary identification framework for analysis, superseding council formations."
  ),
  tags$p("Additional information can be found under the Documentation section.")
)

## minority page text
minority_table_docu <- list(
  tags$p(
   "Minority Voting refers to a scenario where a measure or policy proposal is adopted despite opposition from one or more member states."
  ),
  tags$p(
    tags$b("Policy Areas"),br(),
    "Policy Areas are constructed and defined as per EuroVoc's guidelines and serve as the primary identification framework for analysis, superseding council configurations."
  ),
  tags$p("Additional information can be found under the Documentation section.")
)

slider_label <- "Years: "
slider_message <- "Filter the date range for calculations"

# Selectize Input - Policy Area
selectize_policy_label <- "Policy Area"
selectize_policy_message <- "Select up to two policy areas, as defined by the Council of the EU"
clear_button_policy_label <- "Revert to All"
clear_button_policy_message <- "Use to revert back to the default view"

# Selectize Input - Countries
selectize_country_label <- "Countries"
selectize_country_message <- "Filter shown countries (in EU abbreviations)"
clear_button_country_label <- "Clear country selection"
clear_button_country_message <- "Use to revert back to the default view, showing all countries"

# Card Headers
card_header_plot <- "Corresponding data for the plot"

# DataTable
datatable_colnames <- c("Country", "Policy Area", "In Favour", "Abstained", "Against", "Not Participating")
datatable_colnames_consensus <- c("Year", "Policy Area", "Total Number of Votes per Year and Policy Area", "Voting Type", "Number of Decisions with Voting Type", "Percentage of Decisions with Voting Type")
datatable_colnames_all <- c("Voting Date","Policy Area", "Council Configuration", "Voting Rule", "In Favour", "Against", "Abstained", "Not Participating","Interinstitutional Code", "Voting Procedure ID","Document(s)")
datatable_search_placeholder <- "Search..."
datatable_download_text <- "Download"
datatable_filename <- "ceu_votes_all"
datatable_filename_minority <- "ceu_votes_minority"
datatable_filename_consensus <- "ceu_votes_consensus"





all_votes_table_docu <- list(
  # tags$p(
  #   tags$b("Year"),
  #   "This is the year used for the aggregation of votes."
  # ),
  # tags$p(tags$b("Country"),
  #        "The ISO2C code of the respective country used for aggregating vote results."),  
  # tags$p(tags$b("tbc"),
  #        "Needs to be extended."),
  tags$p(tags$b("Policy Areas"),br(),
         "Policy Areas are constructed and defined as per EuroVoc's guidelines and serve as the primary identification framework for analysis, superseding council formations.")
  ,
  tags$p("Additional information can be found under the Documentation section.")
)


doku_nav <- function(id, label = "doku") {
  ns <- NS(id)
  tagList(fluidPage(
                    fluidRow(
                      column(9,
                             card(
                               includeHTML(rmarkdown::render("www/documentation/documentation.Rmd",params = list(nrows=nrow(df_all_votes_table)))),
                               style = "margin-top: 20px;"
                             )),
                      column(
                        3,
                        card(
                          card_header("Contact:"),
                          card_body(
                            tags$li(
                              tags$a("Nicolai von Ondarza", href = "https://www.swp-berlin.org/wissenschaftler-in/nicolai-von-ondarza"),
                              ", Head of the Research Division EU/Europe, German Institute for International and Security Affairs (SWP)."
                            ),
                            tags$li(
                              tags$a("Paul Bochtler", href = "https://www.swp-berlin.org/wissenschaftler-in/paul-bochtler"),
                              ", Data Analysis Information Research Specialist and International Economics, German Institute for International and Security Affairs (SWP)."
                            )
                          ),
                          style = "margin-top: 20px;"
                        ),
                        div(  
                          class = "sticky-top",  
                          style = "top: 20px;", 
                          card(
                            includeMarkdown(
                              "www/documentation/toc.md"
                            ),
                            style = "margin-top: 20px;",
                          )
                        )
                      )
                    )))
}

