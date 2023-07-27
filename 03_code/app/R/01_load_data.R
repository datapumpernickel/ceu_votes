
date <- list.files("01_raw_data") |> 
  str_remove_all(string = _, pattern = "_.*") |> 
  sort(decreasing = T) |> 
  pluck(1)

df_all_votes_plot <-
  readr::read_rds(glue("01_raw_data/{date}_all_votes_count_year.rds")) |>
  filter(!year == "2009")

df_all_votes_table <- readr::read_rds(glue("01_raw_data/{date}_full_dataset.rds")) 


df_minority <-
  readr::read_rds(glue("01_raw_data/{date}_minority_votes.rds"))

df_consensus <-
  readr::read_rds(glue("01_raw_data/{date}_consensus_votes.rds")) 
