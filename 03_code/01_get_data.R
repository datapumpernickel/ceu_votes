#### PAUL BOCHTLER
#### 17.06.2022
#### ANALYSIS  OF Voting data EU

#### set environment####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../')
getwd()

## empty potential rests from other scripts
rm(list = ls())

## load packages and install missing packages
require(pacman)

## define libraries to be loaded
p_load(
  char = c(
    'tidyverse',
    'readxl',
    "rvest",
    "httr",
    "lubridate",
    "openxlsx",
    "janitor",
    "openxlsx",
    "extrafont",
    "lubridate",
    "SWPcdR",
    "tidytext",
    "countrycode"
  )
)


unique_voting_results_query <-
  "PREFIX acts: <http://data.consilium.europa.eu/id/acts/>
PREFIX tax: <http://data.consilium.europa.eu/id/taxonomy/>
PREFIX codi: <http://data.consilium.europa.eu/def/codi/>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX votpos: <http://data.consilium.europa.eu/id/taxonomy/votingposition>
SELECT distinct ?voteProc ?docTitle ?forInterInstitutionalCode ?policyArea ?policyAreaLabel ?councilConfigurationLabel ?voteDecision ?votingRule  ?decisionDate ?votingRuleLabel group_concat(distinct ?voteOn;separator='|') as ?documents  group_concat(distinct ?countryCodeInFavour;separator='|') as ?countryCodeInFavourGrouped  group_concat(distinct ?countryCodeAgainst;separator='|') as ?countryCodeAgainstGrouped  group_concat(distinct ?countryCodeAbstained;separator='|') as ?countryCodeAbstainedGrouped  group_concat(distinct ?countryCodeNotParticipating;separator='|') as ?countryCodeNotParticipatingGrouped
FROM <http://data.consilium.europa.eu/id/dataset/VotingResults>
FROM <http://data.consilium.europa.eu/id/dataset/PublicRegister>
WHERE {     ?voteProc a codi:VotingProcedure.
?voteProc codi:votingRule ?votingRule.
       ?votingRule skos:prefLabel ?votingRuleLabel.
       optional {
              ?voteProc codi:policyArea ?policyArea.
              ?policyArea skos:prefLabel ?policyAreaLabel
       }
optional {
              ?voteProc codi:forInterInstitutionalCode ?forInterInstitutionalCode
       }
       optional {
              ?meeting codi:appliesProcedure ?voteProc.
              optional { ?meeting codi:meetingsessionnumber ?meetingSessionNumber. }
              optional {
                     ?meeting codi:configuration ?meetingConfig.
                     ?meetingConfig a skos:Concept.
                     ?meetingConfig skos:prefLabel ?councilConfigurationLabel.
              }
       }
              optional {
              ?voteProc codi:voteOn ?voteOn.
              ?voteOn codi:expressed ?docExpression.
              ?docExpression dct:title ?docTitle.
              ?docExpression dct:language ?docLanguage.
              FILTER ( lang(?docTitle) = 'en' )

       }
?voteProc codi:hasVoteOutcome ?voteDecision.
?voteDecision dct:dateAccepted ?decisionDate.
?voteDecision codi:hasVotingPosition
?countryVote_uri .
optional {
?countryVote_uri codi:votingposition <http://data.consilium.europa.eu/id/taxonomy/votingposition/votedagainst>.
?countryVote_uri codi:country ?countryVoteAgainst_uri.
?countryVoteAgainst_uri skos:notation ?countryCodeAgainst.     }
optional {
?countryVote_uri codi:votingposition <http://data.consilium.europa.eu/id/taxonomy/votingposition/votedinfavour>.
?countryVote_uri codi:country ?countryVoteInFavour_uri.        ?countryVoteInFavour_uri skos:notation ?countryCodeInFavour.     }
optional {        ?countryVote_uri codi:votingposition <http://data.consilium.europa.eu/id/taxonomy/votingposition/abstained>.
?countryVote_uri codi:country ?countryAbstained_uri.        ?countryAbstained_uri skos:notation ?countryCodeAbstained.     }
optional {        ?countryVote_uri codi:votingposition <http://data.consilium.europa.eu/id/taxonomy/votingposition/notparticipating>.
?countryVote_uri codi:country ?countryNotParticipating_uri.        ?countryNotParticipating_uri skos:notation ?countryCodeNotParticipating.     }
}"



public_voting <-
  httr::POST(
    url = 'https://data.consilium.europa.eu/sparql',
    accept("text/csv"),
    body = list(query = unique_voting_results_query)
  ) %>%
  content(., "text") %>%
  read_csv(.) %>%
  group_by(voteProc) %>%
  mutate(duplicate_proc = ifelse(n() > 1, T, F)) %>%
  mutate(year = ymd(decisionDate) %>% year()) |> 
  filter(!votingRuleLabel == "unanimity")

vote_set <- public_voting %>%
  select(-documents, -docTitle) |>
  distinct(.keep_all = T) %>%
  ungroup() %>%
  mutate(year = format(decisionDate, "%Y"),
         month = format(decisionDate, "%Y-%m")) %>%
  mutate(amount_no_votes = ifelse(
    !is.na(countryCodeAgainstGrouped),
    ifelse(
      str_detect(countryCodeAgainstGrouped, "\\|"),
      str_count(countryCodeAgainstGrouped, "\\|") +
        1,
      1
    ),
    0
  )) |>
  clean_names() |>
  replace_na(list(policy_area_label = "Missing")) |>
  mutate(policy_area_label = ifelse(policy_area_label == "Justice and Home Affairs", "Justice and Home Affairs", str_to_title(policy_area_label)))

title_set <- public_voting %>%
  distinct(voteProc, documents, docTitle, decisionDate) %>%
  ungroup() %>%
  mutate(year = format(decisionDate, "%Y"),
         month = format(decisionDate, "%Y-%m")) %>%
  clean_names()  |>
  mutate(document_link = str_c("https://www.consilium.europa.eu/en/documents-publications/public-register/public-register-search/results/?ImmcIdentifier=",
                               str_remove_all(documents, 
                                              "http://data.consilium.europa.eu/id/document/"))) |> 
  mutate(link = paste0("<a href='", document_link, "' target='_blank'>", doc_title, "</a>")) |>
  filter(!is.na(documents)) |>
  transmute(
    vote_procedure_id = str_remove_all(
      vote_proc,
      "http://data.consilium.europa.eu/id/votingprocedure/"
    ),
    link,
    doc_title
  ) |>
  group_by(vote_procedure_id) |>
  summarise(document_links = str_c(link, collapse = ", "),
            documents = str_c(doc_title, collapse = ", "))

all_yearly <- vote_set %>%
  ungroup() %>%
  count(year) %>%
  rename(count_year = n)

write_rds(all_yearly,
          file = file.path(
            "03_code","app",
            "01_raw_data",
            str_c(Sys.Date(), "_all_votes_count_year.rds")
          ))

all_yearly_policy_area <- vote_set %>%
  ungroup() %>%
  count(year, policy_area_label) %>%
  rename(count_year_policy = n)

voting_patterns_config <- vote_set %>%
  select(
    year,
    vote_proc,
    policy_area_label,
    country_code_against_grouped,
    country_code_abstained_grouped,
    amount_no_votes
  ) %>%
  group_by(year, policy_area_label) %>%
  summarise(einstimmig = sum(is.na(country_code_against_grouped) & is.na(country_code_abstained_grouped)),
            einstimmig_mit_enthaltung = sum(is.na(country_code_against_grouped) & !is.na(country_code_abstained_grouped)))   %>%
  left_join(all_yearly_policy_area) %>%
  pivot_longer(cols = c("einstimmig","einstimmig_mit_enthaltung"),
               names_to = "voting_type",
               values_to = "count_voting_type") %>%
  ungroup() %>%
  mutate(
    voting_type = case_when(
      voting_type == "einstimmig" ~ "only yes",
      voting_type == "einstimmig_mit_enthaltung" ~ "yes, but also abstentions"
    )
  ) |>
  filter(!year == "2009")  %>%
  group_by(year, voting_type) %>%
  group_modify(~ .x %>%
                 adorn_totals("row", name = "All")) %>%
  group_by(year, policy_area_label) %>%
  mutate(perc = scales::percent(count_voting_type/count_year_policy, 0.1)) %>%
  ungroup()  %>%
  select(year, policy_area_label, count_year_policy, voting_type, count_voting_type, perc)


write_rds(voting_patterns_config,
          file = file.path(
            "03_code","app",
            "01_raw_data",
            str_c(Sys.Date(), "_consensus_votes.rds")
          ))




public_voting_long <- vote_set %>% ungroup() %>%
  pivot_longer(
    c(
      country_code_abstained_grouped,
      country_code_against_grouped,
      country_code_in_favour_grouped,
      country_code_not_participating_grouped
    ),
    names_to = "voting_position",
    values_to = "country"
  ) %>% mutate(country = strsplit(country, "\\|")) %>%
  unnest(country) %>%
  mutate(
    voting_position = case_when(
      voting_position == "country_code_in_favour_grouped" ~ "infavor",
      voting_position == "country_code_abstained_grouped" ~ "abstained",
      voting_position == "country_code_against_grouped" ~ "against",
      voting_position == "country_code_not_participating_grouped" ~ "not participating"
    )
  ) %>%
  filter(!is.na(country)) %>%
  ungroup()

amount_no_votes <- public_voting_long  %>%
  group_by(year, country, policy_area_label) %>%
  count(voting_position) %>%
  pivot_wider(names_from = voting_position, values_from = n) %>%
  replace_na(list(
    infavor = 0,
    abstained = 0,
    against = 0,
    'not participating' = 0
  )) %>%
  ungroup() %>%
  pivot_longer(
    c(infavor, abstained, against, 'not participating'),
    names_to = "voting_position",
    values_to = "amount"
  )

amount_no_votes_wide <- public_voting_long  %>%
  group_by(year, country, policy_area_label) %>%
  count(voting_position) %>%
  pivot_wider(names_from = voting_position, values_from = n) %>%
  replace_na(list(
    infavor = 0,
    abstained = 0,
    against = 0,
    'not participating' = 0
  )) %>%
  ungroup() %>%
  arrange(country, year) |>
  filter(!year == "2009")  |> 
  group_by(year, country) %>%
  group_modify(~ .x %>%
                 adorn_totals("row", name = "All")) |>
  ungroup()|>
  mutate(country_full = countrycode(country, origin = "eurostat", destination = "country.name"))


write_rds(amount_no_votes_wide, file = file.path(
              "03_code","app",
  "01_raw_data",
  str_c(Sys.Date(),
        "_minority_votes.rds")
))


### all voting procedures data
public_voting <-
  httr::POST(
    url = 'https://data.consilium.europa.eu/sparql',
    accept("text/csv"),
    body = list(query = unique_voting_results_query)
  ) %>%
  content(., "text") %>%
  read_csv(.) %>%
  group_by(voteProc) %>%
  mutate(duplicate_proc = ifelse(n() > 1, T, F)) %>%
  mutate(year = ymd(decisionDate) %>% year()) 

vote_set <- public_voting %>%
  select(-documents, -docTitle) |>
  distinct(.keep_all = T) %>%
  ungroup() %>%
  mutate(year = format(decisionDate, "%Y"),
         month = format(decisionDate, "%Y-%m")) %>%
  mutate(amount_no_votes = ifelse(
    !is.na(countryCodeAgainstGrouped),
    ifelse(
      str_detect(countryCodeAgainstGrouped, "\\|"),
      str_count(countryCodeAgainstGrouped, "\\|") +
        1,
      1
    ),
    0
  )) |>
  clean_names() |>
  replace_na(list(policy_area_label = "Missing")) 

full_data <- vote_set  |>
  transmute(
    interinstitutional_code = for_inter_institutional_code,
    policy_area = policy_area_label,
    vote_procedure_id = str_remove_all(
      vote_proc,
      "http://data.consilium.europa.eu/id/votingprocedure/"
    ),
    council_configuration = council_configuration_label,
    voting_rule = voting_rule_label,
    in_favor = country_code_in_favour_grouped,
    against = country_code_against_grouped,
    abstained = country_code_abstained_grouped,
    not_participating = country_code_not_participating_grouped,
    decision_date
  ) |>
  left_join(title_set) |> 
  mutate(in_favor = str_replace_all(in_favor, "\\|",", "),
         against = str_replace_all(against, "\\|",", "),
         abstained = str_replace_all(abstained, "\\|",", "),
         not_participating = str_replace_all(not_participating, "\\|",", "),
         )|> 
  filter(!decision_date < as.Date("2010-01-01"))%>%
  select(
    decision_date, 
    policy_area, 
    council_configuration,
    voting_rule,
    in_favor,
    against,
    abstained,
    not_participating,
    # document_links,
    interinstitutional_code,
    vote_procedure_id,
    documents
  )

write_rds(full_data, file = file.path(
              "03_code","app",
  "01_raw_data",
  str_c(Sys.Date(),
        "_full_dataset.rds")
))

