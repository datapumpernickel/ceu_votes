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
    "rvest",
    "httr"
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

website_query <-  "PREFIX acts: <http://data.consilium.europa.eu/id/acts/>  PREFIX tax: <http://data.consilium.europa.eu/id/taxonomy/>  PREFIX codi: <http://data.consilium.europa.eu/def/codi/>  PREFIX skos:<http://www.w3.org/2004/02/skos/core#>  PREFIX dct: <http://purl.org/dc/terms/>  PREFIX foaf: <http://xmlns.com/foaf/0.1/>  PREFIX owl: <http://www.w3.org/2002/07/owl#>  PREFIX votpos: <http://data.consilium.europa.eu/id/taxonomy/votingposition>  SELECT distinct ?voteProc ?votingRuleId ?policyAreaId ?voteOnDocumentNumber ?votingInstCode ?legisProcId ?decisionDate  group_concat(distinct ?councilActionId;separator='|') as ?councilActionIdGrouped ?meetingSessionNumber ?councilConfigurationId  ?docExpression ?docTitle ?registerPage ?actNumber ?actTypeId  FROM <http://data.consilium.europa.eu/id/dataset/VotingResults>  FROM <http://data.consilium.europa.eu/id/dataset/PublicRegister>  WHERE {    ?voteProc a codi:VotingProcedure.      ?voteProc codi:votingRule ?votingRule.      ?votingRule skos:notation ?votingRuleId    optional {      ?voteProc codi:policyArea ?policyArea.      ?policyArea skos:notation ?policyAreaId    }.    optional {       ?voteProc codi:voteOn ?voteOn.      FILTER (CONTAINS(STR(?voteOn), 'INIT')).      ?voteOn codi:document_number ?voteOnDocumentNumber.      optional { ?voteOn codi:act_number ?actNumber. }.      optional {        ?voteOn codi:actType ?actType.        ?actType skos:notation ?actTypeId.      }.      optional { ?voteOn foaf:page ?registerPage. }      ?voteOn codi:expressed ?docExpression.      ?docExpression dct:title ?docTitle.      ?docExpression dct:language ?docLanguage.      FILTER ( lang(?docTitle) = 'en' || lang(?docTitle) = 'en' )    }.    optional { ?voteProc codi:forInterInstitutionalCode ?votingInstCode }.    optional {      ?voteProc codi:legislativeProcedure ?legisProc.      ?legisProc skos:notation ?legisProcId.    }.    ?voteProc codi:hasVoteOutcome ?voteDecision.    ?voteDecision dct:dateAccepted ?decisionDate.    optional {      ?voteDecision codi:councilAction ?councilAction.      ?councilAction skos:notation ?councilActionId.    }.    optional {      ?meeting codi:appliesProcedure ?voteProc.      optional { ?meeting codi:meetingsessionnumber ?meetingSessionNumber. }      optional {        ?meeting codi:configuration ?meetingConfig.        ?meetingConfig a skos:Concept.        ?meetingConfig skos:notation ?councilConfigurationId.      }    }  } ORDER BY DESC(?decisionDate), ?votingInstCode"


our_data <-
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

web_data <- 
  httr::POST(
    url = 'https://data.consilium.europa.eu/sparql',
    accept("text/csv"),
    body = list(query = website_query)
  )%>%
  content(., "text") %>%
  read_csv(.) |> 
  filter(councilActionIdGrouped=="10")


not_in_our_data <- web_data |> 
  filter(!voteProc %in% our_data$voteProc)


in_our_data_but_should_not <- our_data |> 
  filter(voteProc %in% web_data$voteProc)


writexl::write_xlsx(list(not_in_our_data, in_our_data_but_should_not), "01_raw_data/difference_website_data.xlsx")

