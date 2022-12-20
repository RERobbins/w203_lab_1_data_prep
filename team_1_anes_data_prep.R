require(tidyverse)
require(haven)

# This script takes the ANES 2020 time series database in SPSS sav format
# and creates the following two files in its directory.
#
# team_1_anes_data.rds contains the source data frame for our study, note that
# this file only contains observations with respect to people who voted or for
# whom we infer voting intent and presume to be voters.
#
# team_1_anes_data_cleaning_statistics contains various statistics that are
# artifacts of the data cleaning process and which are used in the R markdown
# file as inputs to our project report.  These statistics are the various
# study.* variables below.

# read ANES master data set
anes_data <- read_sav("anes_timeseries_2020_spss_20220210.sav") 

# create relevant test statistics for R markdown report
# descriptions of the several variables referenced appear in the comments below

study.count <- nrow(anes_data)
study.party_identity_table <- table(anes_data %>% select(V201231x))
study.voting_table <- table(anes_data %>% select(V202109x))
study.non_vote_reason_table <- table(anes_data %>% select(V202123))

study.non_vote_inferred_failed_registration_count <- anes_data %>% 
  filter (V202123 == 5 & V201019 == 1) %>%
  nrow()

# we use a subset of the ANES master data set
# this sequence creates the variables we use in our analysis and retains
# the source data without modification, select additional potentially
# relevant columns from the ANES master data set are included for
# reference purposes in the event they prove to be useful

anes_data<- anes_data %>%
  select (V200001,                 # Pre: Case ID 
          V201018,                 # Pre: Party of Registration
          V201019,                 # Pre: Does R Intend to Register
          V201020,                 # Pre: Did R Vote in a Primary or Caucus
          V201022,                 # Pre: Already Voted in General Election
          V201023,                 # Pre: Confirmation of Early Vote
          V201025x,                # Pre: Summary Registration and Early Vote
          V201228,                 # Pre: Party Identification
          V201229,                 # Pre: Party Identification Strong
          V201230,                 # Pre: Party Identification Leaning
          V201231x,                # Pre: Party Identification Summary
          V202051,                 # Post: R Registered to Vote
          V202064,                 # Post: Party of Registration
          V202066,                 # Post: Voted in General Election
          V202109x,                # Pre-Post: Voter Turnout 
          V202119,                 # Post: Difficulty Level (if voted)
          starts_with ("V202120"), # Post: [Range] Voting Problems (if voted)
          V202123) %>%             # Post: Main Reason for Not Voting
  
  # make a copy of the Case ID
  mutate (case_id = V200001, .before = V200001) %>%
  

  # Pull party ID from Anes summary variable
  mutate  (party = case_when (
    V201231x >= 1 & V201231x <= 3 ~ "Democrat",
    V201231x >= 5 & V201231x <= 7 ~ "Republican"),
    .after = case_id) %>%  
  
  # Pull whether individual voted from Anes summary variable
  mutate (voted = case_when (
    V202109x == 0 ~ FALSE,
    V202109x == 1 ~ TRUE),
    .after = party) %>%
  
  # Eliminate non-response values from Anes difficulty level variable
  mutate (voted.difficulty_level = ifelse (V202119 < 0, NA, V202119),
          .after = voted) %>%
  
  # Derive whether a voter mentioned voting problems by looking across
  # the relevant range of Anes variables
  mutate (voted.problem_mentioned = case_when (
    V202120a == 1 ~ TRUE,                # Registration Problem
    V202120b == 1 ~ TRUE,                # ID Card Problem
    V202120c == 1 ~ TRUE,                # Getting Absentee Ballot Problem
    V202120d == 1 ~ TRUE,                # Confusion re Ballot or Voting Machine
    V202120e == 1 ~ TRUE,                # Difficulty Getting to Polling Place
    V202120f == 1 ~ TRUE,                # Long Wait Times
    V202120g == 1 ~ TRUE,                # Work Schedule
    V202120h == 1 ~ TRUE,                # Bad Weather
    V202120i == 1 ~ TRUE,                # Mailing Ballot
    V202120j == 1 ~ TRUE,                # Other Problem
    V202120k == 1 ~ FALSE),              # Mentioned Lack of Problems
    .after = voted.difficulty_level) %>%
  
  # Respondents who did not vote can explain the primary reason for not
  # voting.  Most of the response alternatives correspond to things that
  # would have been captured by the prompt to voters concerning difficulties
  # they encountered.  We take these kinds of responses as indicators that the
  # respondent intended to vote but was unable to complete the act of voting, 
  # when this the we will presume that the respondent is a voter for purposes
  # of this study.
  # 
  # The first item we address is where someone did not vote due to not being
  # registered.  Here, we look to see if the respondent previously expressed
  # an intent to register and if so, we consider the respondent a presumed 
  # voter.  Otherwise, we view this is a person who never intended to vote.
  # Of the 176 respondents we presume to be voters, 31 are included because
  # they failed to be registered despite earlier expressing an intent to 
  # register to vote.
  # 
  # We disregard "I forgot", "not interested", "too busy", "did not like the
  # candidates", "I didn't know enough" and "other" as we believe these do
  # not support an inference that the respondent wanted to vote encountered
  # difficulty that thwarted the attempt.
  # 
  # In addition to events that are the same as events that voters are prompted
  # about (which we include) we are including "out of town" and "sick or
  # disabled" as indicating someone who intended to vote but did not due to
  # difficulty.  Of the 176 respondents we presume to be voters, 20 are included
  # because they said they did not vote because they were out of town and 61 
  # because they said they were sick or disabled.
  
  # Identify presumptive voters by reviewing the reason for not voting.
  mutate (presumed = case_when (
    V202123 == 5 & V201019 == 1 ~ TRUE,   # Presumed Registration Problem
    V202123 > 5 & V202123 < 14 ~ TRUE,    # Subset of Reasons for Not Voting
    TRUE ~ FALSE),
    .after = voted.problem_mentioned) %>% 

  # voted.difficulty_level with inferred difficulty for presumed voters
  # a presumed voter is deemed to have had the highest level of difficulty
  # since he or she was not able to vote
  
  # Capture the reason for not voting.
  mutate (presumed.reason = ifelse (V202123 < 0, NA, V202123),
          .after = presumed) %>%

  mutate (presumed.difficulty_level = ifelse (presumed, 5, NA),
          .after = presumed.reason) %>%
  
  # We deem someone to be a voter if they voted or if they are among those
  # we presume to be a voter but who were frustrated in their attempt to vote.
  mutate (voter = voted | presumed, 
          .after = case_id) %>%
  
  mutate (voter.difficulty_level = case_when(
    presumed ~ presumed.difficulty_level,
    TRUE ~ voted.difficulty_level),
    .after = party)
          
# Coerce party to a factor  
anes_data$party <- factor(anes_data$party)
  
# Coerce voted.difficulty_level to an ordered factor and add labels
anes_data$voted.difficulty_level <- 
  ordered(anes_data$voted.difficulty_level, 
          levels = seq(1:5), 
          labels = c("not",
                     "little",
                     "moderate",
                     "very",
                     "extreme"))

# Coerce presumed.difficulty_level to an ordered factor and add labels
anes_data$presumed.difficulty_level <- 
  ordered(anes_data$presumed.difficulty_level, 
          levels = seq(1:5), 
          labels = c("not",
                     "little",
                     "moderate",
                      "very",
                      "extreme"))

# Coerce voter.difficulty_level to an ordered factor and add labels
anes_data$voter.difficulty_level <- 
  ordered(anes_data$voter.difficulty_level, 
          levels = seq(1:5),
          labels = c("not",
                     "little",
                     "moderate",
                     "very",
                     "extreme"))

# Coerce presumed reason to a factor and add labels.  
anes_data$presumed.reason <-
  factor(anes_data$presumed.reason,
         labels = c("forgot",
                    "not interested",
                    "too busy",
                    "candidates",
                    "not registered",
                    "lacked correct id",
                    "out of town",
                    "sick or disabled",
                    "transportation",
                    "bad weather",
                    "poll line too long",
                    "denied at polls",
                    "absentee ballot problem",
                    "did not know where to vote",
                    "lacked information about choices",
                    "other"))

# Remove information about any respondent we do not consider to be a voter.

anes_data <- anes_data %>% 
  filter(voter == TRUE) %>%
  select(-voter)

saveRDS(anes_data,file = "team_1_anes_data.rds")

save(study.count,
     study.party_identity_table,
     study.voting_table,
     study.non_vote_reason_table,
     study.non_vote_inferred_failed_registration_count,
     file = "team_1_anes_data_cleaning_statistics.Rdata")
