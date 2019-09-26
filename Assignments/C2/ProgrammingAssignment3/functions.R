library(datasets)

best <- function(state, outcome) {
    check_inputs(state, outcome)
    
    outcome_column <- rlang::sym(column_mapping(outcome))
    
    hospital_outcomes <- read_outcome_data()
    
    hospital_outcomes %>%
        mutate(selected_outcome = as.double(!!outcome_column)) %>%
        filter(State == state & !is.na(selected_outcome)) %>%
        filter(selected_outcome == min(selected_outcome)) %>%
        arrange('Hospital Name') %>%
        pull('Hospital Name')-> best_hospital
    
    return(best_hospital)
}


rankhospital <- function(state, outcome, num = "best") {
    check_inputs(state, outcome)
    outcome_column <- rlang::sym(column_mapping(outcome))
    hospital_outcomes <- read_outcome_data() %>% filter(!is.na(!!outcome_column))
    
    #if (!is.integer(num) | !num %in% c('best', 'worst')) {
    #    stop("'num' must be an integer, 'best', or 'worst'")
    #}
    
    rank_row <- select_rank_row(num)
    
    hospital_outcomes %>%
        filter(State == state) %>%
        arrange(`Hospital Name`) %>%
        mutate(rank = rank(!!outcome_column, ties.method = 'first')) %>%
        filter(rank == rank_row(rank)) %>%
        pull(`Hospital Name`) %>%
        ifelse(length(.) == 0, NA, .) -> hospital_ranking

    
    return(hospital_ranking)
}

rankall <- function(outcome, num = "best") {
    check_outcome(outcome)
    
    outcome_column <- rlang::sym(column_mapping(outcome))
    
    hospital_outcomes <- read_outcome_data() %>% filter(!is.na(!!outcome_column))
    
    states = tibble(State = c(state.abb, 'DC'))
    
    rank_row <- select_rank_row(num)
    
    hospital_outcomes %>%
        arrange(`Hospital Name`) %>%
        group_by(State) %>%
        mutate(rank = rank(!!outcome_column, ties.method = 'first')) %>%
        filter(rank == rank_row(rank)) %>%
        ungroup() %>%
        select(`Hospital Name`, State) %>%
        full_join(states) %>%
        arrange(State, `Hospital Name`) %>%
        as.data.frame() -> ranked_hospitals
    
    return (ranked_hospitals)
}

select_rank_row <- function(num) {
    rank_row <- NULL
    if (num == 'best') { rank_row = function(x) min(x) }
    else if (num == 'worst') { rank_row = function(x) max(x) }
    else { rank_row <- function(rank) num }
    
    return(rank_row)
}

check_inputs <- function(state, outcome) {
    check_state(state)
    check_outcome(outcome)
}

check_state <- function(state) {
    if (!state %in% state.abb) { 
        stop('Invalid state: ', state) 
    }
}

check_outcome <- function(outcome) {
    if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
        stop("Invalid outcome: ", outcome)
    }
}


read_outcome_data <- function() {
    base::suppressMessages(read_csv)
    
    outcome_data <- read_csv(
        file = 'outcome-of-care-measures.csv',
        col_types = outcome_col_types()
    )
    
    return(outcome_data)
}


column_mapping <- function(outcome) {
    outcome_map <- list(
        'heart attack' = 'Hospital 30-Day Death (Mortality) Rates from Heart Attack',
        'heart failure' = 'Hospital 30-Day Death (Mortality) Rates from Heart Failure',
        'pneumonia' = 'Hospital 30-Day Death (Mortality) Rates from Pneumonia'
    )
    
    return( outcome_map[[outcome]] )
}

outcome_col_types <- function() {
    c <- cols(
        `Provider Number` = col_character(),
        `Hospital Name` = col_character(),
        `Address 1` = col_character(),
        `Address 2` = col_logical(),
        `Address 3` = col_logical(),
        City = col_character(),
        State = col_character(),
        `ZIP Code` = col_character(),
        `County Name` = col_character(),
        `Phone Number` = col_double(),
        `Hospital 30-Day Death (Mortality) Rates from Heart Attack` = col_double(),
        `Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Attack` = col_character(),
        `Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack` = col_character(),
        `Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack` = col_character(),
        `Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack` = col_character(),
        `Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack` = col_character(),
        `Hospital 30-Day Death (Mortality) Rates from Heart Failure` = col_double(),
        `Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Failure` = col_character(),
        `Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure` = col_character(),
        `Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure` = col_character(),
        `Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure` = col_character(),
        `Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure` = col_character(),
        `Hospital 30-Day Death (Mortality) Rates from Pneumonia` = col_double(),
        `Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Pneumonia` = col_character(),
        `Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia` = col_character(),
        `Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia` = col_character(),
        `Number of Patients - Hospital 30-Day Death (Mortality) Rates from Pneumonia` = col_character(),
        `Footnote - Hospital 30-Day Death (Mortality) Rates from Pneumonia` = col_character(),
        `Hospital 30-Day Readmission Rates from Heart Attack` = col_character(),
        `Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Attack` = col_character(),
        `Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack` = col_character(),
        `Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack` = col_character(),
        `Number of Patients - Hospital 30-Day Readmission Rates from Heart Attack` = col_character(),
        `Footnote - Hospital 30-Day Readmission Rates from Heart Attack` = col_character(),
        `Hospital 30-Day Readmission Rates from Heart Failure` = col_character(),
        `Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Failure` = col_character(),
        `Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure` = col_character(),
        `Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure` = col_character(),
        `Number of Patients - Hospital 30-Day Readmission Rates from Heart Failure` = col_character(),
        `Footnote - Hospital 30-Day Readmission Rates from Heart Failure` = col_character(),
        `Hospital 30-Day Readmission Rates from Pneumonia` = col_character(),
        `Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Pneumonia` = col_character(),
        `Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia` = col_character(),
        `Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia` = col_character(),
        `Number of Patients - Hospital 30-Day Readmission Rates from Pneumonia` = col_character(),
        `Footnote - Hospital 30-Day Readmission Rates from Pneumonia` = col_character()
    )
    
    return(c)
}