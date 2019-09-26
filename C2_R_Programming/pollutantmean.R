get_observations_from_dir <- function(directory, id) {
    col_formats = cols(
        Date = col_date(format = ""),
        sulfate = col_double(),
        nitrate = col_double(),
        ID = col_double()
    )
    
    x <- tibble(id = id) %>%
        mutate(
            filename = map_chr(id, ~paste(sprintf("%03d", .x), '.csv', sep = '')),
            path = map_chr(filename, ~file.path(directory, .x)),
            data = map(path, ~read_csv(file = .x, col_types = col_formats))
        ) %>%
        unnest(data)
    
    return(x)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    x <- get_observations_from_dir(directory, id)
    
    return( mean(x[[pollutant]], na.rm = T) )
}


complete <- function(directory, id = 1:332) {
    x <- get_observations_from_dir(directory, id)
    
    complete <- x %>%
        na.omit() %>%
        group_by(ID) %>%
        summarise(nobs = n()) %>%
        arrange(order(match(ID, id))) %>%
        as.data.frame()
    
    return(complete)
}


corr <- function(directory, threshold = 0) {
    x <- get_observations_from_dir(directory, id = 1:332)
    
    # We get a list of IDs above the threshold
    above_threshold_ids <- x %>%
        filter(!(is.na(sulfate) & is.na(nitrate))) %>%
        group_by(id) %>%
        tally() %>%
        filter(n > threshold) %>%
        pull(id)
    
    # Filter out the entries that are below the threshold
    correlations <- x %>%
        na.omit() %>%
        filter(id %in% above_threshold_ids) %>%
        group_by(id) %>%
        summarise(correlation = cor(sulfate, nitrate,)) %>%
        pull(correlation)
    
    return(correlations)
}

