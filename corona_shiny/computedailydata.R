computedailydata <- function(date){
    # Load the data from Johns Hopkins Github repo
    daily_data_global = read.csv("time_series_covid19_confirmed_global.csv")
    daily_death_global = read.csv("time_series_covid19_deaths_global.csv")

    # Process the input date value
    date_value <- strsplit(date, "-")
    year <- as.numeric(date_value[[1]])[1] %% 100
    month <- as.numeric(date_value[[1]])[2]
    day <- as.numeric(date_value[[1]])[3]
    converted_date_value <- paste0('X',month,'.',day,'.',year)
    last_date <- if(day-1 == 0) paste0('X',(month-1),'.',30,'.',year) else paste0('X',month,'.',(day-1),'.',year)
    total_cases <- sum(daily_data_global[converted_date_value])
    newly_cases <- total_cases - sum(daily_data_global[last_date])
    total_death_cases <- sum(daily_death_global[converted_date_value])

    return(list(total_cases, newly_cases, total_death_cases))
}
