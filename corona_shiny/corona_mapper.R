corona_mapper <- function(date = '2020-04-21', mode = 'Province/State', cate = 'confirmed'){
    # Load mapbox token from my local dir
    mapboxToken <- readLines("mapbox_token.txt")    # for test "E:/COVID19_SEIR/corona_shiny/"
    Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca
    # Load the data from Johns Hopkins Github repo
    daily_data_global = read.csv("time_series_covid19_confirmed_global.csv")
    daily_death_global = read.csv("time_series_covid19_deaths_global.csv")

    # Remove invalid country data from the global data df as 
    zero_coor <- which(daily_data_global['Lat'] == 0 | is.na(daily_data_global['Lat']))
    daily_data_global <- daily_data_global[-zero_coor,]
    daily_death_global <- daily_death_global[-zero_coor,]

    # Load the coordinates of countries
    country_geo_info = read.csv("GeoInfo.csv")
    # Process the input date value
    date_value <- strsplit(date, "-")
    year <- as.numeric(date_value[[1]])[1] %% 100
    month <- as.numeric(date_value[[1]])[2]
    day <- as.numeric(date_value[[1]])[3]
    converted_date_value <- paste0('X',month,'.',day,'.',year)

    # Process the data
    if(mode == 'Country/Region'){
        daily_data <- daily_data_global
        death_data <- daily_death_global

        # Combine the province data for some countires
        country_names <- unique(daily_data['Country.Region'])
        combined_country <- c('Canada', 'China', 'Australia')
        for(country in combined_country){
        rows_temp <- which(daily_data['Country.Region'] == country)
        extract_temp <- daily_data[rows_temp,]
        extract_death_temp <- death_data[rows_temp,]
        lat_temp <- country_geo_info[which(country_geo_info[,4] == country), 2]
        lon_temp <- country_geo_info[which(country_geo_info[,4] == country), 3]
        combined_extract_temp <- data.frame(Province.State = "", Country.Region = country, Lat = lat_temp, Long = lon_temp)


        combined_data <- t(data.frame(colSums(extract_temp[,5:ncol(extract_temp)])))
        temp_country_data_df <- cbind(combined_extract_temp,combined_data)
        combined_death_data <- t(data.frame(colSums(extract_death_temp[,5:ncol(extract_death_temp)])))
        temp_country_death_data_df <- cbind(combined_extract_temp,combined_death_data)
        rownames(temp_country_data_df) <- NULL
        rownames(temp_country_death_data_df) <- NULL

        daily_data <- daily_data[-rows_temp,]
        daily_data <- rbind(daily_data,temp_country_data_df)
        death_data <- death_data[-rows_temp,]
        death_data <- rbind(death_data,temp_country_death_data_df)
        }
    }else if(mode == 'Province/State'){
        daily_data <- daily_data_global
        death_data <- daily_death_global

        # Load the US cities data from Johns Hopkins Github repo
        daily_data_us = read.csv("time_series_covid19_confirmed_US.csv")  # for test "E:/COVID19_SEIR/corona_shiny/"
        death_data_us = read.csv("time_series_covid19_deaths_US.csv")

        # Load the coordinates of US states
        usstate_geo_info = read.csv("GeoInfoUS.csv")
        us_row <- which(daily_data['Country.Region'] == 'US')
        daily_data <- daily_data[-us_row,]
        death_data <- death_data[-us_row,]

        # Process state data of US 
        # Remove the invalid rows
        non_state_data_row <- which(daily_data_us['Lat'] == 0 | is.na(daily_data_us['Lat']))
        daily_data_us <- daily_data_us[-non_state_data_row,]
        death_data_us <- death_data_us[-non_state_data_row,]

        state_names  <- as.character(unique(daily_data_us['Province_State'][,1]))
        for(state in state_names){
            rows_temp <- which(daily_data_us['Province_State'] == state)
            extract_temp <- daily_data_us[rows_temp,]
            extract_death_temp <- death_data_us[rows_temp,]

            if(state %in% unique(usstate_geo_info[,4])){
                lat_temp <- usstate_geo_info[which(usstate_geo_info[,4] == state), 2]
                lon_temp <- usstate_geo_info[which(usstate_geo_info[,4] == state), 3]
                combined_extract_temp <- data.frame(Province.State = state, Country.Region = "US", Lat = lat_temp, Long = lon_temp)

                combined_data <- t(data.frame(colSums(extract_temp[,12:ncol(extract_temp)])))
                temp_state_data_df <- cbind(combined_extract_temp,combined_data)
                combined_death_data <- t(data.frame(colSums(extract_death_temp[,13:ncol(extract_death_temp)])))
                temp_state_death_data_df <- cbind(combined_extract_temp,combined_death_data)

                rownames(temp_state_data_df) <- NULL
                rownames(temp_state_death_data_df) <- NULL

                daily_data <- rbind(daily_data,temp_state_data_df)
                death_data <- rbind(death_data,temp_state_death_data_df)

            }else{
                temp_extrstate_data_df <- extract_temp[,c(c(7:10),c(12:ncol(daily_data_us)))]
                temp_extrstate_death_data_df <- extract_death_temp[,c(c(7:10),c(13:ncol(death_data_us)))]

                combined_extract_temp <- temp_extrstate_data_df[1,c(1:4)]
                colnames(combined_extract_temp) <- c('Province.State', 'Country.Region', 'Lat', 'Long')

                combined_data <- t(data.frame(colSums(temp_extrstate_data_df[,5:ncol(temp_extrstate_data_df)])))
                temp_state_data_df <- cbind(combined_extract_temp,combined_data)
                combined_death_data <- t(data.frame(colSums(temp_extrstate_death_data_df[,5:ncol(temp_extrstate_death_data_df)])))
                temp_state_death_data_df <- cbind(combined_extract_temp,combined_death_data)

                rownames(temp_state_data_df) <- NULL
                rownames(temp_state_death_data_df) <- NULL

                daily_data <- rbind(daily_data,temp_state_data_df)
                death_data <- rbind(death_data,temp_state_death_data_df)
            }
            
        }
    }else{
        print('Please specify the scale!')
    }

    data_col <- which(colnames(daily_data) == converted_date_value)

    daily_data_plot <- cbind(
        daily_data[,1:4], # geo information
        daily_data[,data_col], log( apply(cbind(200*(daily_data[,data_col])+1,1),1,max)), # total confirmed cases
        death_data[,data_col], log( apply(cbind(400*(death_data[,data_col])+1,1),1,max)), # death cases
        daily_data[,data_col] - daily_data[,(data_col-1)], log( apply(cbind(400*(daily_data[,data_col] - daily_data[,(data_col-1)])+1,1),1,max) ) # new confirmed cases
    )

    colnames(daily_data_plot)[5:10] <- c('confirm', 'confirmsize', 'death', 'deathsize', 'increase', 'increasesize')

    # Colors for different scales and categories
    if(cate == 'confirmed'){
        dot_color <- if(mode == 'Country/Region') "#D0104C"  else "#D0104C"
        daily_data_plot <- cbind(daily_data_plot, daily_data_plot['confirm'], daily_data_plot['confirmsize'])
        colnames(daily_data_plot)[11:12] <- c('focal', 'focalsize')
    }else if(cate == 'new'){
        dot_color <- if(mode == 'Country/Region') "#EFBB24"  else "#EFBB24"
        daily_data_plot <- cbind(daily_data_plot, daily_data_plot['increase'], daily_data_plot['increasesize'])
        colnames(daily_data_plot)[11:12] <- c('focal', 'focalsize')
    }else{ # death
        dot_color <- if(mode == 'Country/Region') "#BDC0BA" else "#BDC0BA"
        daily_data_plot <- cbind(daily_data_plot, daily_data_plot['death'], daily_data_plot['deathsize'])
        colnames(daily_data_plot)[11:12] <- c('focal', 'focalsize')
    }



    fig <- daily_data_plot
    fig <- fig %>%
    plot_ly(
        lat = ~Lat,
        lon = ~Long,
        marker = list(color = dot_color,
        size = ~focalsize,
        opacity = 0.6),
        type = 'scattermapbox',
        mode = 'markers',
        text = ~paste(Country.Region, '<br>', Province.State,'<br>Infection: ',confirm, '<br>New infection: ', increase, '<br>Death: ', death)
    )
    fig <- fig %>%
    layout(
        mapbox = list(
        style = 'dark',
        zoom =1,
        center = list(lon = -88, lat = 34)),
        margin =list(l=0,t=0,b=0,r=0),
        hoverlabel = list(bgcolor = 'rgb(32,32,32)', bordercolor = 'rgb(32,32,32)', font = list(size = 15,color = 'rgb(255,255,255)'))
        )
    fig <- fig %>%
    config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

    fig
}
