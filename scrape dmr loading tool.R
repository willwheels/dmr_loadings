library(httr)
library(tidyverse)
library(jsonlite)

## pollutant categories

# NutN = Nitrogen
# NutP = Phosphorus
# ORG = Organic Enrichment
# SS = Solids
# MET = Metals
# PP = Clean Water Act Priority Pollutants
# CER = CERCLA Hazardous Substances
# TRI = TRI Chemicals
# RAD = Radionuclides
# PAT = Pathogen Indicators
# Temp = Temperature
# WW = Wastewater Flow
# GRAD = General Radioactivity
# CLR = Color
# WET = Whole Effluent Toxicity
# PFAS = Per- and Polyfluoroalkyl Substances (PFAS)

# p_maj Y/N seems to diff between majors and minors
# p_maj = "Y" 53108 query rows
# p_fac_type = "POTW" also 53108 rows

scrape_dmr_loadings <- function(p_maj, p_poll_cat) {
  
  legal_poll_cats <- c("NutN", "NutP", "ORG", "SS", "MET", "PP", "CER", "TRI", "RAD", "PAT", "Temp", "WW", "GRAD",
                       "CLR", "WET", "PFAS")

  
  if (!(p_maj %in% c("Y", "N"))) {stop("p_maj must be Y or N")}
  if (!(p_poll_cat %in% legal_poll_cats )) {stop("p_poll_cat not in allowable list, I don't make the rules")}
  
  base_params <- list(output = "JSON",
                      p_end_date = "12/2019",
                      p_est = "Y",
                      p_fac_type = "POTW",
                      p_loads_data = "DMR",
                      p_nd = "ZERO",
                      p_nutrient_agg = "Y",
                      p_param_group = "Y",
                      p_start_date = "01/2019",
                      p_year = "2018"
  )
  
  params <- base_params 
  params$p_maj <- p_maj
  params$p_poll_cat <- p_poll_cat
  

  #need to add pageno


  
  dmr_loads <- httr::GET(
    url = "https://ofmpub.epa.gov",
    path = "echo/dmr_rest_services.get_custom_data_monitor_pd",
    query = params)
  
  return_df <- dmr_loads %>%
    httr::content("text") %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("Results") %>%
    purrr::pluck("Results")
  

  
  
  for (i in seq(101,102)){
    
    params$pageno <- i
    
    # Start the clock!
    ptm <- proc.time()
    
    dmr_loads_loop <- httr::GET(
      url = "https://ofmpub.epa.gov",
      path = "echo/dmr_rest_services.get_custom_data_monitor_pd",
      query = params)
    
    return_loop <- dmr_loads_loop %>%
      httr::content("text") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("Results") %>%
      purrr::pluck("Results")
    
    return_df <- rbind(return_df, return_loop)  ## not currently working
    
    # Stop the clock
    ptm2 <- proc.time() - ptm
    
    print(c(i, ptm2))

  }
  
    return(return_df)
}

junk <- scrape_dmr_loadings("Y", "NutN")


#### testing below this

params <- list(output = "JSON",
                    p_end_date = "12/2018",
                    p_est = "Y",
                    p_fac_type = "POTW",
                    p_loads_data = "DMR",
                    p_nd = "ZERO",
                    p_nutrient_agg = "Y",
                    p_param_group = "Y",
                    p_start_date = "01/2018",
                    p_year = "2018",
                    responseset = 1000,
                    p_maj = "Y",
                    p_poll_cat = "NutN"
)






dmr_loads <- httr::GET(
    url = "https://ofmpub.epa.gov",
    path = "echo/dmr_rest_services.get_custom_data_monitor_pd",
    query = params)

test <- dmr_loads %>%
  httr::content("text") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("Results") %>%
  purrr::pluck("Results")


length(test) ## 1 if error, 8 if not

"Error" %in% names(test) ## true if error, false if not

## after first pull

## total num rows
test$QueryRows

## num pages
test$PageCount

## if not an error

test <- test %>%
  purrr::pluck("Results")

## can return error
  
