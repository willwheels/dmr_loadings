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

## add year to function

scrape_dmr_loadings <- function(p_maj, p_potw, p_poll_cat, p_huc_region) {
  
  legal_poll_cats <- c("NutN", "NutP", "ORG", "SS", "MET", "PP", "CER", "TRI", "RAD", "PAT", "Temp", "WW", "GRAD",
                       "CLR", "WET", "PFAS")

  
  
  if (!(p_maj %in% c("Y", "N"))) {stop("p_maj must be Y or N")}
  if (!(p_poll_cat %in% legal_poll_cats )) {stop("p_poll_cat not in allowable list, I don't make the rules")}
  
  base_params <- list(output = "CSV",
                      p_end_date = "12/2022",
                      p_est = "Y",
                      p_fac_type = "POTW",
                      p_loads_data = "DMR",
                      p_nd = "ZERO",
                      p_param_group = "Y",
                      p_start_date = "01/2022",
                      p_year = "2022"
  )
  
  params <- base_params 
  
  if (p_poll_cat %in% c("NutN", "NutP")) {p_nutrient_agg = "Y"}
  params$p_maj <- p_maj
  params$p_poll_cat <- p_poll_cat
  params$p_potw <- p_potw
  params$responseset <- "100000"  # maximum allowed but if you really call this many, a 503 is likely
  params$output <- "CSV"
  params$suppressheaders <- "Y"
  
  dmr_loads <- httr::GET(
    url = "https://echodata.epa.gov",
    path = "echo/dmr_rest_services.get_custom_data_monitor_pd",
    query = params)
  
  
  
  #https://echodata.epa.gov/echo/dmr_rest_services.get_custom_data_monitor_pd?output=CSV&p_end_date=12%2F2022&p_est=Y&p_fac_type=POTW&p_loads_data=DMR&p_maj=Y&p_nd=ZERO&p_nutrient_agg=Y&p_param_group=N&p_poll_cat=NUTN&p_st=DC&p_start_date=01%2F2022&p_year=2022&pageno=1
  
  source("readr col specification.R")
  
  return_df <- dmr_loads %>%
    httr::content("text") %>%
    read_csv(col_types =  cols) %>%
    #read_csv(skip = 3) %>%
    janitor::clean_names()
  

  
  
    return(return_df)
}

junk <- scrape_dmr_loadings(p_maj = "Y", p_potw = "Y", p_poll_cat = "WW", p_huc_region = "01")









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



