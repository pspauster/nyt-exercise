library(tidyverse)
library(tidycensus)
library(blsAPI) #this package rules! makes it so much easier
library(rjson)

##############START WITH BLS ####################################

# NAICS code for apparel 31500000"

################## FOR THE COUNTRY #######################

#10 year limit
bls_api_info <- list(
  seriesid="CES3231500001",
  startyear = "2014",
  endyear = "2024"
)

bls_resp <- blsAPI(bls_api_info) %>% fromJSON()

vars_ac5s_23 <- load_variables(2023,"acs5")

apiDF <- function(data) {
  df  <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE))
  colnames(df) <- c("year", "period", "periodName", "value")
  return(df)
}

#the industry declining nationwide
apparel_manufacturing_total  <- apiDF(bls_resp$Results$series[[1]]$data)

#How many are women?
#women: CEU3231500010

women_api_info <- list(
  seriesid="CEU3231500010",
  startyear = "2014",
  endyear = "2024"
)

women_resp <- blsAPI(women_api_info) %>% fromJSON()

women_total  <- apiDF(women_resp$Results$series[[1]]$data) %>% 
  left_join(apparel_manufacturing_total, by = c("year", "period", "periodName")) %>% 
  mutate(share_women = as.numeric(value.x)/as.numeric(value.y))

################# BY COUNTY NATIONWIDE ######################

#for each year, pull the national count employment and wage data for the industry (year average)
apparel_data_time <- map_df(as.character(2014:2023),
                            ~blsQCEW('Industry', year=.x, quarter='a', industry = "315")) %>% 
  left_join(fips_codes %>% 
              mutate(area_fips = str_c(state_code, county_code)),
            by = "area_fips")

#what counties nationwide have the most
counties_sum <- apparel_data_time %>% 
  filter(!is.na(state), year %in% c(2014, 2023), own_code=="5") %>% #just private just counties
  select(year, annual_avg_emplvl, avg_annual_pay, state, county, area_fips) %>% 
  pivot_wider(id_cols = c(area_fips, county, state), names_from = year, values_from = c("annual_avg_emplvl","avg_annual_pay"))%>% 
  mutate(job_change = annual_avg_emplvl_2023-annual_avg_emplvl_2014,
         pay_change = avg_annual_pay_2023-avg_annual_pay_2014,
         per_job_change = job_change/annual_avg_emplvl_2014,
         per_pay_change = pay_change/avg_annual_pay_2014,
         )

#biggest losers
counties_sum %>% 
  filter(annual_avg_emplvl_2014 >1000) %>% 
  arrange(desc(per_job_change))

############## FOR CAROLINA COUNTIES ###########################

#look particularly at NC/SC
carolinas_apparel <- apparel_data_time %>% 
  mutate(state_fips = str_sub(area_fips, 1, 2)) %>% 
  filter(state_fips %in% c("37", "45")) #FIPS FOR NC AND SC

#The industry has been in decline for the last decade
#employment_by_year
carolinas_apparel %>% 
  filter(area_fips %in% c("37000", "45000")) %>% 
  select(area_fips, annual_avg_emplvl, year) %>% 
  arrange(area_fips, desc(year))

#Top counties for manufacturing employment
#employment_by_county
carolinas_apparel %>% 
  filter(year == 2023) %>% 
  arrange(desc(annual_avg_emplvl)) %>% 
  select(area_fips,state_code, county, annual_avg_emplvl) %>% 
  head(12)

#Pay not keeping up with inflation
#pay_by_year
carolinas_apparel %>% 
  filter(area_fips %in% c("37000", "45000")) %>% 
  select(area_fips, avg_annual_pay, year) %>% 
  arrange(area_fips, desc(year))

## Other series I would investigate with more time - I see them broken out by subindustry here:
## I could aggregate and get characteristics of workers

## median years (age): LNU02083800
## hispanic: LNU02073160
## black: LNU02072512
## white: LNU02094828
## asian: LNU02072835

#################### CENSUS #################################

vars_ac5s_23 <- load_variables(2023,"acs5")

acs_counties <- get_acs(
  year = 2023,
  variables = c("total" = "B03002_001",
                "not_hisp" = "B03002_002",
                "hispanic_latino" = "B03002_012",
                "denom_foreign" = "B05001_001",
                "total_foreign" = "B05001_002"),
  geography = "county",
)
















