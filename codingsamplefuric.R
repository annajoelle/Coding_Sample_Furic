###########################CODING SAMPLE########################################

#This code is aimed at anaylsing plant level time series data on the cement
#manufacturing industry during the Great Depression.
#The dataset includes data on a wide range of variables every two years from 1929 to 1935.
#In particular, the research question associated with the dataset was whether
#multimarket contacts lead to more collusion among firms (i.e : in are prices higher
#in markets - here defined geographically due to specifities of the cement indutstry -
#where plants belong to firms that interact on other markets as well)
#To answer this question : we start by cleaning the dataset, then we allocate plants 
#to on of 25 markets in the US based on the county they are located in, then we create
#functions that allow us to build a measure of the level of multimarket contacts
#between parent firms in each market and a measure of prices, then we apply these
#functions to the dataset for each year of observations and finally we combine these
#results into a panel dataframe. This final dataset allows us to run regressions
#to test the relation between multimarket contacts and price levels.


library(haven)
library(ggplot2)
library(readxl)
library(openxlsx)
library(readr)
library(tidyr)
library(tidyverse)
library(AER)
library(broom)
library(dplyr)
library(jtools)
library(huxtable)
library(stargazer)
library(igraph)
library(gtools)
library(plm)

##### We load the initial data frame====
data0 <- read_dta("Données historiques ciment avec quantités sans flags.dta")

##### We clean the data frame====
#### Automated cleaning ====
data0$a002[data0$a002 == "Same"] <- data0$a001[data0$a002 == "Same"]
data0$a002[data0$a002 == ""] <- data0$a001[data0$a002 == ""]
data0$a001[data0$a001 == ""] <- data0$a002[data0$a001 == ""]

data0$a002 <- gsub(",","",data0$a002)
data0$a002 <- gsub("\\.","",data0$a002)
data0$a002 <- gsub("\\bCo\\b","Company",data0$a002)
data0$a002 <- gsub("\\bCorp\\b","Corporation",data0$a002)
data0$a002 <- gsub("\\bInc\\b","Incorporated",data0$a002)
data0 <- data0[!grepl("Above", data0$a002),]
data0 <- data0[-108, ]
#### Cleaning specific variables :
### Data cleaning for firm name variable ====
data0$a002[data0$a002=="A Corporation"] <- data0$a001[data0$a002]
data0$a002[data0$a002=="Blue Diamond Corporation Ltd"] <- "Blue Diamond Company"
data0$a002[data0$a002=="Louisville Cement Company of N Y"] <- "Louisville Cement Company"
data0$a002[data0$a002=="Louisville Cement Company of New York"] <- "Louisville Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas (Plant No 1)"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas Plant No 2"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Company"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Century Cement Mfg Company Incorporated"] <- "Century Cement Corporation"
data0$a002[data0$a001=="Cumberland Portland Cement Company Cowan Tennessee"] <- "Cumberland Portland Cement Company"
data0$a002[data0$a001=="Trinity Portland Cement Company" | data0$a001=="Trinity Portland Cement Co."] <- "Trinity Portland Cement Company"
data0$a002[data0$a001=="Castalia Portland Cement Co."] <- "Castalia Portland Cement Company"
data0$a002[data0$a001=="Wabash Portland Cement Co-Osborn Ohio"] <- "Wabash Portland Cement Corporation"
data0$a002[data0$a001=="Great Lakes Portland Cement Corporation"] <- "Great Lakes Portland Cement Corporation"
data0$a002[data0$a002=="Glens Falls Portland Cement Company (One Plant Only)"] <- "Glens Falls Portland Cement Company"
data0$a002[data0$a001=="Idaho Portland Cement Co."] <- "Idaho Portland Cement Company"
data0$a002[data0$a002=="Kosmos Portland Cement Company Incorporated" | data0$a002=="Kosmos Portland Cement Company Ind" | data0$a002 == "Kosmos Portland Cement Company, Plant"] <- "Kosmos Portland Cement Company"
data0$a002[data0$a001=="Leigh Portland Cement Company"] <- "Lehigh Portland Cement Company"
data0$a002[data0$a001=="Leigh Portland Cement Company"] <- "Lehigh Portland Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company-Pennsylvania"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Alabama"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Alabama Plant No 1"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Ind Incorporated"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Indiana Incorporated"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Louisiana"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company New York Incorporated"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Pennsylvania"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas Plant No 1"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas-Plant No 2"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas Plant-No 2"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas (Plant No. 1)"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Texas (Plant No 2)"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Company Virginia Incorporated"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Corpn (A Virginia Corpn)"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Corporation"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Corporation-A Virginia Corporation"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Lone Star Cement Corporation (A Virginia Corporation)"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Howard M Thomas Manager"] <- "Fort Scott Hydraulic Cement Company"
data0$a002[data0$a002=="Louisville Cement Corporation"] <- "Louisville Cement Company"
data0$a002[data0$a002=="Marquette Cement Mfg Company"] <- "Marquette Cement Manufacturing Company"
data0$a002[data0$a002=="Oregon Portland Cement Company L E Newlands (Pres)"] <- "Oregon Portland Cement Company"
data0$a002[data0$a002=="Pennsylvania-Dixie Cement Corpn"] <- "Pennsylvania-Dixie Cement Corporation"
data0$a002[data0$a002=="Pennsylvania-Dixie Cement Corpn"] <- "Pennsylvania-Dixie Cement Corporation"
data0$a002[data0$a002=="Subsidiary of The Universal Atlas Portland Cement Company)"] <- "The Universal Atlas Portland Cement Company"
data0$a002[data0$a002=="Superior Portland Cement Incorporated Operator"] <- "Superior Portland Cement Incorporated"
data0$a002[data0$a002=="The Carneys Company"] <- "The Carney Company"
data0$a002[data0$a002=="The Carneige Company Incorporated"] <- "The Carney Company"
data0$a002[data0$a002=="The Lone Star Cement Company (Kansas)"] <- "Lone Star Cement Company"
data0$a002[data0$a002=="Whitehall Cement Manufacturing Company"] <- "The Whitehall Cement Manufacturing Company"
data0$a002[data0$a001=="Oregon Portland Cement Company"] <- "Oregon Portland Cement Company"
data0$a002[data0$a001=="Southern Cement Company"] <- "Southern Cement Company"

### Data cleaning for price variable====
data0$gp_q_cementGeneral1[data0$gp_q_cementGeneral1=="714250 Bbls"] <- 714250
data0$gp_q_cementGeneral1[data0$gp_q_cementGeneral1=="960319 Bbls"] <- 960319
data0$gp_q_cementGeneral1[data0$gp_q_cementGeneral1=="652061 Bbls."] <- 652061



##### We allocate each plant to a market based on the county they belong to :
data0$ag005 <- data0$ag003
#### We rename counties that share a name based on the state they are located in ====
data0$ag005[data0$ag005=="Wayne"&data0$ag001=="Michigan"] <- "Wayne MI"
data0$ag005[data0$ag005=="Wayne"&data0$ag001=="West Virginia"] <- "Wayne WV"
data0$ag005[data0$ag005=="Washington"&data0$ag001=="Maryland"] <- "Washington MD"
data0$ag005[data0$ag005=="Washington"&data0$ag001=="Oklahoma"] <- "Washington OK"
data0$ag005[data0$ag005=="Warren"&data0$ag001=="New York"] <- "Warren NY"
data0$ag005[data0$ag005=="Warren"&data0$ag001=="New Jersey"] <- "Warren NJ"
data0$ag005[data0$ag005=="St. Louis"&data0$ag001=="Michigan"] <- "St. Louis MI"
data0$ag005[data0$ag005=="St. Louis"&data0$ag001=="Missouri"] <- "St. Louis MO"
data0$ag005[data0$ag005=="St. Clair"&data0$ag001=="Michigan"] <- "St. Clair MI"
data0$ag005[data0$ag005=="St. Clair"&data0$ag001=="Alabama"] <- "St. Clair AL"
data0$ag005[data0$ag005=="Polk"&data0$ag001=="Iowa"] <- "Polk IA"
data0$ag005[data0$ag005=="Polk"&data0$ag001=="Georgia"] <- "Polk GA"
data0$ag005[data0$ag005=="Montgomery"&data0$ag001=="Kansas"] <- "Montgomery KS"
data0$ag005[data0$ag005=="Montgomery"&data0$ag001=="Pennsylvania"] <- "Montgomery PA"
data0$ag005[data0$ag005=="Lawrence"&data0$ag001=="Pennsylvania"] <- "Lawrence PA"
data0$ag005[data0$ag005=="Lawrence"&data0$ag001=="Indiana"] <- "Lawrence IN"
data0$ag005[data0$ag005=="Lawrence"&data0$ag001=="Ohio"] <- "Lawrence OH"
data0$ag005[data0$ag005=="Lake"&data0$ag001=="Indiana"] <- "Lake IN"
data0$ag005[data0$ag005=="Lake"&data0$ag001=="Ohio"] <- "Lake OH"
data0$ag005[data0$ag005=="La Salle"] <- "Lasalle"
data0$ag005[data0$ag005=="Knox"&data0$ag001=="Tennessee"] <- "Knox TN"
data0$ag005[data0$ag005=="Knox"&data0$ag001=="Maine"] <- "Knox ME"
data0$ag005[data0$ag005=="Jefferson"&data0$ag001=="Alabama"] <- "Jefferson AL"
data0$ag005[data0$ag005=="Jefferson"&data0$ag001=="Kentucky"] <- "Jefferson KY"
data0$ag005[data0$ag005=="Jackson"&data0$ag001=="Missouri"] <- "Jackson MO"
data0$ag005[data0$ag005=="Jackson"&data0$ag001=="Arkansas"] <- "Jackson AR"
data0$ag005[data0$ag005=="Jackson"&data0$ag001=="Oregon"] <- "Jackson OR"
data0$ag005[data0$ag005=="Greene"&data0$ag001=="Ohio"] <- "Greene OH"
data0$ag005[data0$ag005=="Greene"&data0$ag001=="New York"] <- "Greene NY"
data0$ag005[data0$ag005=="Erie"&data0$ag001=="Ohio"] <- "Erie OH"
data0$ag005[data0$ag005=="Erie"&data0$ag001=="New York"] <- "Erie NY"
data0$ag005[data0$ag005=="Houston"] <- "Houston County"
#### We assign plants to a market based on closest city within 25 cities ====
data0$ag006 <- data0$ag005
data0$ag006[data0$ag006=="Albany"] <- "New York City"
data0$ag006[data0$ag006=="Allegheny"] <- "Pittsburgh"
data0$ag006[data0$ag006=="Allen"] <- "Kansas City"
data0$ag006[data0$ag006=="Alpena"] <- "Detroit"
data0$ag006[data0$ag006=="Augusta"] <- "Baltimore"
data0$ag006[data0$ag006=="Baker"] <- "Seattle"
data0$ag006[data0$ag006=="Bannock"] <- "Salt Lake City"
data0$ag006[data0$ag006=="Bay"] <- "Detroit"
data0$ag006[data0$ag006=="Berkeley"] <- "Baltimore"
data0$ag006[data0$ag006=="Berks"] <- "Philadelphia"
data0$ag006[data0$ag006=="Bexar"] <- "San Antonio"
data0$ag006[data0$ag006=="Blue Earth"] <- "Minneapolis"
data0$ag006[data0$ag006=="Bourbon"] <- "Kansas City"
data0$ag006[data0$ag006=="Box Elder"] <- "Salt Lake City"
data0$ag006[data0$ag006=="Branch"] <- "Detroit"
data0$ag006[data0$ag006=="Butler"] <- "Pittsburgh"
data0$ag006[data0$ag006=="Calaveras"] <- "San Francisco"
data0$ag006[data0$ag006=="Cape Girardeau"] <- "St. Louis"
data0$ag006[data0$ag006=="Carroll"] <- "Baltimore"
data0$ag006[data0$ag006=="Cass"] <- "Kansas City"
data0$ag006[data0$ag006=="Cerro Gordo"] <- "Minneapolis"
data0$ag006[data0$ag006=="Chester"] <- "Philadelphia"
data0$ag006[data0$ag006=="Clackamas"] <- "Seattle"
data0$ag006[data0$ag006=="Clark"] <- "Salt Lake City"
data0$ag006[data0$ag006=="Columbia"] <- "Boston"
data0$ag006[data0$ag006=="Columbiana"] <- "Cleveland"
data0$ag006[data0$ag006=="Contra Costa"] <- "San Francisco"
data0$ag006[data0$ag006=="Cuyahoga"] <- "Cleveland"
data0$ag006[data0$ag006=="Dallas"] <- "Dallas"
data0$ag006[data0$ag006=="Davidson"] <- "Birmingham"
data0$ag006[data0$ag006=="Eaton"] <- "Detroit"
data0$ag006[data0$ag006=="El Paso"] <- "Phoenix"
data0$ag006[data0$ag006=="Emmet"] <- "Minneapolis"
data0$ag006[data0$ag006=="Erie OH"] <- "Cleveland"
data0$ag006[data0$ag006=="Erie NY"] <- "Cleveland"
data0$ag006[data0$ag006=="Fergus"] <- "Salt Lake City"
data0$ag006[data0$ag006=="Franklin"] <- "Birmingham"
data0$ag006[data0$ag006=="Fremont"] <- "Denver"
data0$ag006[data0$ag006=="Gallatin"] <- "Salt Lake City"
data0$ag006[data0$ag006=="Genesee"] <- "Detroit"
data0$ag006[data0$ag006=="Greene NY"] <- "New York City"
data0$ag006[data0$ag006=="Greene OH"] <- "Cincinnati"
data0$ag006[data0$ag006=="Hamilton"] <- "Atlanta"
data0$ag006[data0$ag006=="Harris"] <- "Houston"
data0$ag006[data0$ag006=="Hillsborough"] <- "Atlanta"
data0$ag006[data0$ag006=="Houston County"] <- "Atlanta"
data0$ag006[data0$ag006=="Howard"] <- "Dallas"
data0$ag006[data0$ag006=="Jackson MO"] <- "Detroit"
data0$ag006[data0$ag006=="Jackson AR"] <- "St. Louis"
data0$ag006[data0$ag006=="Jackson OR"] <- "Seattle"
data0$ag006[data0$ag006=="Jefferson AL"] <- "Birmingham"
data0$ag006[data0$ag006=="Jefferson KY"] <- "Cincinnati"
data0$ag006[data0$ag006=="Kern"] <- "Los Angeles"
data0$ag006[data0$ag006=="King"] <- "Seattle"
data0$ag006[data0$ag006=="Knox TN"] <- "Atlanta"
data0$ag006[data0$ag006=="Knox ME"] <- "Boston"
data0$ag006[data0$ag006=="Lagrange"] <- "Chicago"
data0$ag006[data0$ag006=="Lake IN"] <- "Chicago"
data0$ag006[data0$ag006=="Lake OH"] <- "Cleveland"
data0$ag006[data0$ag006=="Larimer"] <- "Denver"
data0$ag006[data0$ag006=="Lasalle"] <- "Chicago"
data0$ag006[data0$ag006=="Lawrence PA"] <- "Pittsburgh"
data0$ag006[data0$ag006=="Lawrence IN"] <- "Cincinnati"
data0$ag006[data0$ag006=="Lawrence OH"] <- "Cleveland"
data0$ag006[data0$ag006=="Lee"] <- "Chicago"
data0$ag006[data0$ag006=="Lehigh"] <- "Philadelphia"
data0$ag006[data0$ag006=="Lenawee"] <- "Detroit"
data0$ag006[data0$ag006=="Los Angeles"] <- "Los Angeles"
data0$ag006[data0$ag006=="Lucas"] <- "Detroit"
data0$ag006[data0$ag006=="Manitowoc"] <- "Chicago"
data0$ag006[data0$ag006=="Marengo"] <- "Birmingham"
data0$ag006[data0$ag006=="Marion"] <- "Atlanta"
data0$ag006[data0$ag006=="McLennan"] <- "Dallas"
data0$ag006[data0$ag006=="Merced"] <- "San Francisco"
data0$ag006[data0$ag006=="Montgomery KS"] <- "Kansas City"
data0$ag006[data0$ag006=="Montgomery PA"] <- "Baltimore"
data0$ag006[data0$ag006=="Morgan"] <- "Salt Lake City"
data0$ag006[data0$ag006=="Mower"] <- "Minneapolis"
data0$ag006[data0$ag006=="Muskingum"] <- "Pittsburgh"
data0$ag006[data0$ag006=="Neosho"] <- "Kansas City"
data0$ag006[data0$ag006=="Newaygo"] <- "Detroit"
data0$ag006[data0$ag006=="Norfolk"] <- "Baltimore"
data0$ag006[data0$ag006=="Northampton"] <- "Philadelphia"
data0$ag006[data0$ag006=="Nuckolls"] <- "Kansas City"
data0$ag006[data0$ag006=="Onondaga"] <- "New York City"
data0$ag006[data0$ag006=="Orleans"] <- "New Orleans"
data0$ag006[data0$ag006=="Pend Oreille"] <- "Seattle"
data0$ag006[data0$ag006=="Pocahontas"] <- "Minneapolis"
data0$ag006[data0$ag006=="Polk GA"] <- "Atlanta"
data0$ag006[data0$ag006=="Polk IA"] <- "Kansas City"
data0$ag006[data0$ag006=="Pontotoc"] <- "Oklahoma City"
data0$ag006[data0$ag006=="Preston"] <- "Pittsburgh"
data0$ag006[data0$ag006=="Putnam"] <- "Cincinnati"
data0$ag006[data0$ag006=="Ralls"] <- "St. Louis"
data0$ag006[data0$ag006=="Riverside"] <- "Los Angeles"
data0$ag006[data0$ag006=="Salt Lake"] <- "Salt Lake City"
data0$ag006[data0$ag006=="San Benito"] <- "San Francisco"
data0$ag006[data0$ag006=="San Bernardino"] <- "Los Angeles"
data0$ag006[data0$ag006=="San Mateo"] <- "San Francisco"
data0$ag006[data0$ag006=="Santa Cruz"] <- "San Francisco"
data0$ag006[data0$ag006=="Schoharie"] <- "New York City"
data0$ag006[data0$ag006=="Scott"] <- "Chicago"
data0$ag006[data0$ag006=="Skagit"] <- "Seattle"
data0$ag006[data0$ag006=="Somerset"] <- "Pittsburgh"
data0$ag006[data0$ag006=="Spokane"] <- "Seattle"
data0$ag006[data0$ag006=="St. Clair AL"] <- "Birmingham"
data0$ag006[data0$ag006=="St. Clair MI"] <- "Detroit"
data0$ag006[data0$ag006=="St. Louis MO"] <- "St. Louis"
data0$ag006[data0$ag006=="St. Louis MN"] <- "Minneapolis"
data0$ag006[data0$ag006=="Stark"] <- "Cleveland"
data0$ag006[data0$ag006=="Sullivan"] <- "Cincinnati"
data0$ag006[data0$ag006=="Tarrant"] <- "Dallas"
data0$ag006[data0$ag006=="Tompkins"] <- "New York City"
data0$ag006[data0$ag006=="Ulster"] <- "New York City"
data0$ag006[data0$ag006=="Warren NJ"] <- "New York City"
data0$ag006[data0$ag006=="Warren NY"] <- "Boston"
data0$ag006[data0$ag006=="Washington OK"] <- "Oklahoma City"
data0$ag006[data0$ag006=="Washington MD"] <- "Baltimore"
data0$ag006[data0$ag006=="Washtenaw"] <- "Detroit"
data0$ag006[data0$ag006=="Wayne WV"] <- "Cincinnati"
data0$ag006[data0$ag006=="Wayne MI"] <- "Detroit"
data0$ag006[data0$ag006=="Whatcom"] <- "Seattle"
data0$ag006[data0$ag006=="Wilson"] <- "Kansas City"
data0$ag006[data0$ag006=="Wyandotte"] <- "Kansas City"
data0$ag006[data0$ag006=="York"] <- "Baltimore"








##### We introduce functions that measure multimarket contact and that create a price variable ====

# A function that selects the relevant variables for the analysis
select.variables <- function(data) {
  data <- data %>% 
    select(year,a001, a002, ag001, ag002,ag003, ag006, e005s, e005t, g000v, g00VAa, gp_v_pc, gp_q_pc, gp_q_cementGeneral1, gp_v_cementGeneral1)
  data
}

# A function that makes them numeric
variables.numeric <- function(data) {
  data$g000v <- as.numeric(data$g000v)
  data$g00VAa <- as.numeric(data$g00VAa)
  data$e005s <- as.numeric(data$e005s)
  data$e005t <- as.numeric(data$e005t)
  data$gp_q_cementGeneral1 <- as.numeric(data$gp_q_cementGeneral1)
  data$gp_v_cementGeneral1 <- as.numeric(data$gp_v_cementGeneral1)
  data$year <- as.numeric(data$year)
  data$gp_v_pc <- as.numeric(data$gp_v_pc)
  data$gp_q_pc <- as.numeric(data$gp_q_pc)
  data
}

# A function that creates a pair for each market that each parent firm is present in
company.markets <- function(data) {
  data %>%
    select(a002, ag006) %>%
    distinct()
}

#A function that creates pairs between all parent firms
company.pairs <- function(data) {
  expand.grid(a002 = unique(data$a002), company2 = unique(data$a002)) %>%
    filter(a002 != company2)  
}

#A function that counts the markets in common for each pair of parent firms in the dataset
count.commonmarkets <- function(a002, company2, data) {
  sum(data$ag006[data$a002 == a002] %in% data$ag006[data$a002 == company2])
}

#A function that creates a dataframe with the number of common markets for each company pair
result.common <- function(company_pairs, common_markets){
  result <- data.frame(company_pairs, common_markets = unlist(common_markets))
  result
}

#A function that creates a list of all markets
create.marketlist <- function (data) {unique(data$ag006)}

#A function that creates a list of all pairs of parent firms present in each market
create.pairslist <- function(data, markets){
  pairs_list <- list()
  for (i in markets) {
    filtered <- data %>% 
      filter(ag006 == i)
    companies <- unique(filtered$a002)
    if (length(companies) >= 2) {
      pairs_i <- combinations(n = length(companies), r = 2, repeats.allowed = FALSE, v = companies)
      pairs_list[[i]] <- pairs_i
    } else {
      pairs_list[[i]] <- matrix(nrow = 0, ncol = 2)
    }
  }
  pairs_list
}

#A function that formalizes the lists of pairs as a data frames
dataframe.pairslist <- function (pairs_list, markets) {
  for (i in markets) {
    pairs_list[[i]] <- as.data.frame(pairs_list[[i]])
    colnames(pairs_list[[i]]) <- c("a002","company2")
  }
  for (i in markets) {
    pairs_list[[i]] <- as.data.frame(pairs_list[[i]]) %>%
      mutate(across(everything(), as.character))}
  pairs_list
}
#A function that creates, for each market, a dataframe containing the pairs of firms and the markets they have in common
merged.dataframe <- function(pairs_list, markets, result) {
  mergeddf <- list()
  for (i in markets) {
    mergeddf[[i]] <- pairs_list[[i]] %>% 
      inner_join(result, by = c("a002"="a002", "company2"="company2"))
  }
  mergeddf}

#A function that calculates the mean number of contacts for each market
mean.values <- function(markets, mergeddf){
  mean_values <- numeric(length(markets))
  for (i in seq_along(markets)) {
    market <- markets[i]
    mean_values[i] <- mean(mergeddf[[market]]$common_markets, na.rm = TRUE)
  }
  mean_values}

#A function that calculates the mean price of portland cement for each market  (this variable is chosen because it is the only one reported consistently across the 4 years in the dataset)
mean.pricepc <- function(data, markets) {
  mean_pricepc <- numeric(length(markets))
  data <- data %>% filter(!is.na(gp_q_pc)&!is.na(gp_v_pc))
  for (i in seq_along(markets)) {
    filtered_data <- data %>%
      filter(ag006 == markets[i], complete.cases(gp_v_pc), complete.cases(gp_q_pc)) %>% 
      mutate(mean_pricepc = gp_v_pc/gp_q_pc)
    mean_pricepc[i] <- mean(filtered_data$mean_pricepc, na.rm = TRUE)}
  mean_pricepc}

#A function that calculates the number of firms on the market
number.firms <- function(data, markets) {
  firms <- numeric(length(markets))
  for (i in seq_along(markets)) {
    filtered_data <- data %>%
      filter(ag006 == markets[i])
    firms[i] <- length(unique(filtered_data$a002))
  }
  firms
}

#A function that creates the panel dataframe with the variables we created

resultyear.df <- function(markets, mean_pricepc, mean_values, number_firms) {
  result_year <- data.frame(
    market = markets,
    mean_pricepc = mean_pricepc,
    mean_mmc = mean_values,
    number_firms = number_firms)
  result_year
}

#A function that runs all the functions for a specific dataframe
new.dataset <- function(data) {
  data <- select.variables (data)
  data <- variables.numeric(data)
  company_marketsdata <- company.markets(data)
  company_pairsdata <- company.pairs(data)
  common_marketsdata <- apply(company_pairsdata, 1, function(pair) {
    count.commonmarkets(pair[1], pair[2], company_marketsdata)
  })
  resultdata <- result.common(company_pairsdata, common_marketsdata)
  marketsdata <- create.marketlist(data)
  pairs_listdata <- create.pairslist(data,marketsdata)
  pairs_listdatad <- dataframe.pairslist(pairs_listdata, marketsdata)
  mergeddfdata <- merged.dataframe(pairs_listdatad, marketsdata, resultdata)
  mean_valuesdata <- mean.values(marketsdata, mergeddfdata)
  mean_pricepcdata <- mean.pricepc(data, marketsdata)
  number_firmsdata <- number.firms(data, marketsdata)
  result_yeardata <- resultyear.df(marketsdata, mean_pricepcdata, mean_valuesdata, number_firmsdata)
  result_yeardata}

##### We use these functions to create a dataframe for each year in the dataset and then combine them into the panel dataframe

d1931 <- data0 %>% 
  filter(year=="1931")
panel1931 <- new.dataset(d1931)

d1929 <- data0 %>% 
  filter(year=="1929")
panel1929 <- new.dataset(d1929)

d1933 <- data0 %>% 
  filter(year=="1933")
panel1933 <- new.dataset(d1933)

d1935 <- data0 %>% 
  filter(year=="1935")
panel1935 <- new.dataset(d1935)

panel1929 <- panel1929 %>% mutate(year = 1929)
panel1931 <- panel1931 %>% mutate(year = 1931)
panel1933 <- panel1933 %>% mutate(year = 1933)
panel1935 <- panel1935 %>% mutate(year = 1935)

panel_data <- bind_rows(panel1929, panel1931, panel1933, panel1935)

panel_datacontact <- panel_data %>% 
  filter(mean_mmc>0)

data_panel = pdata.frame(panel_datacontact, index=c("market","year"), row.names=TRUE)

#####We run panel regressions to answer the research question
#Simple panel regressions controlling for different types of fixed effects (individual, time, two waysa and with pooling) without controls
regplmpool = plm(mean_pricepc ~ mean_mmc,
                 data = data_panel,
                 model = "pooling")
summary(regplmpool)
export_summs(regplmpool, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (time fixed effects model)")
coeftest(regplmpool, vcov = vcovHC, type = "HC1")


regplmindiv = plm(mean_pricepc ~ mean_mmc,
                  data = data_panel,
                  model = "within",
                  effect="individual")
summary(regplmindiv)
export_summs(regplmindiv, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (within model)")
coeftest(regplmindiv, vcov = vcovHC, type = "HC1")

regplmtime = plm(mean_pricepc ~ mean_mmc,
                 data = data_panel,
                 model = "within",
                 effect="time")
summary(regplmtime)
export_summs(regplmtime, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (time fixed effects model)")
coeftest(regplmtime, vcov = vcovHC, type = "HC1")

regplmtwo = plm(mean_pricepc ~ mean_mmc,
                data = data_panel,
                model = "within",
                effect="twoways")
summary(regplmtwo)
export_summs(regplmtwo, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (two ways model)")
coeftest(regplmtwo, vcov = vcovHC, type = "HC1")

cov1 = vcovHC(regplmindiv, type = "HC1")
cov2 = vcovHC(regplmtime, type = "HC1")
cov3 = vcovHC(regplmtwo, type = "HC1")
cov4 = vcovHC(regplmpool, type = "HC1")
robust_se <- list(sqrt(diag(cov1)),
                  sqrt(diag(cov2)),
                  sqrt(diag(cov3)),
                  sqrt(diag(cov4)))

stargazer(regplmindiv, regplmtime, regplmtwo, regplmpool, title="Regression Results : panel regression",
          dep.var.labels="Price of portland cement",
          keep.stat=c("n", "rsq"),
          type = "text",
          covariate.labels = "Mean mmc",
          style = "aer",
          se = robust_se, column.labels=c("Individual", "Time", "Two ways", "Pooling"), model.numbers=FALSE)

#We run the same regressions controlling for the number of firms on each market with robust estimates
regplmpoolp = plm(mean_pricepc ~ mean_mmc + number_firms,
                  data = data_panel,
                  model = "pooling")
summary(regplmpoolp)
export_summs(regplmpoolp, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (time fixed effects model)")
coeftest(regplmpool, vcov = vcovHC, type = "HC1")

regplmindivp = plm(mean_pricepc ~ mean_mmc + number_firms,
                   data = data_panel,
                   model = "within",
                   effect="individual")
summary(regplmindivp)
export_summs(regplmindivp, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (within model)")
coeftest(regplmindivp, vcov = vcovHC, type = "HC1")

regplmtimep = plm(mean_pricepc ~ mean_mmc + number_firms,
                  data = data_panel,
                  model = "within",
                  effect="time")
summary(regplmtimep)
export_summs(regplmtimep, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (time fixed effects model)")
coeftest(regplmtimep, vcov = vcovHC, type = "HC1")

regplmtwop = plm(mean_pricepc ~ mean_mmc+number_firms,
                 data = data_panel,
                 model = "within",
                 effect="twoways")
summary(regplmtwop)
export_summs(regplmtwop, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), model.names = "Price of Portland cement (two ways model)")
coeftest(regplmtwop, vcov = vcovHC, type = "HC1")

cov1p = vcovHC(regplmindivp, type = "HC1")
cov2p = vcovHC(regplmtimep, type = "HC1")
cov3p = vcovHC(regplmtwop, type = "HC1")
cov4p = vcovHC(regplmpoolp, type = "HC1")
robust_sep <- list(sqrt(diag(cov1p)),
                   sqrt(diag(cov2p)),
                   sqrt(diag(cov3p)),
                   sqrt(diag(cov4p)))

stargazer(regplmindivp, regplmtimep, regplmtwop, regplmpoolp, title="Regression Results : panel regression with control for number of firms",
          dep.var.labels="Price of portland cement",
          keep.stat=c("n", "rsq"),
          type = "text",
          covariate.labels = c("Mean mmc","Number of firms"),
          style = "aer",
          se = robust_sep, column.labels=c("Individual", "Time", "Two ways", "Pooling"), model.numbers=FALSE)

#Graph representation
ggplot(data_panel, aes(x = mean_mmc, y = mean_pricepc)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Plot of mean multimarket contacts (MMC) and Price of Portland Cement on each market every year",
    x = "Mean MMC",
    y = "Mean Price"
  ) +
  theme_minimal()

data_panel <- data_panel %>%
 mutate(Pred_Time = predict(regplmtimep),
        Pred_TwoWays = predict(regplmtwop),
        Pred_Pooling = predict(regplmpoolp),
        Pred_Individual = predict(regplmindivp))

data_long <- data_panel %>%
  pivot_longer(
    cols = starts_with("Pred_"),
    names_to = "Model",
    values_to = "Prediction"
  )

ggplot(data_long, aes(x = mean_mmc, y = mean_pricepc)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = Prediction, color = Model, linetype = Model), linewidth = 1) +
  facet_wrap(~ Model) +
  labs(
    title = "Regression Lines Across Models",
    x = "Mean MMC",
    y = "Mean Price"
  ) +
  theme_minimal()
