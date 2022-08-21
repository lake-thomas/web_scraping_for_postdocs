# USE THIS SITE AS TUTORIAL:
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/


# Setup
# install.packages("rvest")
library(rvest)
library(tidyverse)
library(stringr)
library(flextable)

####################################################################
# ECOLOG Trial: pausing because it requires a password to enter forum
# and I don't know enough html for that
####################################################################

# # Specifying the url for desired website to be scraped
# url <- 'https://eservices.esa.org/Eservices/Contacts/Sign_In.aspx'
# pgsession <- session(url)               ## create session
# pgform    <- html_form(pgsession)[[1]]       ## pull form from session
# 
# 
# filled_form <- set_values(pgform,
#                           `ctl00$Header2$HeaderTop1$tbUsername` = "sbreitbart@wesleyan.edu", 
#                           `ctl00$Header2$HeaderTop1$tbPassword` = "Barcel0na!")
# 
# submit_form(pgsession,filled_form)
# 
# #Reading the HTML code from the website
# webpage <- read_html(url)
# 
# #Using CSS selectors to scrape the rankings section
# rank_data_html <- html_nodes(webpage,'td , .explanation')
# 
# #Converting the ranking data to text
# rank_data <- html_text(rank_data_html)
# 
# #Let's have a look at the rankings
# head(rank_data)

####################################################################
# Natural resources job board
####################################################################

#Specifying the url for desired website to be scraped
url <- 'https://wfscjobs.tamu.edu/?job_category=full-time-positions'
#url2 <- 'https://wfscjobs.tamu.edu/?job_category=post-doctoral-appointments'

# Reading the HTML code from the website
webpage <- read_html(url)
#webpage2 <- read_html(url2)

# Using CSS selectors to scrape the relevant section
#job_data_html <- html_nodes(webpage,
#                            '.job-title, .job-posted-date')

#postdoc_data_html <- html_nodes(webpage2,
#                                '.job-title, .job-posted-date')

# Job Descriptions, Locations, Salary, Etc -----

# get links CSS selectors
job_links = webpage %>% 
  rvest::html_nodes('[class="job-listing-link"]') %>%
  rvest::html_attr("href")

#Define R objects to hold our target variables

agencies_list <- list() 
location_list <- list() 
category_list <- list()
salary_list <- list()
apply_list <- list()
website_list <- list()
description_list <- list()
qualification_list <- list()



###
#This will become a function
###

# Loop through all website links & pull data

for (i in 1:length(job_links)){
  
  description_url <- read_html(job_links[i]) #Get url link to job description & other information
  
  #Holds all our job description dataset
  description_data_html <- html_nodes(description_url, '.job-posting-dd') #using CSS selectors to scrape relevant section
  
  # Agency
  agency_html <- html_text(description_data_html[1]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(agency_html)[2] <- "Agency_Name"
  
  # Location
  location_html <- html_text(description_data_html[2]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(location_html)[2] <- "Location_Name"
  
  # Category
  category_html <- html_text(description_data_html[3]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(category_html)[2] <- "Category"
  
  # Salary
  salary_html <- html_text(description_data_html[4]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(salary_html)[2] <- "Salary"
  
  # Apply Date
  applydate_html <- html_text(description_data_html[5]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(applydate_html)[2] <- "Apply_Date"
  
  # Website
  website_html <- html_text(description_data_html[6]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(website_html)[2] <- "Website"
  
  # Description
  description_html <- html_text(description_data_html[7]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(description_html)[2] <- "Description"
  
  # Qualifications
  qualification_html <- html_text(description_data_html[8]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(qualification_html)[2] <- "Qualifications"
  
  
  
  #Append each data source to list
  agencies_list[[i]] <- agency_html$Agency_Name #append the agency data from HTML to a list (length 250)
  
  location_list[[i]] <- location_html$Location_Name
  
  category_list[[i]] <- category_html$Category
  
  salary_list[[i]] <- salary_html$Salary
  
  apply_list[[i]] <- applydate_html$Apply_Date
  
  website_list[[i]] <- website_html$Website
  
  description_list[[i]] <- description_html$Description
  
  qualification_list[[i]] <- qualification_html$Qualifications
  
}
  
#combine all jobs data into one DataFrame

job_data <- do.call(rbind, Map(data.frame, A=agencies_list, B=location_list,
                               C=category_list, D=salary_list, E=apply_list, 
                               D=website_list, E=description_list, G=qualification_list))

dim(job_data) #250 rows, 8 columns
str(job_data)



job_data$E.1 <- sapply(job_data$E.1,
                                    function(x) { gsub("[\r\n\t]", "", x) })



# Merge into one df
ds_opps <- job_data %>%
  # select relevant posts
  dplyr::filter( 
    grepl('data|quant| R |model|distribution| GIS | satellite | sensing | invasive',
          E.1,
          ignore.case = TRUE)) %>%
  # create new columns listing if keyword has been detected
  mutate(keyword_Data = case_when(grepl("data", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_Quant = case_when(grepl("quant",
                                         ignore.case = TRUE,
                                         E.1) ~ "yes",
                                   TRUE ~ " ")) %>%
  mutate(keyword_R = case_when(grepl(" r ",
                                     ignore.case = TRUE,
                                     E.1) ~ "yes",
                               TRUE ~ " ")) %>%
  mutate(keyword_model = case_when(grepl("model",
                                         ignore.case = TRUE,
                                         E.1) ~ "yes",
                                   TRUE ~ " "))%>%
  mutate(keyword_dist = case_when(grepl("distribution",
                                         ignore.case = TRUE,
                                         E.1) ~ "yes",
                                   TRUE ~ " ")) %>%
  mutate(keyword_gis = case_when(grepl("gis",
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_satellite = case_when(grepl("satellite",
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_sensing = case_when(grepl("sensing",
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_invasive = case_when(grepl("invasive",
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
  arrange(A)



flextable(ds_opps) %>%
  autofit() %>%
  save_as_html(
    "Potential_jobs",
    path = "Jobs.html")











####################################################################
# Natural resources job board (Post Doctoral Appointments)
####################################################################

#Specifying the url for desired website to be scraped
#url <- 'https://wfscjobs.tamu.edu/?job_category=full-time-positions'
url2 <- 'https://wfscjobs.tamu.edu/?job_category=post-doctoral-appointments'

# Reading the HTML code from the website
webpage <- read_html(url2)
#webpage2 <- read_html(url2)

# Using CSS selectors to scrape the relevant section
#job_data_html <- html_nodes(webpage,
#                            '.job-title, .job-posted-date')

#postdoc_data_html <- html_nodes(webpage2,
#                                '.job-title, .job-posted-date')

# Job Descriptions, Locations, Salary, Etc -----

# get links CSS selectors
job_links = webpage %>% 
  rvest::html_nodes('[class="job-listing-link"]') %>%
  rvest::html_attr("href")

#Define R objects to hold our target variables

agencies_list <- list() 
location_list <- list() 
category_list <- list()
salary_list <- list()
apply_list <- list()
website_list <- list()
description_list <- list()
qualification_list <- list()



###
#This will become a function
###

# Loop through all website links & pull data

for (i in 1:length(job_links)){
  
  description_url <- read_html(job_links[i]) #Get url link to job description & other information
  
  #Holds all our job description dataset
  description_data_html <- html_nodes(description_url, '.job-posting-dd') #using CSS selectors to scrape relevant section
  
  # Agency
  agency_html <- html_text(description_data_html[1]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(agency_html)[2] <- "Agency_Name"
  
  # Location
  location_html <- html_text(description_data_html[2]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(location_html)[2] <- "Location_Name"
  
  # Category
  category_html <- html_text(description_data_html[3]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(category_html)[2] <- "Category"
  
  # Salary
  salary_html <- html_text(description_data_html[4]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(salary_html)[2] <- "Salary"
  
  # Apply Date
  applydate_html <- html_text(description_data_html[5]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(applydate_html)[2] <- "Apply_Date"
  
  # Website
  website_html <- html_text(description_data_html[6]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(website_html)[2] <- "Website"
  
  # Description
  description_html <- html_text(description_data_html[7]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(description_html)[2] <- "Description"
  
  # Qualifications
  qualification_html <- html_text(description_data_html[8]) %>%
    as.data.frame() %>%
    tibble::rowid_to_column("ID")
  
  colnames(qualification_html)[2] <- "Qualifications"
  
  
  
  #Append each data source to list
  agencies_list[[i]] <- agency_html$Agency_Name #append the agency data from HTML to a list (length 250)
  
  location_list[[i]] <- location_html$Location_Name
  
  category_list[[i]] <- category_html$Category
  
  salary_list[[i]] <- salary_html$Salary
  
  apply_list[[i]] <- applydate_html$Apply_Date
  
  website_list[[i]] <- website_html$Website
  
  description_list[[i]] <- description_html$Description
  
  qualification_list[[i]] <- qualification_html$Qualifications
  
}
  
#combine all jobs data into one DataFrame

job_data <- do.call(rbind, Map(data.frame, A=agencies_list, B=location_list,
                               C=category_list, D=salary_list, E=apply_list, 
                               D=website_list, E=description_list, G=qualification_list))

dim(job_data) #250 rows, 8 columns
str(job_data)


#(Try to) clean the special characters
job_data$E.1 <- sapply(job_data$E.1,
                                    function(x) { gsub("[\r\n\t]", "", x) })



# Merge into one df
ds_opps_postdocs <- job_data %>%
  # select relevant posts
  dplyr::filter( 
    grepl(' invasive |model|data|quant| R | python | sensing | ArcGIS |distrib|satellite|plant|ecology|machine',
          E.1,
          ignore.case = TRUE)) %>%
  # create new columns listing if keyword has been detected
  mutate(keyword_invasive = case_when(grepl("invasive", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_model = case_when(grepl("model", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_data = case_when(grepl("data", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_quant = case_when(grepl("quant", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_R = case_when(grepl("R", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_python = case_when(grepl("python", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_sensing = case_when(grepl("sensing", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_ArcGIS = case_when(grepl("ArcGIS", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_distribution = case_when(grepl("distribution", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_satellite = case_when(grepl("satellite", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_plant = case_when(grepl("plant", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
    # create new columns listing if keyword has been detected
  mutate(keyword_ecology = case_when(grepl("ecology", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " ")) %>%
      # create new columns listing if keyword has been detected
  mutate(keyword_machine = case_when(grepl("machine", 
                                        ignore.case = TRUE,
                                        E.1) ~ "yes",
                                  TRUE ~ " "))

flextable(ds_opps_postdocs) %>%
  autofit() %>%
  save_as_html(
    "Potential_jobs_postdocs",
    path = "Jobs_postdocs.html")


### EOF ###
