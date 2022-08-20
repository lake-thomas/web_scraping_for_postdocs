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
url2 <- 'https://wfscjobs.tamu.edu/?job_category=post-doctoral-appointments'

# Reading the HTML code from the website
webpage <- read_html(url)
webpage2 <- read_html(url2)

# Using CSS selectors to scrape the relevant section
job_data_html <- html_nodes(webpage,
                            '.job-title, .job-posted-date')

postdoc_data_html <- html_nodes(webpage2,
                                '.job-title, .job-posted-date')

# JOBS-----
# Converting the data to text
job_data <- html_text(job_data_html) %>%
  as.data.frame()

# Get posting dates and save as separate df
job_dates <- job_data %>%
  slice(which(row_number() %% 2 == 0)) %>%
  tibble::rowid_to_column("ID")

# remove posting dates from original df
job_data %<>%
  slice(which(row_number() %% 2 == 1))%>%
  tibble::rowid_to_column("ID")

# join job descriptions and posting dates
job_data <- right_join(job_data, job_dates,
                       by = "ID") %>%
  dplyr::rename(Position = 2,
                Date = 3) %>%
  dplyr::mutate(Level = "Job")


# POSTDOCS-----
# Converting the data to text
postdoc_data <- html_text(postdoc_data_html) %>%
  as.data.frame()

# Get posting dates and save as separate df
postdoc_dates <- postdoc_data %>%
  slice(which(row_number() %% 2 == 0)) %>%
  tibble::rowid_to_column("ID")

# remove posting dates from original df
postdoc_data %<>%
  slice(which(row_number() %% 2 == 1))%>%
  tibble::rowid_to_column("ID")

# join job descriptions and posting dates
postdoc_data <- right_join(postdoc_data, postdoc_dates,
                           by = "ID") %>%
  dplyr::rename(Position = 2,
                Date = 3) %>%
  dplyr::mutate(Level = "Postdoc")


# Merge into one df
ds_opps <- rbind(job_data,
                 postdoc_data) %>%
  
  # select relevant posts
  dplyr::filter( 
    grepl('data|quant| R |model|distribution| GIS | satellite | sensing | invasive',
          Position,
          ignore.case = TRUE)) %>%
  
  # create new columns listing if keyword has been detected
  mutate(keyword_Data = case_when(grepl("data", 
                                        ignore.case = TRUE,
                                        Position) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_Quant = case_when(grepl("quant",
                                         ignore.case = TRUE,
                                         Position) ~ "yes",
                                   TRUE ~ " ")) %>%
  mutate(keyword_R = case_when(grepl(" r ",
                                     ignore.case = TRUE,
                                     Position) ~ "yes",
                               TRUE ~ " ")) %>%
  mutate(keyword_model = case_when(grepl("model",
                                         ignore.case = TRUE,
                                         Position) ~ "yes",
                                   TRUE ~ " "))%>%
  mutate(keyword_dist = case_when(grepl("distribution",
                                         ignore.case = TRUE,
                                         Position) ~ "yes",
                                   TRUE ~ " ")) %>%
  mutate(keyword_gis = case_when(grepl("gis",
                                        ignore.case = TRUE,
                                        Position) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_satellite = case_when(grepl("satellite",
                                        ignore.case = TRUE,
                                        Position) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_sensing = case_when(grepl("sensing",
                                        ignore.case = TRUE,
                                        Position) ~ "yes",
                                  TRUE ~ " ")) %>%
  mutate(keyword_invasive = case_when(grepl("invasive",
                                        ignore.case = TRUE,
                                        Position) ~ "yes",
                                  TRUE ~ " ")) %>%
  arrange(ID)



flextable(ds_opps) %>%
  autofit() %>%
  save_as_html(
    "Potential_jobs_postdocs",
    path = "Jobs_postdocs.html")
