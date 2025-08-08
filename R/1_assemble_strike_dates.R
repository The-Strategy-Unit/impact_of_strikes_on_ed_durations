# README
# This script creates a data frame containing the various dates of 
# industrial action taken by NHS staff strikes since 2022.  
# The data frame indicates that types of staff that were on strike.  Where 
# staff are on strike for a partial day (e.g. from 7am) then the value 
# indicates the part of the part covered by the strike.  Otherwise the value
# is either 0 (no industrial action) or 1 (industrial action all day).

# Note this data frame does not contains details of cancelled or postponed 
# strikes.  These are listed elsewhere  and classed as unusual dates.

# Note this data frame does not contains details of small scale strikes
# (local, regional strikes or those affecting small staff groups)
# These are listed elsewhere  and classed as unusual dates.


# The main source of information used to construct these data frames are 
# https://en.wikipedia.org/wiki/2022%E2%80%93present_National_Health_Service_strikes
# https://www.bma.org.uk/our-campaigns/junior-doctor-campaigns/pay/junior-doctors-guide-to-industrial-action-in-england-2024/time-out-of-training-toot-during-junior-doctors-strikes-in-england
# https://www.personneltoday.com/hr/who-is-on-strike-and-when/

# These have been checked against the special IA sitreps.

# secondary sources for each strike are given in df creation below

# set up ----
# library("lubridate")
# library("testthat")
# library("ggplot2")
# library("janitor")
# library("stringr")
# library("readxl")
# library("tibble")
# library("dplyr")
# library("purrr")
# library("readr")
# library("tidyr")
# library("here")
# all called by deps.R


assemble_strike_dates <- function() {
  
  
  tribble(~measure, ~date, ~value,
          
          "nurses", "2022-12-15", 1, #https://www.rcn.org.uk/news-and-events/Press-Releases/first-strike-dates-announced-by-rcn-after-uk-government-declines-nhs-pay-negotiations
          "nurses", "2022-12-20", 1, #https://www.rcn.org.uk/news-and-events/Press-Releases/rcn-confirms-the-locations-of-december-strikes-across-the-uk
          "nurses", "2023-01-18", 1, #https://www.rcn.org.uk/news-and-events/Press-Releases/strike-dates-for-january-announced
          "nurses", "2023-01-19", 1, #https://www.rcn.org.uk/news-and-events/Press-Releases/strike-dates-for-january-announced
          "nurses", "2023-02-06", 12/24, #https://www.rcn.org.uk/news-and-events/Press-Releases/rcn-announces-further-strike-action-in-february-after-uk-government-fails-to-start-negotiations
          "nurses", "2023-02-07", 12/24, #https://www.rcn.org.uk/news-and-events/Press-Releases/rcn-announces-further-strike-action-in-february-after-uk-government-fails-to-start-negotiations
          "nurses", "2023-03-01", 18/24, #https://www.rcn.org.uk/news-and-events/Press-Releases/royal-college-of-nursing-steps-up-strike-action-by-removing-wide-ranging-derogations
          "nurses", "2023-03-02", 1,     #https://www.rcn.org.uk/news-and-events/Press-Releases/royal-college-of-nursing-steps-up-strike-action-by-removing-wide-ranging-derogations
          "nurses", "2023-03-03", 6/24,  #https://www.rcn.org.uk/news-and-events/Press-Releases/royal-college-of-nursing-steps-up-strike-action-by-removing-wide-ranging-derogations"
          "nurses", "2023-04-30", 4/24, #https://www.rcn.org.uk/magazines/Action/2023/Feb/Spring-nursing-strike-action-what-you-need-to-know#:~:text=The%20upcoming%2048%2Dhour%20strike,24%2Dhour%20services%20or%20not.
          "nurses", "2023-05-01", 1, #https://www.rcn.org.uk/magazines/Action/2023/Feb/Spring-nursing-strike-action-what-you-need-to-know#:~:text=The%20upcoming%2048%2Dhour%20strike,24%2Dhour%20services%20or%20not.
          
          "ambulance", "2022-12-21", 1, #https://www.unison.org.uk/news/press-release/2023/03/unison-to-suspend-strike-to-enter-nhs-pay-talks-with-government/
          "ambulance", "2023-01-11", 1, #https://www.unison.org.uk/news/press-release/2023/03/unison-to-suspend-strike-to-enter-nhs-pay-talks-with-government/
          "ambulance", "2023-01-23", 1, #https://www.unison.org.uk/news/press-release/2023/03/unison-to-suspend-strike-to-enter-nhs-pay-talks-with-government/
          "ambulance", "2023-02-10", 1, #https://www.unison.org.uk/news/press-release/2023/03/unison-to-suspend-strike-to-enter-nhs-pay-talks-with-government/
          "ambulance", "2023-02-17", 1, #https://www.england.nhs.uk/long-read/industrial-action-13-17-and-20-february-2023/
          "ambulance", "2023-02-20", 1, #https://www.gmb.org.uk/news/more-11000-ambulance-workers-strike
          
          "jr doctors", "2023-03-13", 17/24, #https://www.bma.org.uk/bma-media-centre/bma-announces-dates-for-72-hour-walk-out-by-junior-doctors-in-england-saying-health-secretary-has-left-them-with-no-choice
          "jr doctors", "2023-03-14", 1, #https://www.bma.org.uk/bma-media-centre/bma-announces-dates-for-72-hour-walk-out-by-junior-doctors-in-england-saying-health-secretary-has-left-them-with-no-choice
          "jr doctors", "2023-03-15", 1, #https://www.bma.org.uk/bma-media-centre/bma-announces-dates-for-72-hour-walk-out-by-junior-doctors-in-england-saying-health-secretary-has-left-them-with-no-choice
          "jr doctors", "2023-03-16", 7/24, #https://www.bma.org.uk/bma-media-centre/bma-announces-dates-for-72-hour-walk-out-by-junior-doctors-in-england-saying-health-secretary-has-left-them-with-no-choice
          "jr doctors", "2023-04-11", 17/24, #https://www.theguardian.com/uk-news/2023/mar/23/junior-doctors-england-strike-four-days-april
          "jr doctors", "2023-04-12", 1, #https://www.theguardian.com/uk-news/2023/mar/23/junior-doctors-england-strike-four-days-april
          "jr doctors", "2023-04-13", 1, #https://www.theguardian.com/uk-news/2023/mar/23/junior-doctors-england-strike-four-days-april
          "jr doctors", "2023-04-14", 1, #https://www.theguardian.com/uk-news/2023/mar/23/junior-doctors-england-strike-four-days-april
          "jr doctors", "2023-04-15", 7/24, #https://www.theguardian.com/uk-news/2023/mar/23/junior-doctors-england-strike-four-days-april
          "jr doctors", "2023-06-14", 17/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-announce-june-strike-action-after-government-fails-to-make-credible-pay-offer
          "jr doctors", "2023-06-15", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-announce-june-strike-action-after-government-fails-to-make-credible-pay-offer
          "jr doctors", "2023-06-16", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-announce-june-strike-action-after-government-fails-to-make-credible-pay-offer
          "jr doctors", "2023-06-17", 7/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-announce-june-strike-action-after-government-fails-to-make-credible-pay-offer
          "jr doctors", "2023-07-13", 17/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-urge-government-to-come-back-to-the-table-as-new-strike-begins-in-england
          "jr doctors", "2023-07-14", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-urge-government-to-come-back-to-the-table-as-new-strike-begins-in-england
          "jr doctors", "2023-07-15", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-urge-government-to-come-back-to-the-table-as-new-strike-begins-in-england
          "jr doctors", "2023-07-16", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-urge-government-to-come-back-to-the-table-as-new-strike-begins-in-england
          "jr doctors", "2023-07-17", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-urge-government-to-come-back-to-the-table-as-new-strike-begins-in-england
          "jr doctors", "2023-07-18", 7/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-urge-government-to-come-back-to-the-table-as-new-strike-begins-in-england
          "jr doctors", "2023-08-11", 17/24, #https://www.bma.org.uk/bma-media-centre/new-junior-doctors-implore-the-government-to-come-to-the-table-as-fresh-round-of-strikes-begin-in-england
          "jr doctors", "2023-08-12", 1, #https://www.bma.org.uk/bma-media-centre/new-junior-doctors-implore-the-government-to-come-to-the-table-as-fresh-round-of-strikes-begin-in-england
          "jr doctors", "2023-08-13", 1, #https://www.bma.org.uk/bma-media-centre/new-junior-doctors-implore-the-government-to-come-to-the-table-as-fresh-round-of-strikes-begin-in-england
          "jr doctors", "2023-08-14", 1, #https://www.bma.org.uk/bma-media-centre/new-junior-doctors-implore-the-government-to-come-to-the-table-as-fresh-round-of-strikes-begin-in-england
          "jr doctors", "2023-08-15", 7/24, #https://www.bma.org.uk/bma-media-centre/new-junior-doctors-implore-the-government-to-come-to-the-table-as-fresh-round-of-strikes-begin-in-england
          "jr doctors", "2023-09-20", 17/24, #https://www.bma.org.uk/news-and-opinion/consultants-and-junior-doctors-camaraderie-grows-during-joint-industrial-action
          "jr doctors", "2023-09-21", 1, #https://www.bma.org.uk/news-and-opinion/consultants-and-junior-doctors-camaraderie-grows-during-joint-industrial-action
          "jr doctors", "2023-09-22", 1, #https://www.bma.org.uk/news-and-opinion/consultants-and-junior-doctors-camaraderie-grows-during-joint-industrial-action
          "jr doctors", "2023-09-23", 7/24, #https://www.bma.org.uk/news-and-opinion/consultants-and-junior-doctors-camaraderie-grows-during-joint-industrial-action
          "jr doctors", "2023-10-02", 17/24, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action    
          "jr doctors", "2023-10-03", 1, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          "jr doctors", "2023-10-04", 1, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action      
          "jr doctors", "2023-10-05", 7/24, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          "jr doctors", "2023-12-20", 17/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-new-round-of-strikes-as-government-fails-to-improve-pay-offer
          "jr doctors", "2023-12-21", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-new-round-of-strikes-as-government-fails-to-improve-pay-offer
          "jr doctors", "2023-12-22", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-new-round-of-strikes-as-government-fails-to-improve-pay-offer
          "jr doctors", "2023-12-23", 7/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-new-round-of-strikes-as-government-fails-to-improve-pay-offer
          "jr doctors", "2024-01-03", 17/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-01-04", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-01-05", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-01-06", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-01-07", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-01-08", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-01-09", 7/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-begin-longest-single-strike-action-in-nhs-history#:~:text=The%20strike%20dates%20in%20England,care%20and%20a%20healthy%20population.
          "jr doctors", "2024-02-24", 17/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-to-begin-new-strike-as-government-fails-to-present-new-pay-offer
          "jr doctors", "2024-02-25", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-to-begin-new-strike-as-government-fails-to-present-new-pay-offer
          "jr doctors", "2024-02-26", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-to-begin-new-strike-as-government-fails-to-present-new-pay-offer
          "jr doctors", "2024-02-27", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-to-begin-new-strike-as-government-fails-to-present-new-pay-offer
          "jr doctors", "2024-02-28", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-in-england-to-begin-new-strike-as-government-fails-to-present-new-pay-offer
          "jr doctors", "2024-06-27", 17/24, #"ttps://www.bma.org.uk/bma-media-centre/junior-doctors-announce-new-strike-dates-in-england-ahead-of-general-election
          "jr doctors", "2024-06-28", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-announce-new-strike-dates-in-england-ahead-of-general-election
          "jr doctors", "2024-06-29", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-announce-new-strike-dates-in-england-ahead-of-general-election
          "jr doctors", "2024-06-30", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-announce-new-strike-dates-in-england-ahead-of-general-election
          "jr doctors", "2024-07-01", 1, #https://www.bma.org.uk/bma-media-centre/junior-doctors-announce-new-strike-dates-in-england-ahead-of-general-election
          "jr doctors", "2024-07-02", 7/24, #https://www.bma.org.uk/bma-media-centre/junior-doctors-announce-new-strike-dates-in-england-ahead-of-general-election
          
          "consultants", "2023-07-20", 17/24, #https://www.england.nhs.uk/long-read/industrial-action-junior-doctors-and-dental-trainees-13-18-july-2023-bma-consultants-20-22-july-2023/
          "consultants", "2023-07-21", 1, #https://www.england.nhs.uk/long-read/industrial-action-junior-doctors-and-dental-trainees-13-18-july-2023-bma-consultants-20-22-july-2023/
          "consultants", "2023-07-22", 7/24, #https://www.england.nhs.uk/long-read/industrial-action-junior-doctors-and-dental-trainees-13-18-july-2023-bma-consultants-20-22-july-2023/
          "consultants", "2023-08-24", 17/24, #https://www.bmj.com/content/382/bmj.p1955
          "consultants", "2023-08-25", 1, #https://www.bmj.com/content/382/bmj.p1955
          "consultants", "2023-08-26", 7/24, #https://www.bmj.com/content/382/bmj.p1955       
          "consultants", "2023-09-19", 17/24, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          "consultants", "2023-09-20", 7/24, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          "consultants", "2023-10-02", 17/24, #"ttps://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          "consultants", "2023-10-03", 1, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          "consultants", "2023-10-04", 7/24, #https://www.bma.org.uk/bma-media-centre/doctors-in-england-announce-four-days-of-joint-strikes-this-autumn-as-juniors-vote-for-six-more-months-of-industrial-action
          
    ) |> 
    mutate(date = as_date(date)) |> 
    mutate(measure = paste0(measure, " industrial action (hrs/24)"))
  
}
