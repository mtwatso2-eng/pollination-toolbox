# Import packages
require(shiny); require(shinyBS); require(shinyWidgets)
require(magrittr); require(tidyverse); require(lubridate)
require(googlesheets4)
require(DT); require(plotly); require(rhandsontable);
require(brapir)

# Import functions defined in the app
source("utils.R")

if("administratorData" %in% list.files()){
  # Import user data
  administratorData <<- readRDS("administratorData")
  # Set timezone to user time zone
  Sys.setenv(TZ = administratorData$tz)
  # Get new Google Sheets API authentication token
  options(gargle_oauth_cache = ".secrets")
  gargle::gargle_oauth_cache()
  googlesheets4::gs4_auth()
  list.files(".secrets/")
  gs4_auth(
    cache = ".secrets",
    email = administratorData$email
  )
}

# Change a default option that can complicate data frame creation
options(stringsAsFactors = FALSE)

# Tooltips for table column names - these tooltips are not table-specific 
# (ie they will show up in any table containing one of the below names).
# Tooltips are in the format "Column Name" = "Tooltip"
tooltips <- list(
  "Unknown" = "This column shows up when pollinations can't be matched with a grouping factor. This often happens if someone makes a cross that isn't listed in the Pollination Master Sheet in Google Drive.",
  "SecondsPerPollination" = "The mean number of seconds per pollination, excluding outlier values like long breaks.",
  "PercentBetterThanAverage" = "How much faster (or slower) a pollination rate is than the average rate for the same crosses.",
  "PercentBetterThanPersonalAverage" = "How much faster (or slower) a person's pollination rate is than that person's average rate for the same crosses.",
  "CapsuleCollections" = "The total number of capsule collections, including compatibility and dedicated crosses.",
  "SuccessRate" = "The number of total capsule collections divided by the number of total pollinations greater than 40 days old. If shown in aggregate (e.g. totals), the weighted average of cross success rates.",
  "SeedPerCapsule" = "The number of cleaned seed divided by the number of collected capsules at the most recent date that seed were collected. If shown in aggregate (e.g. totals), the weighted average of cross seed per capsule numbers.",
  "CostPerSeed" = "An estimate of the cost of pollination (in dollars) per seed, taking into account the average time per pollination, success rate, seed per capsule, and assuming an hourly wage of $12/hr. This cost does not include related costs like paired cross plant maintenance, capsule collection time, and seed cleaning time. If shown in aggregate (e.g. totals), the weighted average of cross costs per seed.",
  "SuccessRateEstimate" = "The number of surviving compatibility capsules divided by the number of compatibility crosses. If shown in aggregate (e.g. totals), the weighted average of cross success rates.",
  "ToMatureCapsules" = "The number of capsules projected to mature if we stopped pollinating today. Calculated for each cross as the product of capsule-based success rate and the number of non-failed, uncollected pollinations done between 40 days ago and 40 days before the capsule projection date. If shown in aggregate, the sum of ToMatureCapsules for every cross within that group.",
  "ToDoCapsules" = "The number of capsules projected to mature from pollinations we haven't done yet. Calculated for each cross as the product of capsule-based success rate and average daily pollination count over the last week extended to 40 days before the projection date. If shown in aggregate, the sum of ToDoCapsules for every cross within that group.",
  "ProjectedCapsules" = "The total number of capsules you can expect to have at the projection date if everything goes exactly as it has over the last month, which is unlikely. The sum of TotalCapsuleCollections, ToMatureCapsules, and ToDoCapsules, if included in the table.",
  "Old" = "The number of compatibility crosses done 10 or more days before the most recent failed pollination scan.",
  "Failed" = 'The number of "old" compatibity crosses for which a failed pollination event has been recorded.',
  "Surviving" = 'The number of compatibility crosses presumed to have succeeded because they haven\'t failed after 10 days. The "Old" column minus the "Failed" column.',
  "Cross" = 'Given as "female x male", using greenhouse codes.',
  "Crosses" = 'Given as "female x male . cross instance", using greenhouse codes. Cross instance is absent for dedicated crosses.',
  "id" = 'Not cross instance! Mostly just for R as a way of distinguishing between dedicated crosses; it tells you how many times the value in the "Crosses" column has appeared.',
  "Timestamp" = "The time a pollination was recorded in Google Sheets. The timezone's UTC.",
  "TimeTakenSecs" = "The estimated amount of time one pollination took, in seconds. The time difference between one Google Form submission and the last submission for the same person, divided by the number of crosses within the submission. Outlier values like long breaks are excluded.",
  "FailedPollinationTimestamp" = "The time a failed pollination was recorded in Google Sheets, matched to the same cross instance. The timezone's UTC.",
  "CapsuleCollectionTimestamp" = "The time a capsule pollination was recorded in Google Sheets, matched to the same cross instance (or arbitrarily, in the case of dedicated crosses, which are only analyzed in aggregate). The timezone's UTC.",
  "CrossInstance" = 'The unique code code for an individual compatibility cross, taken from the part after the period in the "Crosses" column.',
  "DedicationStatus" = "Compatibility or dedicated.",
  "Date" = "The date a pollination was recorded in Google Sheets. The timezone's UTC.",
  "Time" = "The time of day a pollination was recorded, rounded to the nearest 5 minutes. The timezone's EST.",
  "Program" = "The program a cross falls under, or unknown if the cross isn't listed in the Pollination Master Sheet in Google Drive.",
  "Group" = "The group a cross falls under, or unknown if the cross isn't listed in the Pollination Master Sheet in Google Drive.",
  "FemaleCode" = "The greenhouse code for a cross' female parent.",
  "FemaleParent" = "The clone name for a cross' female parent. Unknown if a greenhouse code isn't matched with a clone name in the Pollination Master Sheet in Google Drive.",
  "MaleCode" = "The greenhouse code for a crosses' male parent. Unknown if a greenhouse code isn't matched with a clone name in the Pollination Master Sheet in Google Drive.",
  "MaleParent" = "The clone name for a cross' male parent. Unknown if a greenhouse code isn't matched with a clone name in the Pollination Master Sheet in Google Drive.",
  "Parents" = "The clone names for a cross' parents. Unknown if a greenhouse code isn't matched with a clone name in the Pollination Master Sheet in Google Drive."
)