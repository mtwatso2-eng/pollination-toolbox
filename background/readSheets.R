source("global.R")
cache <- list()

while(TRUE){ try({
  
  cache$master$crosses <- tenacious_range_read(administratorData$urls$master, "Crosses", "c")
  cache$readOnly$pollinations <- tenacious_range_read(administratorData$urls$readOnly, "Pollinations", "Tcc")
  cache$readOnly$failedPollinations <- tenacious_range_read(administratorData$urls$readOnly, "Failed Pollinations", "Tc")
  cache$readOnly$capsuleCollections <- tenacious_range_read(administratorData$urls$readOnly, "Capsule Collections", "Tc")
  cache$readOnly$cleanedSeed <- tenacious_range_read(administratorData$urls$readOnly, "Cleaned Seed", "Tci")
  cache$readOnly$weather <- tenacious_range_read(administratorData$urls$readOnly, "Weather", "Tiiii")
  
  cache$readOnly$pollinations %<>% 
    mutate(
      Timestamp = as.POSIXct(Timestamp, optional = TRUE),
      Date = as.Date(Timestamp),
      TimeTakenSecs = as.character(Timestamp - dplyr::lag(., order_by = Pollinator)$Timestamp),
    ) %>%
    mutate(
      TimeTakenSecs = ifelse(dplyr::lag(., order_by = Pollinator)$Date == Date, TimeTakenSecs, NA)
    ) %>%
    splitDenseRows("Crosses") %>% 
    group_by(Timestamp, Pollinator) %>%
    mutate(
      Timestamp = as.POSIXct(Timestamp, optional = TRUE),
      Date = as.Date(Timestamp),
      Time = round_date(Timestamp, period(minutes = 5)),
      Crosses = removeGroup(Crosses),
      TimeTakenSecs = round(as.numeric(TimeTakenSecs) / n(), 2),
      TimeTakenSecs = ifelse(TimeTakenSecs > 0 & TimeTakenSecs < 1800, TimeTakenSecs, NA)
    ) %>%
    ungroup()
  
  cache$readOnly$failedPollinations %<>% 
    splitDenseRows("Crosses") %>%
    rename("FailedPollinationTimestamp" = 1) %>% 
    mutate(
      FailedPollinationTimestamp = as.POSIXct(FailedPollinationTimestamp, optional = TRUE),
      Crosses = removeGroup(Crosses)
    )
  
  cache$readOnly$capsuleCollections %<>% 
    splitDenseRows("Crosses") %>%
    rename("CapsuleCollectionTimestamp" = 1) %>%
    mutate(
      CapsuleCollectionTimestamp = as.POSIXct(CapsuleCollectionTimestamp, optional = TRUE),
      Crosses = removeGroup(Crosses)
    )
  
  cache$readOnly$weather %<>% 
    mutate(Time = Time %>% round_date(period(minutes = 5)) %>% force_tz("EST")) %>%
    filter(!is.na(Time))
  
  # Consolidate data ----------------------------------------------------
  
  # copy the sheets data so we can consolidate data while the cache stays mostly "raw data"
  pollinations <- cache$readOnly$pollinations
  failedPollinations <- cache$readOnly$failedPollinations %<>% 
    filter(sapply(Crosses, function(x){
      grepl(".", x, fixed = TRUE)
    }))
  capsuleCollections <- cache$readOnly$capsuleCollections
  cleanedSeed <- cache$readOnly$cleanedSeed
  weather <- cache$readOnly$weather
  
  # consolidate the Google Form data frames into one data frame
  consolidated <- pollinations
  
  consolidated <- merge(
    consolidated %>% group_by(Crosses) %>% mutate(id = row_number()) %>% ungroup(),
    failedPollinations %>% group_by(Crosses) %>% mutate(id = row_number()) %>% ungroup(),
    all.x = TRUE
  )
  
  consolidated <- merge(
    consolidated %>% group_by(Crosses) %>% mutate(id = row_number()) %>% ungroup(),
    capsuleCollections %>% group_by(Crosses) %>% mutate(id = row_number()) %>% ungroup(),
    all.x = TRUE
  )

  # insert any failed pollination that lacks a corresponding pollination
  for (i in which(!failedPollinations[,"Crosses"] %in% consolidated[,"Crosses"])){
    consolidated[nrow(consolidated) + 1,"Crosses"] <- failedPollinations[i,"Crosses"]
    consolidated[nrow(consolidated),"FailedPollinationTimestamp"] <- failedPollinations[i,"FailedPollinationTimestamp"]
  }
  
  # insert any capsule collection that lacks a corresponding pollination
  for (i in which(!capsuleCollections[,"Crosses"] %in% consolidated[,"Crosses"])){
    consolidated[nrow(consolidated) + 1,"Crosses"] <- capsuleCollections[i,"Crosses"]
    consolidated[nrow(consolidated),"CapsuleCollectionTimestamp"] <- capsuleCollections[i,"CapsuleCollectionTimestamp"]
  }
  
  consolidated %<>% 
    filter(!is.na(Crosses)) %>%
    mutate(
      Cross = getCross(Crosses),
      CrossInstance = getCrossInstance(Crosses),
      DedicationStatus = sapply(Crosses, function(x){
        ifelse(grepl(".", x, fixed = TRUE), "Compatibility", "Dedicated")
      })
    ) %>%
    merge(., cache$master$crosses[!duplicated(cache$master$crosses[,"Cross"]),], by = "Cross", all.x = TRUE) %>%
    left_join(weather, by = "Time") %>%
    mutate(across(c(Program, Group), ~ replace(.x, is.na(.x), "Unknown"))) %>%
    group_by(Crosses) %>% 
    filter(row_number() == 1 | DedicationStatus == "Dedicated") %>%
    ungroup()
  
  cache$master$consolidated <- consolidated
  
  capsulesAtTimestamp <- function(whichCross, CleanedSeedTimestamp){
    count <- cache$master$consolidated %>% 
      filter(Parents == whichCross & CapsuleCollectionTimestamp < CleanedSeedTimestamp) %>%
      summarize(n = n())
    return(count$n)
  }
  
  cache$readOnly$cleanedSeed %<>% 
    rename("CleanedSeedTimestamp" = 1) %>%
    mutate(
      CleanedSeedTimestamp = as.POSIXct(CleanedSeedTimestamp, optional = TRUE)
    ) %>%
    group_by(Cross) %>%
    mutate(NumberOfCleanedSeed = sum(NumberOfCleanedSeed, na.rm = TRUE)) %>%
    slice_max(order_by = CleanedSeedTimestamp, n = 1) %>%
    mutate(
      ToDateCapsules = capsulesAtTimestamp(Cross, CleanedSeedTimestamp),
      SeedPerCapsule = ifelse(ToDateCapsules > 0 & NumberOfCleanedSeed > 0, round(NumberOfCleanedSeed / ToDateCapsules, 2), NA),
    ) %>%
    rename(CleanedSeed = NumberOfCleanedSeed) %>%
    select(Cross, CleanedSeed, SeedPerCapsule)

  saveRDS(cache, "cache")
  
}) }
