# General ----------------------------------------------------

# Get the contents of a .RData cache for data shared between processes
cache <- function(){
  try({
    backupCache <<- readRDS("cache")
  })
  return(backupCache)
}

# Execute any number of script files as separate asynchronous processes
runAsync <- function(files){for (i in files){
  system(paste0(Sys.getenv("R_HOME"), "/bin/Rscript ", i), wait = FALSE)
}}

# Create a toolPanel Shiny UI element 
modulePanel <- function(title, value){
  tabPanel(
    title = title,
    value = value,
    eval(parse(text = value))$ui
  )
}

iframeConvention <- function(iframeURL){
  tags$iframe(
    width = 680, height = 800,
    src = iframeURL,
    frameborder = "no",
  )
}

# create a datatable with tooltips (defined in /global.R)
dataTablewithTooltips <- function(df, ...) {
  datatable(
    df,
    container = htmltools::withTags(table(thead(tr(
      do.call(tr, lapply(names(df), function(x) {
        th(x, title = ifelse(x %in% names(tooltips), tooltips[[x]], ""))
      }))
    )))),
    ...
  )
}

tenacious_range_read <- function(ss, sheet, col_types){
  
  while(TRUE){ try({ return(
    range_read(
      ss = ss,
      sheet = sheet,
      col_types = col_types
    ) %>% 
    as.data.frame %>%
    rename_with(toUpperCamelCase)
  )})}
  
}

# Apply a function cumulatively over a vector
cumulativeApply <- function(x, fun, self.modifying = FALSE){

  if(self.modifying){
    for (i in 1:length(x)){
      x[i] <- fun(x[1:i])
    }
    return(x)
  }
    
  sapply(
    1:length(x),
    function(i){fun(x[1:i])}
  )
  
}

toUpperCamelCase <- function(x){sapply(x,
  function(y){
    if(!grepl(" ", y, fixed = TRUE)){return(y)}
    gsub("[^[:alnum:]._]","", str_to_title(y))
  }
)}

toLowerCamelCase <- function(x){sapply(x,
  function(y){
    if(!grepl(" ", y, fixed = TRUE)){return(y)}
    y <- gsub("[^[:alnum:]._]","", str_to_title(y))
    substr(y, 1, 1) <- tolower(substr(y, 1, 1))
    return(y)
  }
)}

toTitle <- function(x){sapply(x,
  function(y){
    tools::toTitleCase(tolower(gsub("([A-Z]+)", " \\1", y)))
  }                                    
)}

getCrossInstance <- function(cross){sapply(cross, function(x){
  as.numeric(strsplit(x, ".", fixed = TRUE)[[1]][2])
})}

removeGroup <- function(cross){sapply(cross, function(x){
  strsplit(x, "_", fixed = TRUE)[[1]][1]
})}

removeSubgroup <- function(group){sapply(group, function(x){
  strsplit(x, "(", fixed = TRUE)[[1]][1]
})}

getCross <- function(crossInstance){sapply(crossInstance, function(x){
  strsplit(x, ".", fixed = TRUE)[[1]][1]
})}

getMother <- function(cross){sapply(cross, function(x){
  as.numeric(strsplit(x, "x", fixed = TRUE)[[1]][1])
})}

getFather <- function(cross){sapply(cross, function(x){
  as.numeric(strsplit(x, "x", fixed = TRUE)[[1]][2])
})}

removeCrossSubsets <- function(crossInstance){sapply(crossInstance, function(x){
  strsplit(x, "(", fixed = TRUE)[[1]][1]
})}

statMode <- function(x) {
   uniqx <- unique(na.omit(x))
   uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Background: Read Sheets ----------------------------------------------------

splitDenseRows <- function(df, denseColumnName){
  if(nrow(df) ==  0) {return(df)}
  newdf <- do.call("rbind",
      apply(df, 1, function(x){
        if(!grepl("\n", x[denseColumnName])) {return(x)}
        instances <- strsplit(x[denseColumnName], "\n", fixed = TRUE)[[1]]
        instancesDF <- data.frame(sapply(x, function(y){rep(y,length(instances))}))
        instancesDF[,denseColumnName] <- instances
        return(instancesDF)
      })
  )
  names(newdf) <- gsub(".", " ", names(newdf), fixed = TRUE)
  return(newdf)
}

# Module: Summaries -------------------------------------------------------------

countAggregate <- function(data, x, fill){
  
  data <- na.omit(data[,c(x, fill)])
  aggregated <- data.frame(unique(data[,x]))
  names(aggregated) <- x
  for(i in 1:nrow(aggregated)){
    for(j in unique(data[,fill])){
      aggregated[i,as.character(j)] <- sum(data[which(data[,x] == aggregated[i,x]),fill] == j, na.rm = TRUE)
    }
  }
  aggregated <- aggregated[order(apply(aggregated[-1], 1, sum)),]
  aggregated <- list("df" = aggregated)
  aggregated$bars <- t(aggregated$df[,-1])
  aggregated$max <- max(apply(aggregated$bars, 2, sum))
  return(aggregated)
}

to.VPD <- function(T,RH){
  abs((RH / 100 * 0.6108 * exp(17.27 * T / (T + 237.3))) - (0.6108 * exp(17.27 * T / (T + 237.3))))
}

# Module: Nursery Planner -------------------------------------------------------------

crosses <- function(){
  
  cache <- cache()
  cleanedSeed <- cache$readOnly$cleanedSeed
  
  lastCapsuleCollectionDate <- (
      cache$master$consolidated$CapsuleCollectionTimestamp %>%
      na.omit %>%
      max %>% 
      as.Date
    )
  
  aggregated <- cache$master$consolidated %>%
    group_by(Parents) %>%
    mutate(
      across(where(is.numeric), function(x){mean(x, na.rm = TRUE)}),
      IsFortyPlusDaysOld = (Date <= lastCapsuleCollectionDate - 40),
      ThisWeek = sum(Date >= Sys.Date() - 7, na.rm = TRUE),
      Old = sum(IsFortyPlusDaysOld, na.rm = TRUE),
      Total = n(),
      Dedicated = sum(DedicationStatus == "Dedicated", na.rm = TRUE),
      Compatibilities = sum(DedicationStatus == "Compatibility", na.rm = TRUE),
      CapsuleCollections = sum((!is.na(CapsuleCollectionTimestamp))),
      SuccessRate = ifelse(Old > 0, round(CapsuleCollections / Old, 2), NA)
    ) %>%
    slice(1) %>%
    full_join(cleanedSeed, by = c("Parents" = "Cross")) %>%
    mutate(
      across(SeedPerCapsule:last_col(), function(x){round(weighted.mean(x, CapsuleCollections, na.rm = TRUE), 2)}),
      across(where(is.integer), function(x){round(sum(x, na.rm = TRUE))}),
      SuccessRate = ifelse(CapsuleCollections <= Old , round(CapsuleCollections / Old, 2), NA)
    ) %>% 
    slice(1) %>% 
    ungroup() %>%
    arrange(desc(Total)) %>%
    select(Parents, Total:last_col()) %>%
    filter(!is.na(Parents)) %>%
    merge(., cache$master$crosses, by = "Parents", all.x = TRUE, sort = FALSE) %>%
    replace_na(list(SeedPerCapsule = 1.3)) %>%
    rowwise() %>%
    mutate(PlantCount = sum(cache$master$dedications$Parents == Parents, na.rm = T)) %>%
    mutate(PlantCount = ifelse(PlantCount, PlantCount, 1)) %>%
    group_by(FemaleCode) %>%
    mutate(
      SeasonFlowerCountEstimate = max(Total / PlantCount)
    ) %>%
    ungroup() %>%
    mutate(
      SuccessRate = SuccessRate * (Compatibilities > 4),
      SeedPerPol = SeedPerCapsule * SuccessRate,
      SeedPerPlantPerSeason = SeasonFlowerCountEstimate * SeedPerPol
    ) %>%
    select(Parents, SeedPerPlantPerSeason)
  return(aggregated)
  
}


# Module: Print Labels -------------------------------------------------------------

# this function creates a desired number of rows to represent different instances of a cross
expandCrosses <- function(
  df,
  startingCrossNumber,
  numberOfCrosses = 19,
  humanReadable = "HumanReadable",
  scanner = "Scanner",
  femaleCode = "FemaleCode",
  maleCode = "MaleCode",
  group = "Group"
){
  df[,humanReadable] <- rep.int(NA, nrow(df))
  df[,scanner] <- rep.int(NA, nrow(df))
  
  i <- 1
  while (i <= nrow(df)){
    df[i,humanReadable] <- paste(df[i,femaleCode], "x", df[i,maleCode], sep = "")
    df[i,scanner] <- paste(df[i,femaleCode], "x", df[i,maleCode], sep = "")
    df[i,scanner] <- NA
    for (j in startingCrossNumber:(startingCrossNumber+numberOfCrosses-1)){
      df <- rbind(df[1:i-1,], df[i,], df[i:nrow(df),])
      i <- i + 1
      df[i,humanReadable] <- paste(df[i,femaleCode], "x", df[i,maleCode], ".", j, sep = "")
      df[i,scanner] <- paste(df[i,femaleCode], "x", df[i,maleCode], ".", j, sep = "")
    }
    i <- i + 1
  }
  return(df)
}

# this function adds empty columns so no page contains more than one mother
emptyColumns <- function(
  df,
  femaleCode = "FemaleCode",
  scanner = "Scanner",
  stickersPerPage = 80
){
  
  currentMother <- df[1,femaleCode]
  
  i <- 1
  while (i <= nrow(df)){
    if(df[i,femaleCode] != currentMother){
      while ((i-1) %% 80 != 0){
        df <- rbind(df[1:i-1,], rep.int(NA, ncol(df)), df[i:nrow(df),])
        df[i,scanner] <- NA
        i <- i + 1
      }
      currentMother <- df[i,femaleCode]
    }
    i <- i + 1
  }
  return(df)
}

# mailMerge <- function(file, df){
#   
#   txt = "O"
#   inches <- list()
#   inches$x <- 0
#   inches$y <- 0
#   
#   pdf(file = "test.pdf", width = 8.5, height = 11)
#   frame()
#   
#   text(0.1, 1, txt)
#   text(0.1, 1, txt)
#   text(0.1, 1, txt)
#   text(0.1, 1, txt)
#   dev.off()
#   
# }

# Module: BrAPI -------------------------------------------------------------

sweetpotatobase <- brapi_db()$sweetpotatobase

relatedness <- function(pedigree1, pedigree2){

  count <- 0

  for(i in 1:nestDepth(pedigree1)){

    slice1 <- nestSlice(pedigree1, i)

    for(j in 1:nestDepth(pedigree2)){

      slice2 <- nestSlice(pedigree2, j)
      relations <- !is.na(match(slice1, slice2))
      # print(paste(slice1, slice2, relations))
      count <- count + (sum(relations) / length(relations)) / (i * j)

    }

  }

  if(count > 1)
    return(1)
  return(count)

}

pedigree <- local(function(con, germplasmName){

  recursiveGetParents <- function(con, germplasmName, thisPedigree = list()){
    try({parents <- getParents(con, germplasmName)})
    if(exists("parents")){
      for(i in 1:length(parents)){
        if(parents[i] != "NA"){
          thisPedigree[[parents[i]]] <- c(recursiveGetParents(con, parents[i]))
        }
        else{
          thisPedigree[i] <- list()
        }
      }
    }

    return(thisPedigree)
  }

  thisPedigree <- list()
  thisPedigree[[germplasmName]] <- recursiveGetParents(con, germplasmName)
  return(thisPedigree)

})


nestSlice <- function(nestedList, n = 1){
  map_depth(nestedList, n - 1, function(x){names(x)}) %>% unlist %>% unname
}

nestDepth <- function(nestedList){
  i <- 1
  while(TRUE){
    if(is.null(nestSlice(nestedList, i)))
      return(i-1)
    i <- i + 1
  }
}

getParents <- function(con, germplasmName){
  strsplit(brapi_get_germplasm(con, germplasmName = germplasmName)$pedigree, "/")[[1]]
}

nameToDbId <- function(con, germplasmName){
  brapi_get_germplasm(con, germplasmName = germplasmName)$germplasmDbId
}

DbIdToName <- function(con, germplasmDbId){
  brapi_get_germplasm(con, germplasmDbId = germplasmDbId)$germplasmName
}

# ------ local version

# pedigrees <- read.csv("corrected.csv", stringsAsFactors = F)

pedigree <- local(function(germplasmName){
  
  recursiveGetParents <- function(germplasmName, thisPedigree = list()){
    try({parents <- getParents(germplasmName)})
    if(exists("parents")){
      for(i in 1:length(parents)){
        if(parents[i] != "NA"){
          thisPedigree[[parents[i]]] <- c(recursiveGetParents(parents[i]))
        }
        else{
          thisPedigree[i] <- list()
        }
      }
    }
    
    return(thisPedigree)
  }
  
  thisPedigree <- list()
  thisPedigree[[germplasmName]] <- recursiveGetParents(germplasmName)
  return(thisPedigree)
  
})

localGetParents <- function(germplasmName){
  c(pedigrees)
}