if("administratorData" %in% list.files()){
  source("global.R")
  sapply(list.files(path = "modules", recursive = TRUE, pattern = "^.*\\.R$", full.names = TRUE), source)
  runAsync(list.files(path = "background", full.names = TRUE))
}

# UI for Shiny app
ui <- navbarPage(id = "tabs", collapsible = TRUE, title = "Pollination Toolbox",
  navbarMenu("Summaries",
    modulePanel("Pollination Counts", value = "pollinationCounts"),
    modulePanel("Season Summary", value = "seasonSummary"),
    modulePanel("Dedication Helper", value = "dedicationHelper"),
    modulePanel("Season Success Rate", value = "seasonSuccessRate"),
    modulePanel("Rates by Instances", value = "ratesByInstances")
  ),
  navbarMenu("Google Forms",
    tabPanel("Pollinations", iframeConvention(administratorData$urls$pollinationsForm)),
    tabPanel("Failed Pollinations", iframeConvention(administratorData$urls$failedPollinationsForm)),
    tabPanel("Capsule Collections", iframeConvention(administratorData$urls$capsuleCollectionsForm)),
    tabPanel("Cleaned Seed", iframeConvention(administratorData$urls$cleanedSeedForm))
  ),
  modulePanel("Raw Data", value = "rawData"),
  modulePanel("Print Labels", value = "printLabels"),
  modulePanel("About", value = "about"), br()
)

# Server function for Shiny app
server <- function(input, output, session) {
  
  if(!"administratorData" %in% list.files()){
    showModal(modalDialog(footer = NULL,
      title = "It looks like you haven't set up the Pollination Toolbox yet.
      Configure the app with the information below and you'll be good to go!",
      fluidPage(
        textInput("administratorDataEmail", label = "Administrator email"),
        textInput("administratorDataMasterURL", label = "Master sheet URL"),
        textInput("administratorDataReadOnlyURL", label = "Read-only data URL"),
        textInput("administratorDataPollinationsFormURL", label = "Pollinations form URL"),
        textInput("administratorDataFailedPollinationsFormURL", label = "Failed pollinations form URL"),
        textInput("administratorDataCapsuleCollectionsFormURL", label = "Capsule collections form URL"),
        textInput("administratorDataCleanedSeedURL", label = "Cleaned seed form URL"),
        textInput("administratorDataTZ", label = "Timezone"),
        actionButton("submitadministratorData", label = "Submit")
      )
    ))
    observeEvent(input$submitadministratorData, {
        administratorData <- list(
          email = input$administratorDataEmail,
          urls = list(
            master = input$administratorDataMasterURL,
            readOnly = input$administratorDataReadOnlyURL,
            pollinationsForm = paste0(input$administratorDataPollinationsFormURL, "?embedded=true"),
            failedPollinationsForm = paste0(input$administratorDataFailedPollinationsFormURL, "?embedded=true"),
            capsuleCollectionsForm = paste0(input$administratorDataCapsuleCollectionsFormURL, "?embedded=true"),
            cleanedSeedForm = paste0(input$administratorDataCleanedSeedURL, "?embedded=true")
          ),
          tz = input$administratorDataTZ
        )
        # Get authentication for Google Sheets access
        options(gargle_oauth_cache = ".secrets")
        gargle::gargle_oauth_cache()
        googlesheets4::gs4_auth()
        list.files(".secrets/")
        gs4_auth(
          cache = ".secrets",
          email = administratorData$email
        )
        writeRDS(administratorData, administratorData)
        session$reload()
      }
    )
  }
  
  # Load server function for all modules
  sapply(list.files(path = "modules"), function(module){
    get(module)$server(input, output, session)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)