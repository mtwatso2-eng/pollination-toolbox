# Pollination Toolbox

Pollination Toolbox is an R Shiny web app designed to increase the efficiency of making paired crosses in plant breeding. You can ask questions, report bugs and suggest features by emailing Mark at mtwatso2@ncsu.edu.

## Setup Instructions

Pollination Toolbox is an R Shiny web app that depends on Google Forms and Google Sheets to work, so you'll need an environment to run R Shiny in and somewhere to store a few Google Forms and Google Sheets. 

### Deploying Pollination Toolbox
If you haven't deployed a Shiny app before, you can find deployment options and instructions at [R Shiny's website ](https://shiny.rstudio.com/articles/deployment-web.html).

### Setting up Google Forms and Sheets for Pollination Toolbox
Pollination Toolbox expects 4 types of pollination data - pollinations, failed pollinations, capsule collections, and cleaned seed - and 2 other types of data, cross information and weather data. Currently, no documentation exists for setting these forms. Contact Mark at mtwatso2@ncsu with questions.

### App configuration within Pollination Toolbox
Once you've set up an environment for R Shiny along with the required Google Forms and Sheets, run Pollination Toolbox. The app will prompt you (only this first time) for Google authentication, URLs for the Google Sheets and Forms, and your timezone.