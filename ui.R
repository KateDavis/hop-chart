library(shiny)

appVersion <- "1.3"

title <- function(...) {
  div(style="font-size:250%;float:left;padding-left:20px", ...)
}
version <- function(...) {
  div(style="float:left;position:relative;bottom:-8px;padding-left:20px", ...)
}
inputItem <- function(...) {
  div(style="float:left;padding-left:20px;padding-right:20px", ...)
}

hblock <- function(...) {
  div(style="float:left;display:block;width:100%;", ...)
}
hblock2 <- function(...) {
  div(style="float:left;", ...)
}
hblock3 <- function(...) {
  div(style="float:right;padding-right:20px;padding-top:10px;margin-top:-40px", ...)
}

shinyUI(bootstrapPage(
  
  wellPanel(
    title("Interactive Hop Chart"),
    version(sprintf("Version %s", appVersion))
  ),
  conditionalPanel(
    condition = "input.plots == \"Hops\"",
    inputItem(uiOutput("hopList"))),
  conditionalPanel(
    condition = "input.plots != \"Hops\"",
  inputItem(
    checkboxGroupInput("usage", "Usage:",
                       c("Aroma" = "Aroma", "Bittering" = "Bittering", "Dual-Purpose" = "Dual-Purpose")))),
                       #c("Aroma", "Bittering", "Dual-Purpose"))),
  
  conditionalPanel(
    condition = "input.plots == \"Acids\"",
    inputItem(
      checkboxGroupInput("acids", "Acid Type:",
                         c("Alpha.Acid" = "Alpha.Acid", "Beta.Acid" = "Beta.Acid", "Co.Humulone" = "Co.Humulone"),
                         c("Alpha.Acid", "Beta.Acid", "Co.Humulone"))),
    inputItem(
      sliderInput("alphaRange", "Alpha Acid Range:",
                  min = 0, max = 100, value = c(0,100))),
    inputItem(
      sliderInput("betaRange", "Beta Acid Range:",
                  min = 0, max = 100, value = c(0,100))),
    inputItem(
      sliderInput("cohumRange", "Cohumulone Range:",
                  min = 0, max = 100, value = c(0,100)))
    ),
  
  conditionalPanel(
    condition = "input.plots == \"Oils\"",
    inputItem(
      checkboxGroupInput("oils", "Oil Type:",
                         c("Myrcene.Oil" = "Myrcene.Oil", "Humulene.Oil" = "Humulene.Oil", "Caryophyllene.Oil" = "Caryophyllene.Oil", "Farnesene.Oil" = "Farnesene.Oil"),
                         c("Myrcene.Oil", "Humulene.Oil", "Caryophyllene.Oil","Farnesene.Oil"))),
    inputItem(
      sliderInput("myrceneRange", "Myrcene Oil Range:",
                  min = 0, max = 2.5, value = c(0,2.5))),
    inputItem(
      sliderInput("humuleneRange", "Humulene Oil Range:",
                  min = 0, max = 2.5, value = c(0,2.5))),
    inputItem(
      sliderInput("caryophylleneRange", "Caryophyllene Oil Range:",
                  min = 0, max = 2.5, value = c(0,2.5))),
    inputItem(
      sliderInput("farneseneRange", "Farnesene Oil Range:",
                  min = 0, max = 2.5, value = c(0,2.5)))
    ),
  
  hblock(
    hblock3(
      conditionalPanel(
        condition = "input.plots == \"Acids\"",
        selectInput("sortAcids", "Sort by:",
                    list("Name" = "Hops.Variety", 
                         "Alpha Acid" = "Alpha.Acid", 
                         "Beta Acid" = "Beta.Acid",
                         "Cohumulone" = "Co.Humulone"))
      ),
      conditionalPanel(
        condition = "input.plots == \"Oils\"",
        selectInput("sortOils", "Sort by:",
                    list("Name" = "Hops.Variety", 
                         "Myrcene Oil" = "Myrcene.Oil", 
                         "Humulene Oil" = "Humulene.Oil",
                         "Caryophyllene Oil" = "Caryophyllene.Oil",
                         "Farnesene Oil" = "Farnesene.Oil"))
      )),
      tabsetPanel(
        tabPanel("Acids (All Hops)",plotOutput("acidsPlot"),value="Acids"), 
        tabPanel("Oils (All Hops)", plotOutput("oilsPlot"),value="Oils"),
        tabPanel("Acids & Oils (Single Hop)", plotOutput("hopPlot"),value="Hops"),
        id="plots"
      ))
  
))