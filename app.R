#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(DT)
library(fs)
library(purrr)
library(shinyWidgets)

creditProcessingRevenueItems <- c (
  "Credit Authorization Revenue", 
  "Credit Deposit Fee Revenue", 
  "Credit Depsoit % Fee Revenue"         
)

creditCurrencyRevenueItems <- c (
  "CCredit Currency Revenue"         
)

demographicsRow <- fluidRow(
  box(
    title = "Strategic Deal Demographics",
    solidHeader = TRUE,
    background = "yellow",
    width = 12,
    textInput("merchantName",
              label = "Mechant Legal Name",
              placeholder = 'Merchant Name'),
    textInput(
      "nYearsOfContractTerm",
      label = "Contract Term (years)",
      placeholder = 'No. Of Years'
    ),
    textInput(
      "noOfContractLocations",
      label = "Number of Contract Locations",
      placeholder = 'No. Of Locations'
    ),
    textInput(
      "noOfFranchiseLocations",
      label = "Number of Franchise Locations",
      placeholder = 'No. Of Locations'
    ),
    selectInput(
      "fundTransferType",
      label = "Funds Transfer Type",
      choices = c('ACH', 'Wire', 'Both')
    )
  )
)

standardInputs <- box(
  title = "Standard Input",
  solidHeader = FALSE,
  background = "green",
  width = 12,
  collapsible = TRUE,
  DT::dataTableOutput("standardInput")
)

volumeInputs <- box(
  title = "",
  background = "green",
  width = 12,
  collapsible = TRUE,
  DT::dataTableOutput("volumeInput")
)

pricingInputs <- box(
  title = "",
  solidHeader = FALSE,
  collapsible = TRUE,
  background = "green",
  width = 12,
  DT::dataTableOutput("pricingInput")
)

creditProcessingRevenueInputs <- box(
  title = "",
  solidHeader = FALSE,
  collapsible = TRUE,
  background = "green",
  width = 12,
  DT::dataTableOutput("creditProcessingRevenueInput")
)

creditCurrencyRevenueInputs <- box(
  title = "",
  solidHeader = FALSE,
  collapsible = TRUE,
  background = "green",
  width = 12,
  DT::dataTableOutput("creditCurrencyRevenueInput")
)

# debugPanel <- fluidRow(
#

instruments <- list(
  "Credit"    = c(
    "",
    "Visa" = "VS",
    "Mastercard" = "MC",
    "AMEX Settled (Opt Blue)" = "AMXSOB",
    "AMEX Conveyed" = "AMXCVYD",
    "Discover/JCB/Diners Settled" = "DSCSTLD",
    "Cross Currency" = "CC",
    "Discover Conveyed" = "DCSCVYD",
    "Purchasing Card Level III" = "PCIII",
    "Net Connect" = "NC"
  ),
  "PIN Debit" = c("", "w/Hosted Pay Page")
)

productCapabilities <- fluidRow(
  box(
    title = "Product and Capabilities",
    solidHeader = TRUE,
    background = "yellow",
    width = 12,
    fluidRow(
      column(
        width = 2,
        textInput(
          "noOfTiers",
          label = "Tiers #",
          value = "5",
          placeholder = 'No Of Tiers'
        )
      ),
      column(
        width = 6,
        selectInput(
          "tierMethod",
          label = "Methods",
          choices = c(
            "Number Of Transactions" = "noOfTransactions",
            "Dollar Value Of Transactions" = "dvOfTransactions"
          )
        )
      ),
      column(
        width = 4,
        textInput("valueOfTierMethods", label = "Amount", value = "200,000")
      )
    ),
    fluidRow(column(
      width = 4,
      selectInput(
        "capabilities",
        label = "Capabilities",
        choices =  instruments,
        multiple = TRUE
      )
    ))
  )
)

layoutXX <- fluidRow(column(width = 12,
                            fluidRow (
                              column(width = 4,
                                     demographicsRow,
                                     productCapabilities,
                                     fluidRow(
                                       column(width = 12, offset = 10, actionButton("save", "Save"))
                                     )),
                              column(
                                width = 8,
                                fluidRow(standardInputs),
                                fluidRow(volumeInputs),
                                fluidRow(pricingInputs),
                                fluidRow(creditProcessingRevenueInputs),
                                fluidRow(creditCurrencyRevenueInputs)
                              )
                            )))


mainBody <- dashboardBody(useShinyjs(),
                          layoutXX
                          #,fluidRow(htmlOutput("debugMe"))
)


allCapabilities <-
  c(
    "Auth to Capture Ratio",
    "Return Ratio",
    "Chargeback Ratio",
    "Retrieval Ratio",
    "Representment Ratio",
    "Voice Auth Ratio"
  )

creditCapabilities <- list(
  "all" = c(
    "Credit Authorization Fee",
    "Credit Deposit Fees",
    "Credit Deposit Fees (bps)",
    "Voice Authorization Fee",
    "Chargeback Fee",
    "Representment Fee",
    "Collection, Pre-Arbitration & Compliance Fee"
  ),
  "CC" = c("Cross Currency Markup"),
  "purchasing-lvl3" = c("Purchasing Card Level 3 Transaction Fee"),
  "net-connect" = c("NetConnect Fees", "NetConnect Batch Monthly Fees")
)

volumeCapabilities <- list(
  "VS" = c("Proposed Gross Visa Volume",
           "Proposed Gross Visa Txns"),
  "MC" = c(
    "Proposed Gross Mastercard Volume",
    "Proposed Gross Mastercard Txns"
  ),
  "AMXSOB"  = c("Proposed Gross AMEX Transactions"),
  "AMXCVYD" = c("Proposed Gross AMEX Conveyed Transactions"),
  "DSCSTLD" = c("Proposed Discover/JCB/Dinners Settled Transactions"),
  "CC" = c("% Cross Currency Volume")
)


map <-
  list(
    "VS"      = "Credit",
    "MC"      = "Credit",
    "AMXSOB"  = "Credit",
    "AMXCVYD" = "Credit",
    "DSCSTLD" = "Credit",
    "CC"      = "Credit",
    "PP"      = "Dedit"
  )

tieredDataTable <-
  function (df,
            tiers,
            range,
            capabilities,
            rownameHeader,
            editable = TRUE,
            header = TRUE) {
    dataTable = NULL          
    if(header) {
      colNames <- c("", colnames(df))
      bucket_range <- c(rownameHeader, range)
      sketch = htmltools::withTags(table(class = 'display',
                                        thead(tr(
                                          lapply(colNames, th)
                                        ),
                                        tr(
                                          lapply(bucket_range, th)
                                        ))))
      dataTable = datatable(
        data = df,
        container = sketch,
        rownames = capabilities,
        colnames = colNames,
        fillContainer = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE
        ),
        class = 'cell-border stripe',
        editable = editable
      )
    } else {
      dataTable = datatable(
        data = df,
        colnames = NULL,
        rownames = capabilities,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE
        ),
        class = 'cell-border stripe',
        editable = editable
      )      
    } 
    dataTable %>% formatStyle(1:(tiers + 1),
                      color = 'black') %>% formatStyle(
                        columns = 0,
                        color = 'red',
                        backgroundColor = 'orange',
                        fontWeight = 'bold'
                      )
    
  }

tableColNames <- function(tiers) {
  c("Benchmark", map_chr(1:tiers, function (v) paste("Tier", sep = "", v)))
}

createDataFrame <- function(r, c, colNames, rowNames = NULL) {
  df <- data.frame(matrix(rep(0, c * r), ncol = c, nrow = r))
  if(!is.null(rowNames)) 
    rownames(df) <- rowNames
  colnames(df) <- colNames
  df
}

tibble_from <- function(name, cols = NULL) {
  file_name <- paste(paste(".", "data", name, sep = "/"), "csv", sep = ".")
  if (file.exists(file_name)) {
    read_csv(file_name)
  } else {
    tibble()
  }
}

save_frame <- function(tb, name) {
  file_name <- paste(paste(".", "data", name, sep = "/"), "csv", sep = ".")
  if (!file.exists(file_name)) {
    file_create(file_name)
  }
  
}

epoch_time <- function() as.integer(Sys.time())

read_merchant_info <- function() {
  df <- tibble_from("merchant_info")
  if(is_empty(df)) {
    tribble(
      ~MERCHANT_ID, ~NAME, ~CONTRACT_YEARS, ~N_LOCATIONS, ~N_FRANCHISE, ~FUND_XFER_TYPE
    )
  } else {
    df
  }
}

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(disable = F),
  dashboardSidebar(disable = TRUE),
  mainBody,
  title = "Pricing Calculator",
  skin = "red"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  nTiers <- reactive({
    as.integer(input$noOfTiers)
  })
  
  projectedTransactions <- reactive({
    value  <- input$valueOfTierMethods
    method <- input$tierMethod
    as.double(gsub(",", "", value))
  })
  
  selectedPricingCapabilities <- reactive({
    input$capabilities
  })
  
  bucket_range <- reactive({
    estimates <- projectedTransactions()
    tiers <- nTiers()
    buckets <-  seq(0, estimates, by = estimates / tiers)
    range <- rep("", length(buckets))
    for (i in seq_along(buckets)) {
      if (buckets[i] == 0)
        range[i] = ""
      else
        range[i] = paste(buckets[i - 1], "-", as.integer(buckets[i]))
    }
    range
  })

  tables <- reactiveValues()
  standardInputsDF <- reactive({
    if(is.null(tables[["standardInputDF"]])) {
      tiers = nTiers()
      tables[["standardInputDF"]] = createDataFrame(length(allCapabilities), tiers + 1, tableColNames(tiers), allCapabilities)
    }
    tables[["standardInputDF"]]
  })
  
  standardInputProxy <- dataTableProxy('standardInput')
  observeEvent(input$standardInput_cell_edit, {
    info = input$standardInput_cell_edit
    x = tables[["standardInputDF"]]
    i = info$row
    j = info$col
    v = info$value
    x[i, j] = DT::coerceValue(v, x[i, j])
    tables[["standardInputDF"]] = x
    replaceData(standardInputProxy, x, resetPaging = FALSE)
  })
  output$standardInput <- renderDT({
    tieredDataTable(
      standardInputsDF(),
      nTiers(),
      bucket_range(),
      allCapabilities,
      "Standard Inputs"
    )
  })
  
  
  volumeInputsDF <- reactive({
    if(is.null(tables[["volumeInputsDF"]])) {
      tiers = nTiers()
      tables[["volumeInputsDF"]] = createDataFrame(length(volumeCapabilities), tiers + 1, tableColNames(tiers))
    }
    tables[["volumeInputsDF"]]
  })
  volumeInputProxy   <- dataTableProxy('volumeInput')
  observeEvent(input$volumeInput_cell_edit, {
    info = input$volumeInput_cell_edit
    x = tables[["volumeInputsDF"]]
    i = info$row
    j = info$col
    v = info$value
    x[i, j] = DT::coerceValue(v, x[i, j])
    tables[["volumeInputsDF"]] = x
    replaceData(volumeInputProxy, x, resetPaging = FALSE)
  })
  output$volumeInput <- renderDT({
    selectedCapabilities <- input$capabilities
    tieredDataTable(
      volumeInputsDF(),
      nTiers(),
      bucket_range(),
      unlist(volumeCapabilities[selectedCapabilities], use.names = FALSE),
      "Volume Section",
      header = TRUE
    )
  })
  
  
  pricingInputsDF <- reactive({
    if(is.null(tables[["pricingInputsDF"]])) {
      tiers = nTiers()
      tables[["pricingInputsDF"]] = createDataFrame(length(unlist(creditCapabilities["all"], use.names = FALSE)), tiers + 1, tableColNames(tiers))
    }
    tables[["pricingInputsDF"]]
  })
  
  pricingInputProxy  <- dataTableProxy('pricingInput')
  observeEvent(input$pricingInput_cell_edit, {
    info = input$pricingInput_cell_edit
    x = tables[["pricingInputsDF"]]
    i = info$row
    j = info$col
    v = info$value
    x[i, j] <- DT::coerceValue(v, x[i, j])
    tables[["pricingInputsDF"]] = x
    replaceData(pricingInputProxy, x, resetPaging = FALSE)
  })
  output$pricingInput <- renderDT({
    selectedCapabilities <- input$capabilities
    tieredDataTable(
      pricingInputsDF(),
      nTiers(),
      bucket_range(),
      unlist(creditCapabilities["all"], use.names = FALSE),
      "Pricing Section",
      header = TRUE
    )
  })
  
  
  creditProcessingRevenueDF <- reactive({
    if(is.null(tables[["creditProcessingRevenueDF"]])) {
      tiers = nTiers()
      tables[["creditProcessingRevenueDF"]] = createDataFrame(length(creditProcessingRevenueItems), tiers + 1, tableColNames(tiers))
    }
    tables[["creditProcessingRevenueDF"]]
  })
  
  creditProcessingRevenueProxy  <- dataTableProxy('creditProcessingRevenue')
  observeEvent(input$pricingInput_cell_edit, {
    info = input$pricingInput_cell_edit
    x = tables[["creditProcessingRevenueDF"]]
    i = info$row
    j = info$col
    v = info$value
    x[i, j] <- DT::coerceValue(v, x[i, j])
    tables[["creditProcessingRevenueDF"]] = x
    replaceData(creditProcessingRevenueProxy, x, resetPaging = FALSE)
  })
  output$creditProcessingRevenueInput <- renderDT({
    tieredDataTable(
      creditProcessingRevenueDF(),
      nTiers(),
      bucket_range(),
      creditProcessingRevenueItems,
      "Credit Processing Revenue",
      header = TRUE,
      editable = FALSE
    )
  })
  

  creditCurrencyRevenueDF <- reactive({
    if(is.null(tables[["creditCurrencyRevenueDF"]])) {
      tiers = nTiers()
      tables[["creditCurrencyRevenueDF"]] = createDataFrame(length(creditCurrencyRevenueItems), tiers + 1, tableColNames(tiers))
    }
    tables[["creditCurrencyRevenueDF"]]
  })
  
  creditCurrencyRevenueProxy  <- dataTableProxy('creditCurrencyRevenue')
  observeEvent(input$pricingInput_cell_edit, {
    info = input$pricingInput_cell_edit
    x = tables[["creditCurrencyRevenueDF"]]
    i = info$row
    j = info$col
    v = info$value
    x[i, j] <- DT::coerceValue(v, x[i, j])
    tables[["creditCurrencyRevenueDF"]] = x
    replaceData(creditCurrencyRevenueProxy, x, resetPaging = FALSE)
  })
  output$creditCurrencyRevenueInput <- renderDT({
    tieredDataTable(
      creditCurrencyRevenueDF(),
      nTiers(),
      bucket_range(),
      creditCurrencyRevenueItems,
      "Credit Currency Revenue",
      header = TRUE,
      editable = FALSE
    )
  })
  
  merchantInformationDF <- read_merchant_info()
  observeEvent(input$save, {
    merchantId <- epoch_time()
    add_row(
      merchantInformationDF,
      MERCHANT_ID = merchantId,
      NAME = input$merchantName,
      CONTRACT_YEARS = input$nYearsOfContractTerm,
      N_LOCATIONS = input$noOfContractLocations,
      N_FRANCHISE = input$noOfFranchiseLocations,
      FUND_XFER_TYPE = input$fundTransferType
    )

    save_frame(merchantInformationDF, "merchant_info")
  })
  
  output$debugMe <- renderText({
    op <- paste("<b> No Of Tiers:", nTiers(), "<br> <br>")
    op <-
      paste(op,
            "<b>Project Transactions:",
            projectedTransactions(),
            "<br>")
    op <-
      paste(op,
            "<b>Selected Capabilities:",
            selectedPricingCapabilities(),
            "<br>")
    ""
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
