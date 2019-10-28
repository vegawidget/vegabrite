library(shiny)
library(miniUI)
library(vlbuildr)
library(vegawidget)

get_version <- function(file, ref){
  tryCatch(
    processx::run("git", c("show",paste(ref,file,sep=":")))$stdout,
    error = function(e) { NA_character_ }
  )
}


regressionTestViewer <- function(reference_branch = "origin/master") {
  
  processx::run("git","fetch")
  reference_commit <- trimws(processx::run("git", c("rev-parse", reference_branch))$stdout)
  changed <- strsplit(processx::run("git", c("diff","--name-only", reference, "HEAD"))$stdout,"\n")[[1]]
  changed_examples <- changed[grepl("^tests/regression/.*.json",changed)]
  
  previous_versions <- vapply(changed_examples, get_version, "", reference_commit) 
  current_versions <- vapply(changed_examples, get_version, "", "HEAD") 
  
  n_examples <- length(changed_examples)
  
  if (n_examples == 0) {
    message("No changes!")
    return()
  }
  
  ui <- miniPage(
    gadgetTitleBar("Old <----------- Regression Tests -----------> New"),
    miniContentPanel(
      fillRow(
          vegawidgetOutput("oldChart"),
          vegawidgetOutput("newChart")
      )
    ),
    miniButtonBlock(
      actionButton("previousExample", "Previous"),
      h3(textOutput("exampleName")),
      actionButton("nextExample", "Next")
    )
  )
  
  server <- function(input, output, session) {

    i <- reactiveVal(1)
    
    previous_spec <- reactive({
      ix <- i()
      if (is.na(previous_versions[[ix]])) {
        NULL
      } else {
        vegawidget(jsonlite::fromJSON(previous_versions[[ix]], simplifyVector = FALSE))
      }
    })

    current_spec <- reactive({
      ix <- i()
      if (is.na(current_versions[[ix]])) {
        NULL
      } else {
        vegawidget(jsonlite::fromJSON(current_versions[[ix]], simplifyVector = FALSE))
      }
    })
 
    # Define reactive expressions, outputs, etc.
    output$newChart <- renderVegawidget({quote(current_spec())}, quoted = TRUE)
    output$oldChart <- renderVegawidget({quote(previous_spec())}, quoted = TRUE)

    output$exampleName <- renderText({
      basename(changed_examples[[i()]])
    })
    
    observeEvent(input$nextExample, {
      old_val <- i()
      if (old_val != n_examples) {
        i(old_val + 1)
      }
    })
    
    observeEvent(input$previousExample, {
      old_val <- i()
      if (old_val != 1) {
        i(old_val - 1)
      }
    })
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = browserViewer())
}