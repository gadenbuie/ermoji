#' The ermoji emoji gadget
#'
#' Opens a miniUI based Shiny gadget in the RStudio Viewer pane with a
#' searchable table of emoji. Select a row and click the copy button.
#'
#' @return nothing
#' @export
ermoji_gadget <- function() {
  require(shiny)
  require(miniUI)
  runGadget(ermoji_ui(), ermoji_server, viewer = paneViewer(500), stopOnCancel = FALSE)
}

ermoji_shiny <- function() {
  require(shiny)
  require(miniUI)
  shinyApp(ui = ermoji_ui(), server = ermoji_server)
}


ermoji_ui <- function() {
  miniPage(
    title = "ermoji",
    gadgetTitleBar("ermoji"),
    miniContentPanel(
      padding = 10,
      DT::dataTableOutput('emojis', height = "100%", width = "98%")
    ),
    miniButtonBlock(
      actionButton("copy_name", "Copy :emoji_name:", class = "btn-success"),
      actionButton("copy_utf", "Copy Unicode", class = "btn-warning"),
      actionButton("copy_gliph", "Copy Emoji", class = "btn-primary")
    )
  )
}

ermoji_server <- function(input, output, session, clipout = clipr::clipr_available()) {
  output$emojis <- DT::renderDataTable({
    emojis <- emo::jis
    emojis <- emojis[, c('emoji', 'name', "group", "keywords", "aliases")]
    emojis$keywords <- purrr::map_chr(emojis$keywords, ~ paste(., collapse = ", "))
    emojis$aliases <- purrr::map_chr(emojis$aliases, ~ paste(., collapse = ", "))
    DT::datatable(
      emojis,
      rownames = FALSE,
      colnames = c("Emoji", "Name", "Group", "Keywords", "Aliases"),
      filter = "top",
      selection = "single",
      fillContainer = TRUE,
      # style = 'bootstrap',
      class = 'compact stripe nowrap hover',
      options = list(
        searchHighlight = TRUE,
        search = list(regex = TRUE, caseInsensitive = FALSE),
        columnDefs = list(list(
          className = "dt-center", targets = 0
        )),
        pageLength = 10,
        lengthMenu = c(4, 5, 10)
      )
    )
  })

  this_emoji <- reactive({
    req(input$emojis_rows_selected)
    as.list(emo::jis[input$emojis_rows_selected, ])
  })

  this_emoji_name <- reactive({
    # name <- this_emoji()$name
    name <- this_emoji()$aliases[[1]][1]
    paste0(":", gsub(" ", "_", name), ":")
  })

  this_emoji_uni <- reactive({
    uni <- paste0("\\U", this_emoji()$runes)
    gsub(" ", "\\\\U", uni)
  })

  this_emoji_uni_trunc <- reactive({
    uni <- this_emoji()$runes
    uni <- sub(" .+", "...", uni)
    paste0("\\U", uni)
  })

  observeEvent(input$emojis_rows_selected, {
    if (!isTruthy(input$emojis_rows_selected)) {
      updateActionButton(session, "copy_name", "Copy :emoji_name:")
      updateActionButton(session, "copy_utf", "Copy Unicode")
      updateActionButton(session, "copy_gliph", "Copy Emoji")
    } else {
      updateActionButton(session, "copy_name", paste0("Copy <code>", this_emoji_name(), "</code>"))
      updateActionButton(session, "copy_utf", paste("Copy <code>", this_emoji_uni_trunc(), "</code>"))
      updateActionButton(session, "copy_gliph", paste("Copy", this_emoji()$emoji))
    }
  })
  copy_modal <- function(text) {
    showModal(
      modalDialog(
        title = "Select and Copy",
        tags$p("I don't have access to your clipboard. Select the text and", tags$kbd("Ctrl/Cmd"), "+", tags$kbd("c"), "to copy."),
        tags$pre(text),
        easyClose = TRUE
      )
    )
  }
  observeEvent(input$copy_name, {
    if (clipout) clipr::write_clip(this_emoji_name()) else copy_modal(this_emoji_name())
  })
  observeEvent(input$copy_utf, {
    if (clipout) clipr::write_clip(this_emoji_uni()) else copy_modal(this_emoji_uni())
  })
  observeEvent(input$copy_gliph, {
    if (clipout) clipr::write_clip(this_emoji()$emoji) else copy_modal(this_emoji()$emoji)
  })
  observeEvent(input$done, {
    stopApp(invisible())
  })
  observeEvent(input$cancel, {
    stopApp(invisible())
  })
}
