#' The ermoji emoji gadget
#'
#' Opens a miniUI based Shiny gadget in the RStudio Viewer pane with a
#' searchable table of emoji. Select a row and click the copy desired button.
#'
#' @param clipout Should the gadget attempt to write to the clipboard?
#' @param ... Ignored at this time
#' @name ermoji
#' @return nothing
#' @export
ermoji_gadget <- function(clipout = clipr::clipr_available(), ...) {
  shiny::runGadget(ermoji_ui, ermoji_server(clipout, ...), viewer = shiny::paneViewer(500), stopOnCancel = FALSE)
}

#' @rdname ermoji
#' @export
ermoji_shiny <- function(clipout = clipr::clipr_available(), ...) {
  shiny::shinyApp(ui = ermoji_ui, server = ermoji_server(clipout, ...))
}


ermoji_ui <- miniUI::miniPage(
    title = "ermoji",
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
        .dropdown-item {
        	display: block;
        	width: 100%;
        	padding: .25rem 1.5rem;
        	clear: both;
        	font-weight: 400;
        	color: #212529;
        	text-align: inherit;
        	white-space: nowrap;
        	background-color: transparent;
        	border: 0;
        }
        .dropdown-menu {
        	color: #212529;
        	text-align: left;
        	list-style: none;
        }
        @media (min-height: 600px) {
          td.big { font-size: 2em; }
        }
        @media (max-height: 599px) {
          td.big { font-size: 1.5em; }
        }
        table.dataTable tbody td {
          vertical-align: middle;
        }
        ")
      )
    ),
    miniUI::gadgetTitleBar("ermoji"),
    miniUI::miniContentPanel(
      padding = 10,
      DT::dataTableOutput('emojis', height = "100%", width = "98%")
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("copy_name", "Copy :emoji_name:", class = "btn-success"),
      shiny::tags$div(class = "btn-group dropup", style = "width: 33%",
        shiny::tags$button(class = "btn btn-warning dropdown-toggle", href = "#",
                    role = "button", id = "dropdownMenuLink", style = "width: 100%",
                    "data-toggle" = "dropdown", "aria-haspopup" = "true",
                    "aria-expanded" = "false",
                    "Copy Unicode"),
        shiny::tags$div(class = "dropdown-menu", style = "width: 100%",
                 "aria-labelledby"="dropdownMenuLink",
          shiny::actionLink("copy_utf", "Copy unicode", class = "dropdown-item"),
          shiny::actionLink("copy_html", "Copy HTML", class = "dropdown-item")
        )
      ),
      shiny::actionButton("copy_gliph", "Copy Emoji", class = "btn-primary")
    )
  )

ermoji_server <- function(clipout = clipr::clipr_available()) {
  function(input, output, session) {
    output$emojis <- DT::renderDataTable({
      emojis <- emo::jis
      emojis <- emojis[, c('emoji', 'name', "group", "keywords", "aliases")]
      emojis$group <- factor(emojis$group)
      emojis$keywords <- vapply(emojis$keywords, function(x) paste(x, collapse = ", "), character(1))
      emojis$aliases <- vapply(emojis$aliases, function(x) paste(x, collapse = ", "), character(1))
      DT::datatable(
        emojis,
        rownames = FALSE,
        colnames = c("Emoji", "Name", "Group", "Keywords", "Aliases"),
        filter = "top",
        selection = "single",
        fillContainer = TRUE,
        style = 'bootstrap',
        class = 'compact stripe hover',
        extensions = c("Scroller"),
        options = list(
          dom = "<'row'<'col-sm-6 col-sm-offset-2 text-center'f>>tir",
          searchHighlight = TRUE,
          search = list(regex = TRUE, caseInsensitive = FALSE),
          autoWidth = FALSE,
          columnDefs = list(
            list(
              className = "dt-center big", searchable = FALSE, width = "10%", targets = 0
            ),
            list(width = "20%", targets = c(1, 3, 4)),
            list(width = "10%", targets = 2)
          ),
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE
        )
      )
    })

    this_emoji <- shiny::reactive({
      shiny::req(input$emojis_rows_selected)
      as.list(emo::jis[input$emojis_rows_selected, ])
    })

    this_emoji_name <- shiny::reactive({
      # name <- this_emoji()$name
      name <- this_emoji()$aliases[[1]][1]
      paste0(":", gsub(" ", "_", name), ":")
    })

    this_emoji_uni <- shiny::reactive({
      uni <- paste0("\\U", this_emoji()$runes)
      gsub(" ", "\\\\U", uni)
    })

    this_emoji_html <- shiny::reactive({
      rune2html(this_emoji()$runes)
    })

    truncate <- function(x, n = 10) {
      if (nchar(x) > n) {
        paste0(strtrim(x, n), "...")
      } else x
    }

    shiny::observeEvent(input$emojis_rows_selected, {
      if (!shiny::isTruthy(input$emojis_rows_selected)) {
        shiny::updateActionButton(session, "copy_name", "Copy :emoji_name:")
        shiny::updateActionButton(session, "copy_utf", "Copy Unicode")
        shiny::updateActionButton(session, "copy_html", "Copy HTML")
        shiny::updateActionButton(session, "copy_gliph", "Copy Emoji")
      } else {
        shiny::updateActionButton(session, "copy_name", paste0("Copy <code>", this_emoji_name(), "</code>"))
        shiny::updateActionButton(session, "copy_utf", paste("Copy Unicode: <code>", truncate(this_emoji_uni()), "</code>"))
        shiny::updateActionButton(session, "copy_html", paste("Copy HTML: <code>", escape_html(truncate(this_emoji_html())), "</code>"))
        shiny::updateActionButton(session, "copy_gliph", paste("Copy", this_emoji()$emoji))
      }
    })
    copy_modal <- function(text) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Select and Copy",
          shiny::tags$p("I don't have access to your clipboard. Select the text and", shiny::tags$kbd("Ctrl/Cmd"), "+", shiny::tags$kbd("c"), "to copy."),
          shiny::tags$pre(text),
          easyClose = TRUE
        )
      )
    }
    shiny::observeEvent(input$copy_name, {
      if (clipout) clipr::write_clip(this_emoji_name()) else copy_modal(this_emoji_name())
    })
    shiny::observeEvent(input$copy_utf, {
      if (clipout) clipr::write_clip(this_emoji_uni()) else copy_modal(this_emoji_uni())
    })
    shiny::observeEvent(input$copy_html, {
      if (clipout) clipr::write_clip(this_emoji_html()) else copy_modal(this_emoji_html())
    })
    shiny::observeEvent(input$copy_gliph, {
      if (clipout) clipr::write_clip(this_emoji()$emoji) else copy_modal(this_emoji()$emoji)
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible())
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible())
    })
  }
}

escape_html <- function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x
}

#' Convert Emoji to HTML
#'
#' Uses [emo::ji()] to look up emoji, but return the escaped HTML.
#'
#' @param x A search term passed to [emo::ji()] or the result of [emo::ji()].
#' @param copy Should the result be copied to the clipboard?
#' @export
emoji2html <- function(x, copy = clipr::clipr_available()) {
  emojis <- emo::jis$runes
  names(emojis) <- emo::jis$emoji
  if (!inherits(x, "emoji")) {
    x <- emo::ji(x)
  }
  emoji <- emojis[x]
  emoji_html <- rune2html(unname(emoji))
  if (copy) {
    clipr::write_clip(emoji_html)
    message(emoji_html, " copied to clipboard")
    return(invisible(emoji_html))
  }
  emoji_html
}

rune2html <- function(runes) {
  gsub("([0-9A-F]{4,8}) ?", "&#x\\1;", runes)
}
