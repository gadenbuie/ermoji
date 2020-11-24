#' Emoji Picker
#'
#' An emoji picker gadget that lets you search and find emoji and insert either
#' the unicode, HTML, or `emo::ji()` versions of the emoji. Built using the
#' [Emoji Button](https://emoji-button.js.org/) JavaScript library.
#'
#' @return A list with the `emoji`, the emoji `name` and the `html` entity.
#'
#' @param gadget If `TRUE` the app is run as a gadget, otherwise it's run as a
#'   standalone app.
#'
#' @export
emoji_picker <- function(gadget = TRUE) {
  shiny::addResourcePath("eb", system.file("picker", package = "ermoji"))

  if (gadget) {
    shiny::runGadget(
      emoji_picker_ui(),
      emoji_picker_server(quick_add = TRUE),
      viewer = shiny::dialogViewer("ermoji", width = 375, height = 463)
    )
  } else {
    shiny::shinyApp(emoji_picker_ui(), emoji_picker_server(quick_add = FALSE))
  }
}

emoji_picker_ui <- function() {
  shiny::fluidPage(
    id = "picker-gadget",
    class = paste(
      if (is_rstudio_dark()) "dark-theme",
      get_picker_type()
    ),
    shiny::div(id = "emoji-picker", style = "width: 100%; min-height: 425; position: relative;"),
    rstudio_style(),
    shiny::tags$script(src = "eb/emoji-picker.js", type = "module"),
    shiny::tags$head(
      shiny::tags$script(src = 'eb/he.js'),
      shiny::tags$style(
        '.emoji-picker__plugin-container { justify-content: space-between; }
        .emoji-picker { border-radius: 0 !important; }'
      )
    )
  )
}

is_rstudio_dark <- function() {
  if (rstudioapi::hasFun("getThemeInfo")) {
    rstudioapi::getThemeInfo()$dark
  } else {
    FALSE
  }
}

rstudio_style <- function() {
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(NULL)
  }
  theme <- rstudioapi::getThemeInfo()
  shiny::tags$style(shiny::HTML(paste0(
    ".emoji-picker {\n",
    if (theme$dark) "  --dark-background-color: " else "  --background-color: ", theme$background, ";\n",
    if (theme$dark) "  --dark-text-color: " else "  --text-color: ", theme$foreground, ";\n",
    "}\n",
    "body { background-color: ", theme$background, "; color: ", theme$foreground, ";}\n",
    "#picker_type {\n",
    "  display: flex;\n",
    "  justify-content: space-between;\n",
    "  width: 100%;\n",
    "}"
  )))
}

set_picker_type <- function(style = c("unicode", "html", "emo_ji")) {
  if (!rstudioapi::hasFun("setPersistentValue")) {
    return()
  }
  style <- match.arg(style)
  rstudioapi::setPersistentValue("ermoji.picker_type", style)
  invisible(style)
}

get_picker_type <- function() {
  if (!rstudioapi::hasFun("setPersistentValue")) {
    return("unicode")
  }
  style <- rstudioapi::getPersistentValue("ermoji.picker_type")
  if (is.null(style)) {
    return("unicode")
  }
  if (!style %in% c("unicode", "html", "emo_ji")) {
    return("unicode")
  }
  style
}

emoji_picker_server <- function(quick_add = TRUE, document_id = NULL) {
  if (is.null(document_id)) {
    document_id <- rstudioapi::getActiveDocumentContext()$id
  }
  function(input, output, session) {
    shiny::observeEvent(input$close, {
      shiny::stopApp(invisible(input$emoji))
    })

    shiny::observe({
      session$sendCustomMessage('update_picker_type', get_picker_type())
    })

    shiny::observeEvent(input$picker_type, {
      set_picker_type(input$picker_type)
    })

    shiny::observeEvent(input$emoji, {
      emoji <- switch(
        input$picker_type,
        "unicode" = input$emoji$emoji,
        "html" = input$emoji$html,
        "emo_ji" = {
          tryCatch({
            emo::ji(input$emoji$name)
            paste0('emo::ji("', input$emoji$name, '")')
          },
            error = function(e) {
              message("{emo} doesn't know the emoji ", shQuote(input$emoji$name))
              NULL
            }
          )
        }
      )
      if (!is.null(emoji)) {
        rstudioapi::insertText(emoji, id = document_id)
        if (isTRUE(quick_add)) {
          shiny::stopApp(invisible(input$emoji))
        }
      }
    })
  }
}
