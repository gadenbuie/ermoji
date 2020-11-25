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
    shiny::HTML('<div id="alert_bad_emo_ji" class="alert alert-danger" role="alert"><strong style="font-family: monospace">emo</strong> doesn\'t know the emoji <strong id="bad_emo_ji_name"></strong></div>'),
    rstudio_style(),
    shiny::tags$script(src = "eb/emoji-picker.js", type = "module"),
    shiny::tags$head(
      shiny::tags$script(src = 'eb/he.js'),
      shiny::tags$style('
.emoji-picker__plugin-container { justify-content: space-between; }
.emoji-picker { border-radius: 0 !important; }

#alert_bad_emo_ji {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  z-index: 9999;
  border-radius: 0;
  transform: translateY(-100px);
  transition: transform 0.5s ease-in;
  text-align: center;
}

#alert_bad_emo_ji.show-bad-emo-ji {
  transform: translateY(0);
}

#bad_emo_ji_name {
  white-space: nowrap;
}
'
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

emoji_picker_server <- function(quick_add = TRUE, context = NULL) {
  if (is.null(context)) {
    context <- rstudioapi::getActiveDocumentContext()
  }
  initial_search <- if (nzchar(context$selection[[1]]$text)) {
    context$selection[[1]]$text
  }
  function(input, output, session) {
    shiny::observeEvent(input$close, {
      shiny::stopApp(invisible(input$emoji))
    })

    shiny::observe({
      session$sendCustomMessage('update_picker_type', get_picker_type())
      session$sendCustomMessage("search_emoji", initial_search)
    })

    shiny::observeEvent(input$picker_type, {
      set_picker_type(input$picker_type)
    })

    shiny::observeEvent(input$emoji, {
      emoji <- switch(
        input$picker_type,
        "unicode" = input$emoji$emoji,
        "html" = input$emoji$html,
        "emo_ji" = try_emo_ji(input$emoji$name, session = session)
      )
      if (!is.null(emoji)) {
        rstudioapi::insertText(
          location = Map(function(x) x$range, context$selection),
          text = emoji,
          id = context$id
        )
        if (isTRUE(quick_add)) {
          shiny::stopApp(invisible(input$emoji))
        }
      }
    })
  }
}

try_emo_ji <- function(name, session = shiny::getDefaultReactiveDomain()) {
  emo_ji <- tryCatch({
    emo::ji(input$emoji$name)
    paste0('emo::ji("', input$emoji$name, '")')
  },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(emo_ji)) return(emo_ji)

  name_no_space <- gsub(" ", "_", name)
  tryCatch({
    emo::ji(name_no_space)
    paste0('emo::ji("', name_no_space, '")')
  },
    error = function(e) {
      session$sendCustomMessage("bad_emo_ji", name)
      message("{emo} doesn't know the emoji ", shQuote(name))
      NULL
    }
  )
}
