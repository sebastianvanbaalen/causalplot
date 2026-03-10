library(shiny)
library(causalplot)
library(ggplot2)
library(shinythemes)

# ---------------------------------------------------------------------------
# Template field definitions
# ---------------------------------------------------------------------------
# Each causal_plot template maps to its ordered input-field labels.
# The typology() function is handled separately.

causal_fields <- list(
  "111" = c(
    "Independent variable",
    "Causal mechanism",
    "Dependent variable"
  ),
  "1111" = c(
    "Independent variable",
    "Causal mechanism step 1",
    "Causal mechanism step 2",
    "Dependent variable"
  ),
  "11111" = c(
    "Independent variable",
    "Mechanism step 1",
    "Mechanism step 2",
    "Mechanism step 3",
    "Dependent variable"
  ),
  "1121" = c(
    "Independent variable",
    "Shared mechanism step 1",
    "Path 1: mechanism step 2",
    "Path 2: mechanism step 2",
    "Dependent variable"
  ),
  "1211" = c(
    "Independent variable",
    "Path 1: mechanism step 1",
    "Path 2: mechanism step 1",
    "Shared mechanism step 2",
    "Dependent variable"
  ),
  "1221" = c(
    "Independent variable",
    "Path 1: mechanism step 1",
    "Path 2: mechanism step 1",
    "Path 1: mechanism step 2",
    "Path 2: mechanism step 2",
    "Dependent variable"
  ),
  "bathtub" = c(
    "Independent variable",
    "Bottom path: step 1",
    "Bottom path: step 2",
    "Dependent variable"
  ),
  "111_moderator" = c(
    "Independent variable",
    "Causal mechanism",
    "Dependent variable",
    "Moderator variable"
  ),
  "111_confounder" = c(
    "Independent variable",
    "Causal mechanism",
    "Dependent variable",
    "Confounder"
  ),
  "211" = c(
    "IV 1",
    "IV 2",
    "Shared mechanism",
    "Dependent variable"
  ),
  "221" = c(
    "IV 1",
    "IV 2",
    "Path 1: mechanism",
    "Path 2: mechanism",
    "Dependent variable"
  ),
  "2221" = c(
    "IV 1",
    "IV 2",
    "Path 1: mechanism step 1",
    "Path 2: mechanism step 1",
    "Path 1: mechanism step 2",
    "Path 2: mechanism step 2",
    "Dependent variable"
  )
)

typology_fields <- list(
  type_labels = c("Type 1", "Type 2", "Type 3", "Type 4"),
  x_axis_label = "Dimension 2",
  y_axis_label = "Dimension 1",
  x_axis_values = c("Low", "High"),
  y_axis_values = c("Low", "High")
)

# Template display names for the dropdown
template_choices <- c(
  "111  \u2013 One-step mechanism (3 boxes)"          = "111",
  "1111 \u2013 Two-step mechanism (4 boxes)"           = "1111",
  "11111 \u2013 Three-step mechanism (5 boxes)"        = "11111",
  "1121 \u2013 Dual pathway, joint first step (5)"     = "1121",
  "1211 \u2013 Dual pathway, joint second step (5)"    = "1211",
  "1221 \u2013 Full dual pathway (6 boxes)"            = "1221",
  "bathtub \u2013 Classic bathtub model (4 boxes)"     = "bathtub",
  "111 + moderator (4 boxes)"                          = "111_moderator",
  "111 + confounder (4 boxes)"                         = "111_confounder",
  "211  \u2013 Two IVs, shared mechanism (4 boxes)"    = "211",
  "221  \u2013 Two IVs, separate mechanisms (5 boxes)" = "221",
  "2221 \u2013 Two IVs, two-step mechanisms (7 boxes)" = "2221",
  "Typology \u2013 2\u00d72 typology diagram"          = "typology"
)

# Attribution caption shared by all plots
attribution <- "Generated using the causalplot R package created by Sebastian van Baalen"

# Helper: add caption to a ggplot object
add_caption <- function(p, extra = NULL) {
  caption_text <- if (!is.null(extra) && nzchar(extra)) {
    paste0(extra, "\n", attribution)
  } else {
    attribution
  }
  p +
    labs(caption = caption_text) +
    theme(
      plot.caption = element_text(size = 7, colour = "grey50", hjust = 1),
      plot.margin = margin(5, 10, 5, 5)
    )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- navbarPage(
  #theme = shinytheme("yeti"),
  title = HTML(
    'Build publication-ready causal diagrams using the ',
    '<a href="https://github.com/sebastianvanbaalen/causalplot"',
    ' target="_blank"; text-decoration:underline;">',
    'causalplot</a> package'
  ),

  # === Tab 1: Main builder ===
  tabPanel(
    "Diagram builder",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("template", "Choose template", choices = template_choices),
        uiOutput("main_inputs"),
        hr(),
        downloadButton("main_download", "Save as PNG", class="btn btn-success"),
        downloadButton("main_download_pdf", "Save as PDF", class="btn btn-success"),
        hr(),
        helpText(
          "For more advanced features (colors, fonts, figure dimensions, etc.),",
          "use the causalplot R package directly.",
          "See the",
          tags$a(
            "package documentation",
            href = "https://github.com/sebastianvanbaalen/causalplot",
            target = "_blank"
          ),
          "for details."
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("main_plot", height = "500px")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # === Main tab: dynamic inputs ===
  output$main_inputs <- renderUI({
    tmpl <- input$template

    if (tmpl == "typology") {
      # Typology has its own input structure
      tagList(
        h5("Type labels"),
        textInput("typo_tl1", "Top-left",     value = typology_fields$type_labels[1]),
        textInput("typo_tl2", "Top-right",    value = typology_fields$type_labels[2]),
        textInput("typo_tl3", "Bottom-left",  value = typology_fields$type_labels[3]),
        textInput("typo_tl4", "Bottom-right", value = typology_fields$type_labels[4]),
        h5("Axis labels"),
        textInput("typo_xlab", "X-axis label", value = typology_fields$x_axis_label),
        textInput("typo_ylab", "Y-axis label", value = typology_fields$y_axis_label),
        h5("Axis endpoint values"),
        textInput("typo_xval1", "X-axis left value",   value = typology_fields$x_axis_values[1]),
        textInput("typo_xval2", "X-axis right value",  value = typology_fields$x_axis_values[2]),
        textInput("typo_yval1", "Y-axis top value",    value = typology_fields$y_axis_values[1]),
        textInput("typo_yval2", "Y-axis bottom value", value = typology_fields$y_axis_values[2])
      )
    } else {
      # Causal plot templates: generate one text input per label
      fields <- causal_fields[[tmpl]]
      tagList(
        lapply(seq_along(fields), function(i) {
          textInput(
            inputId = paste0("label_", i),
            label   = fields[i],
            value   = fields[i]
          )
        })
      )
    }
  })

  # === Main tab: reactive plot ===
  main_plot_reactive <- reactive({
    tmpl <- input$template

    if (tmpl == "typology") {
      # Collect typology inputs (with safe fallbacks)
      tl <- c(
        input$typo_tl1 %||% "Type 1",
        input$typo_tl2 %||% "Type 2",
        input$typo_tl3 %||% "Type 3",
        input$typo_tl4 %||% "Type 4"
      )
      p <- typology(
        type_labels   = tl,
        x_axis_label  = input$typo_xlab  %||% "Dimension 2",
        y_axis_label  = input$typo_ylab  %||% "Dimension 1",
        x_axis_values = c(
          input$typo_xval1 %||% "Low",
          input$typo_xval2 %||% "High"
        ),
        y_axis_values = c(
          input$typo_yval1 %||% "Low",
          input$typo_yval2 %||% "High"
        ),
        text_size = 5,
        axis_text_size = 4
      )
    } else {
      fields <- causal_fields[[tmpl]]
      labels <- vapply(seq_along(fields), function(i) {
        val <- input[[paste0("label_", i)]]
        if (is.null(val) || !nzchar(val)) fields[i] else val
      }, character(1))

      p <- causal_plot(
        type = tmpl,
        labels = labels,
        text_size = 5
      )
    }

    add_caption(p)
  })

  output$main_plot <- renderPlot({ main_plot_reactive() })

  output$main_download <- downloadHandler(
    filename = function() { "causal_diagram.png" },
    content = function(file) {
      ggsave(file, plot = main_plot_reactive(),
             width = 12, height = 5, dpi = 300)
    }
  )

  output$main_download_pdf <- downloadHandler(
    filename = function() { "causal_diagram.pdf" },
    content = function(file) {
      ggsave(file, plot = main_plot_reactive(),
             width = 12, height = 5, device = "pdf")
    }
  )
}

shinyApp(ui, server)
