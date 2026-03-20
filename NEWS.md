# causalplot 0.2.2

* Add `121` template: one IV, two parallel mechanisms, one DV.

# causalplot 0.2.1

* Prepare for CRAN submission.
* Add `run_app()` function to launch an interactive Shiny app for building diagrams.
* Switch to MIT license.
* Increase maximum allowed lines per box from 3 to 5.
* Fix R CMD check notes (non-portable filenames, explicit exports).

# causalplot 0.2.0

* Add new templates: `211`, `221`, and `2221` for multi-IV designs.
* Add `typology()` function for creating 2x2 typology diagrams.
* Reduce whitespace around plots with auto-computed axis limits.
* Add `111_moderator` and `111_confounder` templates.

# causalplot 0.1.0

* Initial release on GitHub.
* Core `causal_plot()` function with templates: `111`, `1111`, `11111`, `1121`, `1211`, `1221`, and `bathtub`.
* Customizable labels, colors, fonts, corner radius, and arrow styles.
* Built on `ggplot2`, `ggforce`, and `ggtext`.
