# Box imports -----------------------------------------------------------------

box::use(
  shiny[div],
  shiny.blueprint[ControlGroup, Text],
)

# -----------------------------------------------------------------------------

footer <- ControlGroup(
  fill = TRUE,
  Text("Built with ❤ by Appsilon"),
  
  div(
    style = "text-align: right;",
    Text("All rights reserved.")
  )
)
