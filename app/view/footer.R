# Box imports -----------------------------------------------------------------

box::use(
  shiny[div],
  shiny.blueprint[ControlGroup, Text],
)

# -----------------------------------------------------------------------------

footer <- ControlGroup(
  fill = TRUE,
  Text("Built with \U2764\UFE0F by Appsilon"), # red heart unicode

  div(
    style = "text-align: right;",
    Text("All rights reserved.")
  )
)
