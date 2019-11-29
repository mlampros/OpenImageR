
shiny::pageWithSidebar(

  shiny::headerPanel("display image"),

  shiny::sidebarPanel(

    shiny::sliderInput("height_width", "Adjust Image Height and Width:",

                       min = 1, max = 1000,  value = 575)
  ),
  shiny::mainPanel(

    shiny::imageOutput("myImage")
  )
)
