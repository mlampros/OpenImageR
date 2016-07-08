
pageWithSidebar(
  
  headerPanel("display image"),
  
  sidebarPanel(
    
    sliderInput("height_width", "Adjust Image Height and Width:",
                
                min = 1, max = 1000,  value = 575)
  ),
  mainPanel(

    imageOutput("myImage")
  )
)