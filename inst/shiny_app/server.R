
function(input, output) {

  output$myImage <- renderImage({

    list(src = file_path,
         
         height = input$height_width,
         
         width = input$height_width
    )
    
  }, deleteFile = FALSE)
}