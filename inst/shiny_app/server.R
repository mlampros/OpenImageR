
function(input, output) {

  output$myImage <- shiny::renderImage({

    list(src = shiny::getShinyOption("OpenImageR_file_path"),          # set by runUI()

         height = input$height_width,

         width = input$height_width
    )

  }, deleteFile = FALSE)
}
