### Shiny app for building PCA plots

#' R Shiny Application for Principal Component Analysis
#'
#' @return Shiny App
#' @export 
#'
#' @import shiny ggplot2 colourpicker shinyjs shinyWidgets DT data.table shinyBS
#' 
#' @examples 
#' \dontrun{
#' runShinyPCA()
#' }

runShinyPCA <- function() {
  options(shiny.maxRequestSize=8000*1024^2) # sets the max file to load to 100Mb
  
  # Define UI for application that draws a PCA
  ui <- fluidPage(
    
    # Application title
    titlePanel("Principal Component Analysis (PCA)"),
    
    # Sidebar with options
    sidebarLayout(
      sidebarPanel(
        # input file: accepts tab-separated text files
        fileInput("file_pca", "Upload expression matrix", accept = c("text/txt", "text/csv", "text/tab")),
        # input file: sample table that will tell what groups samples belong to
        fluidRow(column(9, fileInput("sample_table", "(optional) Upload sample metadata file", accept = c("text/txt", "text/csv", "text/tab"))),
                 column(3, bsButton("q1", label = "", icon = icon("question"), 
                                    style = "success", size = "extra-small", block=T))),
        # text that appears when the user clicks on the "bsButton" question mark
        bsPopover(id = "q1", title = "Metadata file:",
                  content = paste0("contains columns with samples metadata: the first column must correspond to the first row of the input file!"),
                  placement = "right", 
                  trigger = "click", 
                  options = list(container = "body")),
        # once the input file is loaded, the user can choose which PC to draw on each axis
        uiOutput("pca_columns1"),
        uiOutput("pca_columns2"),
        # also the potential grouping and naming columns from the sample table appear
        uiOutput("sample_grouping_factor"),
        uiOutput("sample_point_labels"),
        uiOutput("sample_point_shapes"),
        #      uiOutput("pca_samples_check"),
        # Plot title
        textInput("pca_title", label = h6("Plot title"), value = "Principal Component Analysis"),
        # Sliders for title, x-axis and y-axis text sizes.
        sliderInput("pca_size_title",
                    h6("Title font size:"),
                    min = 12, max = 60,
                    value = 12, step=2),
        sliderInput("pca_size_xlab",
                    h6("x-axis font size:"),
                    min = 10, max = 40,
                    value = 10, step=2),
        sliderInput("pca_size_ylab",
                    h6("y-axis font size:"),
                    min = 10, max = 40,
                    value = 10, step=2),
        # size of point labels
        numericInput("label_size", label = h6("Point label size:"), 
                     value = 3, 
                     min=0.5, 
                     max=20, 
                     step=0.5)
      )
      ,
      # Panel for the PCA plot itself + an additional tab to select the samples
      mainPanel(
        uiOutput("allsamples")
      )
    )
  )
  
  
  
  # Define server logic required to draw a PCA plot
  server <- function(input, output, session) {
    
    input_pca <- reactive({
      # Reactive function that detects and reads in the input file
      inFile <- input$file_pca
      if (is.null(inFile))
        return(NULL)
      #pca1 <- read.table(inFile$datapath, sep="\t", header=T, as.is = T)
      pca1 <- fread(inFile$datapath, sep="\t", header=T)
      # select numeric columns only (others are supposed to be annotation etc.)
      dplyr::select_if(pca1, is.numeric)
    })
    # enable button that will draw the PCA 
    shinyjs::enable("doPCA")
    
    input_sampletable <- reactive({
      # Reactive function that detects and reads in the input file
      inFile <- input$sample_table
      if (is.null(inFile))
        return(NULL)
      utils::read.table(inFile$datapath, sep="\t", header=T, as.is=T)
    })
    
    observe({
      if(!is.null(input$file_pca)){
        
        output$pca_columns1 <- renderUI({
          numericInput("pca_col1", 
                       label = h6("Principal component on x-axis:"), 
                       value = 1,
                       min=1, 
                       max=10, 
                       step=1)
        })
        
        output$pca_columns2 <- renderUI({
          numericInput("pca_col2", 
                       label = h6("Principal component on y-axis:"), 
                       value = 2,
                       min=1,
                       max=10,
                       step=1)
        })
        
        output$allsamples <- renderUI({
          tabsetPanel(
            tabPanel("Plot",
                     # Action button that will trigger the plot to be drawn after the parameters are set
                     actionBttn("doPCA", "Display PCA plot", icon("bar-chart-o"), style = "jelly", size = "sm"),
                     # Plot PCA
                     plotOutput("pcaPlot"),
                     # Conditional panel: if the user clicked on "display PCA plot", the download button appears
                     # ! Working properly only on Google Chrome!!
                     conditionalPanel(
                       condition = "input.doPCA > 0",
                       fluidRow(column(3,
                                       textInput("name_png", 
                                                 label = h6("Enter file name:"), 
                                                 value = "")),
                                column(3, numericInput("width_png", 
                                                       label = h6("PNG file width:"), 
                                                       value = 700, 
                                                       min=480, 
                                                       max=4000, 
                                                       step=20)),
                                column(3, numericInput("height_png", 
                                                       label = h6("PNG file height:"), 
                                                       value = 500, 
                                                       min=480, 
                                                       max=4000, 
                                                       step=20)),
                                column(3, downloadBttn("downloadPCA", 
                                                       "Save PCA plot", 
                                                       size = "sm", 
                                                       style = "jelly"))
                       )
                     )
            ),
            tabPanel("Selected samples", 
                     checkboxGroupInput("sampleschecks", 
                                        label=h6("Samples selected: unselect and refresh to update the plot"), 
                                        choices=colnames(input_pca()), 
                                        selected=colnames(input_pca()))
            )
          )
        })
        
      }
      # review here: warning
      if(!is.null(input$sample_table)){
        if(any(input_sampletable()[,1] %in% as.character(colnames(input_pca())))){
          output$sample_grouping_factor <- renderUI({
            selectInput("group_color", label = h6("Colour points by:"),
                        choices =  c("no_colour_by", colnames(input_sampletable())[-1]), 
                        selected = 1)
          })
          output$sample_point_labels <- renderUI({
            selectInput("point_labels", label = h6("Name points by:"),
                        choices =  c("default_label","no_label", colnames(input_sampletable())), 
                        selected = 1)
          })
          
          output$sample_point_shapes <- renderUI({
            selectInput("point_shapes", label = h6("Shape points by:"),
                        choices =  c("no_shape_by", colnames(input_sampletable())), 
                        selected = 1)
          })
          
        }else{
          print("Please review the sample table file: the first column must correspond to the first row of the expression table!")
        }
        
      }
    }) # end of observe
    
    
    #output$pcaPlot <- renderPlot({
    drawPCA <- reactive({
      # if no input file, do not output anything
      if (is.null(input$file_pca))
        return(NULL)
      #input$doPCA
      # retrieve data frame from input file
      pca2 <- input_pca()
      # Select samples, groupings, and changed sample names
      pca_samples_select <- input$sampleschecks
      pca_grouping <- input$sampleschecks
      pca_sample_names <- input$sampleschecks
      pca3 <- pca2[,..pca_samples_select]
      
      pca_sample_names1 <- pca_sample_names[pca_samples_select]
      colnames(pca3) <- pca_sample_names1
      
      # Performs principal component analysis on data
      pca <- stats::prcomp(t(pca3))
      # Retrieve the percentages of variation for each component
      percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
      
      # Create data frame with principal components (elected or default) and default sample names.
      df_pca0 <- data.frame(PC1=pca$x[,input$pca_col1], 
                            PC2=pca$x[,input$pca_col2], 
                            sample=colnames(pca3))
      
      # if there is a sample table file, join with expression file and then choose:
      # 1. grouping by color 
      # 2. labels 
      # 3. point shape  
      if(!is.null(input$sample_table)){
        # extract selected samples from sample table
        pca_grouping_colors <- input_sampletable()[input_sampletable()[,1] %in% input$sampleschecks, ]
        # merge data frame with sample table
        df_pca <- merge(df_pca0, pca_grouping_colors, by.x="sample", by.y=1, all=T)
        
        # by default, sample names are labels
        if(input$point_labels != "default_label" & input$point_labels != "no_label"){
          # remove "names" column to replace it by user chosen column for labels
          df_pca <- df_pca[,-grep("^sample$", colnames(df_pca))]
          colnames(df_pca)[grep(paste0("^",input$point_labels,"$"), colnames(df_pca))] <- "name_points"
          #if "no_label" is specified, create an empty column as labels
        }else if(input$point_labels == "no_label"){
          df_pca$name_points <- ""
        }else{
          colnames(df_pca)[grep("^sample$", colnames(df_pca))] <- "name_points"
        }
        # depending on what parameters where set, build ggplot base layer
        if(input$point_shapes == "no_shape_by" & input$group_color == "no_colour_by"){
          p_pca <- ggplot(data=df_pca, aes_string(x="PC1", y="PC2", label="name_points"))
        }else if(input$point_shapes != "no_shape_by" & input$group_color == "no_colour_by"){
          p_pca <- ggplot(data=df_pca, aes_string(x="PC1", y="PC2", label="name_points", shape=input$point_shapes))
        }else if(input$point_shapes == "no_shape_by" & input$group_color != "no_colour_by"){
          p_pca <- ggplot(data=df_pca, aes_string(x="PC1", y="PC2", label="name_points", color=input$group_color))
        }else if(input$point_shapes != "no_shape_by" & input$group_color != "no_colour_by"){
          p_pca <- ggplot(data=df_pca, aes_string(x="PC1", y="PC2", label="name_points", color=input$group_color, shape=input$point_shapes))
        }
      }else{ # if there is no sample table at all
        df_pca <- df_pca0
        p_pca <- ggplot(data=df_pca, aes(x=PC1, y=PC2, label=sample))
      }
      # set x limits to allow space for the labels (pending letting users modify it)
      xlimits <- c(min(df_pca$PC1)*1.2, max(df_pca$PC1)*2.5)
      
      # plot PCA
      p_pca1 <- p_pca + geom_point(size=3) +
        theme_bw() +
        xlab(paste0("PC:",input$pca_col1," ", round(percentVar[input$pca_col1] * 100),"% variance")) +
        ylab(paste0("PC:",input$pca_col2," ", round(percentVar[input$pca_col2] * 100),"% variance")) +
        ggtitle(input$pca_title) +
        geom_text(size=input$label_size, hjust=-0.1) +
        xlim(xlimits) +
        theme(plot.title = element_text(size = input$pca_size_title, face = "bold", hjust=0.5), # size of PCA plot title
              axis.title.x=element_text(size=input$pca_size_xlab), # size of x-axis label
              axis.title.y=element_text(size=input$pca_size_ylab)
              #            legend.position="none"
        )
      # if point shapes are assigned, add more point shape options
      if(!is.null(input$sample_table)){
        if(input$point_shapes != "no_shape_by"){
          p_pca1 + scale_shape_manual(values=1:length(unique(df_pca[,input$point_shapes])))
        }else{
          p_pca1
        }
      }else{
        p_pca1
      }
      
    })
    
    
    observeEvent(input$doPCA, {
      
      # If the user clicks on the "Display Vocano" button, draw pca plot
      output$pcaPlot <- renderPlot({
        # isolate so that the plot is not updated until the "Refresh plot" button is pressed again...
        isolate({drawPCA()})
      })
      
      # The option to export PCA plot appears at the same time as the plot itself
      output$downloadPCA <- downloadHandler(
        filename = function(){
          paste0(Sys.Date(), "_", gsub(" ", "_", input$name_png), "_PCA.png")
        },
        content = function(file){
          grDevices::png(file, width=input$width_png, height=input$height_png) # picking up correct file name in Chrome only...
          p <- drawPCA()
          print(p)
          grDevices::dev.off()
        }
      )
      # Update "doPCA" button label
      updateActionButton(session, "doPCA",
                         label = "Refresh plot", 
                         icon("bar-chart-o"))
      
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  #runApp(launch.browser=TRUE)
}
