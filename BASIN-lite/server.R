# Program: BASINlite Version 1.0 (server.R)
# Authors: Evgeni Radichev, Tim Hartman
# Date last modified: 6 February 2021
# Description: This program takes a user's 2D, color images as an initial input
# and reads them. It then creates a CSV file that the user can fill in to
# describe the images. The user uploads the file, and the program processes the
# images and CSV file to give the user information and hypothesis test results
# related to those images. The user may then download a report of these results.
library(rmarkdown)
options(tinytex.verbose = TRUE)

# finds the location of the BASIN directory regardless of the working directory
this_file = gsub("--file=", "", commandArgs()[grepl("--file", commandArgs())])
if (length(this_file) > 0){
  wd <- paste(head(strsplit(this_file, '[/|\\]')[[1]], -1), collapse = .Platform$file.sep)
}else{
  wd <- dirname(rstudioapi::getSourceEditorContext()$path)
}

basinDir <- wd                                                             # Allows the folder containing this script to be the default working
reportsDir <- getwd()                                                      # directory until the user chooses his or her own directory path

# ensures tkwidgets show up in front-most GUI window
tkraise(tktoplevel())

#######################################
# PRE-DEFINED FUNCTIONS AND VARIABLES #
#######################################

# function that computes the net intensity difference and object count difference across experimental groups
# assumes that the input data is one of the metadata tables used to construct the image metadata table (values$barplotData)
intensityAndCountDifference <- function(data){
  data <- data[order(data$experiment),]
  control <- data[data$condition == "control",]
  test <- data[data$condition == "test",]
  netIntensityDiff <- vaggregate(control$sumImgIntensity, control$experiment, mean) - vaggregate(test$sumImgIntensity, test$experiment, mean)
  objCountDiff <- vaggregate(control$objCount, control$experiment, mean) - vaggregate(test$objCount, test$experiment, mean)
  stainInfo <- vapply(split(data$stain, data$experiment), function(x){x[[1]]}, character(1))
  outTable <- data.frame(unique(data$experiment),stainInfo,netIntensityDiff,objCountDiff)
  names(outTable) <- c("experiment", "stain", "Net Image Intensity Difference", "Object Count Difference")
  return(outTable)
}

tTestToDataFrame <- function(results, frame, altHypo){                          # Defined function to properly construct data frame of t-test results
  if(!is.na(results[[1]])){
    data.frame(
      "stain" = frame, "Ha" = altHypo, "t-statistic" = results$statistic,
      "df" = results$parameter, "p-value" = results$p.value, 
      "lower conf int" = results$conf.int[1], 
      "upper conf int" = results$conf.int[2], 
      "conf level" = attributes(results$conf.int)$conf.level, 
      method = results$method, check.names = FALSE)
  } else {
    data.frame(
      "stain" = frame, "Ha" = "not tested", "t-statistic" = NA, 
      "df" = NA, "p-value" = NA, 
      "lower conf int" = NA, "upper conf int" = NA, 
      "conf level" = NA, method = NA, check.names = FALSE)
  }
}

colExtract <- function(x, column){                                              # Function to extract the given column from feature data
  tryCatch({
    # as.vector(x[,column])
    as.vector(x[column])
  },
  error = function(cond){
    x <- data.frame(b.mean = 0, b.sd = 0, b.mad = 0, s.area = 0)
    # return(x[,column])
    return(x[column])
  })
}

# Translates alternative hypothesis from simple symbols in the analysis table
alternative <- function(x){
  alt <- x$alternative
  if(all(is.na(alt))){return(NA)}
  if(any(stri_cmp_eq(alt,"not="))){
    return("two.sided")
  } else if(any(stri_cmp_eq(alt,"<"))){
    return("less")
  } else if(any(stri_cmp_eq(alt,">"))){
    return("greater")
  } else {
    return(NA)
  }
}

shinyServer(function(input, output, session) {                                  # Defines the server-side logic of the Shiny application.
  
  values <- reactiveValues()                                                    # Creates an object for storing reactive values. Variables stored in  
                                                                                # 'values' can be used across 'observe' and 'reactive' functions
  #################################
  # VISUAL & NAVIGATION FUNCTIONS #
  #################################
  
  hideTab(inputId = "modulePanel", target = "Extractor", session)               # Hides 'Extractor' navbar link so user cannot go to it too soon
  hideTab(inputId = "modulePanel", target = "Viewer", session)                  # Hides 'Viewer' navbar link so user cannot go to it too soon
  hideTab(inputId = "modulePanel", target = "Reporter", session)                # Hides 'Reporter' navbar link so user cannot go to it too soon
  hideElement(id = "objectAreaThresholding")
  hideElement(id = "previewBoxplots")                                           # Hides 'Boxplots & Summary' button in Extractor to avoid confusion
  hideElement(id = "statResults")                                               # Hides 'Analysis Results' button in Extractor to avoid confusion
  hideElement(id = "gotoViewer")                                                # Hides 'Go To Viewer' button in Extractor to avoid confusion
  hideElement(id = "removeGraphOutliers")
  
  output$authorsUsed <- renderValueBox({                                        # These value boxes will show continuously-updated numbers of how 
    valueBox("Authors", value = "##", icon = icon("pen"),                       # many authors have used BASIN, how many images have been analyzed, 
             color = "red")})                                                   # and how many reports have been published
  output$imagesAnlzd <- renderValueBox({
    valueBox("Images Analyzed", value = "##", icon = icon("images"), 
             color = "green")})
  output$reportsPublsd <- renderValueBox({
    valueBox("Reports Published", value = "##", icon = icon("newspaper"), 
             color = "blue")})
  
  values$projDir <- reportsDir                                                  # Sets the initial working directory to the folder containing this script
  observeEvent(input$chooseProjDir, {                                           # Start New Project Button: user-set project directory
    values$projDir <- tclvalue(tkchooseDirectory())                             # Opens a window to choose BASIN session folder destination
    if(values$projDir != ""){                                                   # Check for non-empty directory
      updateTabsetPanel(session, "modulePanel", selected = "Upload")
    }
  })
  
  observeEvent(input$getOntoBIDSdata,                                           # Opens a popup window if 'Get OntoBIDS Data' button is clicked
    alert("Feature coming once OntoBIDS has been populated and published"))
  
  observeEvent(input$tryBASINcloud,                                           # Opens a popup window if 'Get OntoBIDS Data' button is clicked
               alert("Access to tryBASIN coming soon!"))
  
  observeEvent(input$gotoUpload, {                                              
    updateTabsetPanel(session, "modulePanel", selected = "Upload")              # Moves user to 'Upload' page once 'GO!' button is pressed
  })
  
  ###################
  # 'UPLOAD' MODULE #
  ###################
  
  observeEvent(input$closestatResults,                                          # This is for a custom 'Save & Close' button in the Stat Design popup.
  toggleModal(session, modalId = "statResultsPopup", toggle = "close")          # It assures the user that any changes within the popup will be saved.
  )
  
  observeEvent(input$confirmViewerBttn,{                                        # Once user clicks "Go To Viewer" button...
    showTab(                                                                    # Shows 'Reporter' navbar link at the top of the app
      inputId = "modulePanel", target = "Reporter", select = FALSE, session)
    updateTabsetPanel(session, "modulePanel", selected = "Reporter")            # Moves user to Reporter page automatically
  })

  observeEvent(input$uploadImages,{                                             # Observer event for multiple image upload
    unfiltered_files <- as.vector(
      stri_extract_all(
        input$uploadImages$name, regex = ".*jpg|.*png|.*tif", simplify = TRUE))
    values$img.files <- input$uploadImages$name[!is.na(unfiltered_files)]       # Filter out images of accepted format
    values$img.paths <- input$uploadImages$datapath[!is.na(unfiltered_files)]
  })
  
  output$statDesignUpload <- renderUI({                                         # UI for csv upload - stat design and analysis
    validate(need(values$img.files,""))                                         # Prevents UI from showing user a confusing, red error message
      bsCollapse(open = "Statistical Test Design", 
        bsCollapsePanel(
          style = "primary", title = "Statistical Test Design",
          h4("(Optional) Enter names for your red, green, and blue frames to change default visual display:"),
          splitLayout(                       
            textInput(                                                          # These textInput boxes allow the user to customize the RGB frame names for their convenience
              inputId="imgsRedStain", label="Red stain:", value="Red"),         
            textInput(
              inputId="imgsGreenStain", label="Green stain:", value="Green"),
            textInput(
              inputId="imgsBlueStain", label="Blue stain:", value="Blue")
          ),
          h4("(Required) Get and upload statistical test design table:"),
          div(style = "display: inline-block; vertical-align: center;",
          actionButton(inputId = "downloadAnalysisTable",                       # Opens a window allowing user to choose folder in which to download CSV file
                       label = "Get Stat Design Template")),
          div(
            style = "display: inline-block; vertical-align: center;",
            conditionalPanel(condition = "input.downloadAnalysisTable",
                             icon("fas fa-check"))),                            # This checkmark is sort of a way to let the user know that the app did something
          fluidRow(
            column(width = 9, fileInput(
              inputId = "uploadAnalysisTable", label = "", multiple = FALSE,    # Stat analysis CSV file input
              buttonLabel = "Upload Stat Design CSV",
              placeholder = "Drag and drop here", width = "100%")
            ),
            column(width = 3, div(
              style="padding: 20px;",                                           # Aligns action button vertically with fileInput
              disabled(actionButton("gotoExtractor", label = "Confirm Upload")))
            )
          ), # end fluidRow
          h4("Statistical Design Table Snapshot"),
          tableOutput(outputId = "analysisTable")
          #DTOutput(outputId = "analysisTable", width = "100%"),
        )                                                                       # end bsCollapsePanel
      )                                                                         # end bsCollapse
  })                                                                            # end renderUI 'statDesignUpload'
  
  observeEvent(input$downloadAnalysisTable, {                                   # Stat Design Table Download
    df <- data.frame(
      "filename" = req(values$img.files), "stain" = "", "experiment" = NA, 
      "biocondition" = NA, "alternative" = "not=", "color frame" = "red")
    values$analysisTable <- cbind(id = rownames(df), df)
    #tkraise(tktoplevel())
    wd <- tclvalue(tkchooseDirectory())
    if(dir.exists(wd)){
      write.csv(
        values$analysisTable, file = file.path(wd,"analysisTable.csv"), 
        row.names = FALSE)
    }
  })

  observeEvent(input$uploadAnalysisTable, {                                     # Table Upload
    output$analysisTable <- renderTable(expr = {
      df <- input$uploadAnalysisTable
      if (is.null(df))
        return(NULL)
      analysisTable <- read.csv(df$datapath, stringsAsFactors = FALSE)
      if(any(is.na(analysisTable$'color.frame'))){
        # any NA color frame values are set automatically to red frame
        analysisTable$'color.frame'[is.na(analysisTable$'color.frame')] <- "red"
      }
      if(any(is.na(analysisTable$'stain'))){
        # any NA stains are set to blank
        analysisTable$'stain'[is.na(analysisTable$'stain')] <- ""
      }
      checkInput <-  values$img.files %in% analysisTable$filename               # check for correctness of csv table input using filenames
      if(!all(checkInput)){
        disable(id = "gotoExtractor")
        stop("Improper stat design CSV uploaded. 
             Please check your file and upload again")
      }
      enable(id = "gotoExtractor")                                              # Display confirmation button for Upload module 
      ordering <- match(values$img.files, analysisTable$filename)
      analysisTable <- analysisTable[ordering,]
      # fetch color-coding for stains
      redStain <- rep('Red Frame', length(analysisTable$filename))
      greenStain <- rep('Green Frame', length(analysisTable$filename))
      blueStain <- rep('Blue Frame', length(analysisTable$filename))
      if(!is.null(analysisTable$'color.frame')){
        red <- analysisTable$'color.frame' == 'red' & analysisTable$'stain' != ""
        green <- analysisTable$'color.frame' == 'green' & analysisTable$'stain' != ""
        blue <- analysisTable$'color.frame' == 'blue' & analysisTable$'stain' != ""
        redStain[red] <- as.vector(analysisTable$'stain'[red])
        greenStain[green] <- as.vector(analysisTable$'stain'[green])
        blueStain[blue] <- as.vector(analysisTable$'stain'[blue])
      }
      values$redStain <- redStain
      values$greenStain <- greenStain
      values$blueStain <- blueStain
      # color-coding for graphs
      reds <- unique(redStain)
      greens <- unique(greenStain)
      blues <- unique(blueStain)
      colorCode <- c(rep("salmon",length(reds)), rep("green3",length(greens)),rep("cornflower blue",length(blues)))
      names(colorCode) <- c(reds, greens, blues)
      values$colorCode <- colorCode
      # reactive table for access anywhere in app
      values$analysisTable <- analysisTable
      # display table
      analysisTable[1:6,c("filename","stain","experiment","biocondition","alternative","color.frame")]
    }
    ) 
  })
  
  output$imgsRedStain <- renderText(input$imgsRedStain)                         # Stain info for user's convenience
  output$imgsGreenStain <- renderText(input$imgsGreenStain)
  output$imgsBlueStain <- renderText(input$imgsBlueStain)
  
  observeEvent(input$gotoExtractor, {                                           # Create output folder and Log file for session
    values$sessionID <- format.POSIXct(Sys.time(), format = "%m-%d-%Y-%H%M%S")
    if(input$chooseProjDir != 0){
      outputName <- paste0(values$projDir,"/basinOutput",values$sessionID)
      dir.create(outputName)
      setwd(outputName)
      if(!dir.exists("Upload")){
        dir.create("Upload")
        dir.create(file.path("Upload","Images"))
      }
      # save a compressed copy of all input images to the output folder
      imgs <- lapply(values$img.paths, readImage)
      for(i in seq_along(imgs)){
        writeImage(imgs[[i]], files = file.path("Upload","Images",values$img.files[i]), type = "jpeg", quality = 70)
      }
      
      file.copy(
        req(input$uploadAnalysisTable$datapath),                                # copy stat analysis data to output folder
        paste0("Upload","/",input$uploadAnalysisTable$name))
      file.create("Upload/Log.txt")                                             # create Log file for this session
      log <- file("Upload/Log.txt", open = "a+")
      analysisInfo <-  paste0(
        "data file location: ", paste0(outputName,"/Upload"))
      writeLines("UPLOAD", con = log)
      writeLines(values$img.files, con = log, sep = ",")
      writeLines(analysisInfo, con = log, sep = "")
      writeLines("\n", con = log)
      close(log)
    }
    values$threshFail <- FALSE
    showTab(
      inputId = "modulePanel", target = "Extractor", select = FALSE, session)   # take user to next module
    updateTabsetPanel(session, "modulePanel", selected = "Extractor")
  })
  
  ######################
  # 'EXTRACTOR' MODULE #
  ######################
  
  observeEvent({input$confirmThresh}, {
    imgs <- lapply(values$img.paths, readImage)                                 # Extract image data, separate into RGB frames
    # modified function to include grayscale images (1/4/21)
    values$imgs <- lapply(imgs, function(x){
      if(numberOfFrames(x) == 1){
        toRGB(x)
      } else {
        x[,,1:3]
      }
    })                          # Extract RGB frames only
    names(values$imgs) <- values$img.files
    values$imgs.r <- lapply(values$imgs, function(x) getFrame(x, 1))
    values$imgs.g <- lapply(values$imgs, function(x) getFrame(x, 2))
    values$imgs.b <- lapply(values$imgs, function(x) getFrame(x, 3))
    names(values$imgs.r) <- values$img.files
    names(values$imgs.g) <- values$img.files
    names(values$imgs.b) <- values$img.files

    withProgress(message = 'Image Pre-processing', value = 0, {       # Visual Updates for 
      values$threshFail <- FALSE
      incProgress(0.25,detail=paste("Thresholding"))
      # thresholding with various masking algorithms
      autothreshold <- function(x) {
        y <- x*(2^16-1)
        if(!all(y == 0)){
          tryCatch({
            # compute threshold mask using user-selected method
            z <- mask(y, method = input$thresh.auto)
            x <- x*z
          }, error = function(cond){
            values$threshFail <- TRUE
            z <- mask(y, method = "Otsu")
            x <- x*z
          })
        }
        return(x)
      }
      values$imgs.r.thresholded <- lapply(values$imgs.r, autothreshold)
      values$imgs.g.thresholded <- lapply(values$imgs.g, autothreshold)
      values$imgs.b.thresholded <- lapply(values$imgs.b, autothreshold)
      
      incProgress(0.5,detail=paste("Labeling"))
      
      values$imgs.r.label <- lapply(values$imgs.r.thresholded, bwlabel)
      values$imgs.g.label <- lapply(values$imgs.g.thresholded, bwlabel)
      values$imgs.b.label <- lapply(values$imgs.b.thresholded, bwlabel)
      
      names(values$imgs.r.label) <- values$img.files
      names(values$imgs.g.label) <- values$img.files
      names(values$imgs.b.label) <- values$img.files
      
      values$imgs.r.clrlabel <- lapply(values$imgs.r.label, colorLabels)
      values$imgs.g.clrlabel <- lapply(values$imgs.g.label, colorLabels)
      values$imgs.b.clrlabel <- lapply(values$imgs.b.label, colorLabels)
      
      incProgress(0.75,detail=paste("Re-Coloring Objects"))

      values$imgs.r.pntd <- mapply(function(x,tgt){
        paintObjects(x, EBImage::channel(tgt, "asred"), col = "yellow")
      }, x = values$imgs.r.label, tgt = values$imgs.r, SIMPLIFY = FALSE)
      values$imgs.g.pntd <- mapply(function(x,tgt){
        paintObjects(x, EBImage::channel(tgt, "asgreen"), col = "yellow")
      }, x = values$imgs.g.label, tgt = values$imgs.g, SIMPLIFY = FALSE)
      values$imgs.b.pntd <- mapply(function(x,tgt){
        paintObjects(x,EBImage::channel(tgt, "asblue"),col = "yellow")
      }, x = values$imgs.b.label, tgt = values$imgs.b, SIMPLIFY = FALSE)
      
      incProgress(1.0,detail=paste("Moving to Feature Extraction"))
    }) # end withProgress

    # Function to safely extract feature-data from images
    featureExtract <- function(labeled, reference, stain){                      
      labeledNames <- names(labeled)                                          
      fileCount <- length(labeled)
      counter <- 0
      withProgress(message = 'Getting features', value = 0, {                   # Displays a progress window at the bottom right of the app letting the user know what's going on
      mapply(function(l, r, s) {
        tryCatch({
          counter <<- counter + 1
          basicFeatures <- computeFeatures.basic(x = l, ref = r)
          momentFeatures <- computeFeatures.moment(x = l, ref = r)
          shapeFeatures <- computeFeatures.shape(x = l)
          features <- data.frame(
            experiment = values$analysisTable$experiment[counter],
            biocondition = values$analysisTable$biocondition[counter],
            stain = s,
            basicFeatures,
            momentFeatures,
            shapeFeatures, stringsAsFactors = FALSE)
          incProgress(1/fileCount, 
                      detail=paste("Image", counter, "of", fileCount))        # For withProgress: updates the bar every time a loop completes itself
          return(features)
        },
        # reapply image morphology functions to problem image
        error = function(cond){                                               
          imgThresh <- autothreshold(r)
          imgLabel <- bwlabel(imgThresh)
          basicFeatures <- computeFeatures.basic(imgLabel, ref = r)
          # check for null features (blank images/frames)
          if(is.null(basicFeatures)){
            basicFeatures <- data.frame(b.mean = 0, b.sd = 0, b.mad = 0, b.q001=0, b.q005=0, b.q05=0, b.q095=0, b.q099=0)
            momentFeatures <- data.frame(m.cx=0, m.cy=0, m.majoraxis=0, m.eccentricity=0, m.theta=0)
            shapeFeatures <- data.frame(s.area=0, s.perimeter=0, s.radius.mean=0, s.radius.sd=0, s.radius.min=0, s.radius.max=0)
          } else {
            momentFeatures <- computeFeatures.moment(imgLabel, ref = r)
            shapeFeatures <- computeFeatures.shape(imgLabel)
          }
          features <- data.frame(
            experiment = values$analysisTable$experiment[counter],
            biocondition = values$analysisTable$biocondition[counter],
            stain = s,
            basicFeatures,
            momentFeatures,
            shapeFeatures, stringsAsFactors = FALSE)
          incProgress(1/fileCount, 
                      detail=paste("Image", counter, "of", fileCount))        # For withProgress: updates the bar every time a loop completes itself
          return(features)
        })
      },
      l = labeled, r = reference, s = stain, SIMPLIFY = FALSE)
      })
    }
    
    # Evaluate Features for each image's frames
    features.r <- featureExtract(
      req(values$imgs.r.label), reference = values$imgs.r, 
      stain = values$redStain) #input$imgsRedStain)
    features.g <- featureExtract(
      req(values$imgs.g.label), reference = values$imgs.g, 
      stain = values$greenStain) #input$imgsGreenStain)
    features.b <- featureExtract(
      req(values$imgs.b.label), reference = values$imgs.b, 
      stain = values$blueStain) #input$imgsBlueStain)
    
    featuresDF.r <- ldply(req(features.r), .id = "filename")             # Convert list of feature data into data frame
    featuresDF.g <- ldply(req(features.g), .id = "filename")
    featuresDF.b <- ldply(req(features.b), .id = "filename")
    
    featuresDF.r$biocondition <- paste0(featuresDF.r$biocondition,".r")
    featuresDF.g$biocondition <- paste0(featuresDF.g$biocondition,".g")
    featuresDF.b$biocondition <- paste0(featuresDF.b$biocondition,".b")
    
    values$featuresDF.all <- rbind(featuresDF.r,featuresDF.g,featuresDF.b)
    values$maxObjectArea <- max(values$featuresDF.all$s.area)
    
    # create reactive features that can be used by other code blocks
    values$featuresRaw.r <- features.r
    values$featuresRaw.g <- features.g
    values$featuresRaw.b <- features.b
    showElement(id = "objectAreaThresholding")
    showElement(id = "previewBoxplots")                                         # Allows user to see 'Boxplots & Summary' modal
    showElement(id = "statResults")                                             # Allows user to see 'Analysis Results' modal
    showElement(id = "removeGraphOutliers")
  })                                                                            # end observeEvent(input$confirmThresh)
  
  # update feature data after changes in object area range
  observeEvent(input$minMaxObjectArea,{
    minArea <- input$minMaxObjectArea[1]
    maxArea <- input$minMaxObjectArea[2]
    
    features.r.filtered <- lapply(values$featuresRaw.r, function(x){
      x_new <- subset.data.frame(x, s.area > minArea & s.area < maxArea)
      if(nrow(x_new) == 0){
        x_new <- x_new[1,]
        x_new[1,4:ncol(x_new)] <- 0
      }
      return(x_new)
    })
    features.g.filtered <- lapply(values$featuresRaw.g, function(x){
      x_new <- subset.data.frame(x, s.area > minArea & s.area < maxArea)
      if(nrow(x_new) == 0){
        x_new <- x_new[1,]
        x_new[1,4:ncol(x_new)] <- 0
      }
      return(x_new)
    })
    features.b.filtered <- lapply(values$featuresRaw.b, function(x){
      x_new <- subset.data.frame(x, s.area > minArea & s.area < maxArea)
      if(nrow(x_new) == 0){
        x_new <- x_new[1,]
        x_new[1,4:ncol(x_new)] <- 0
      }
      return(x_new)
    })
    
    featuresDF.r <- ldply(req(features.r.filtered), .id = "filename")             # Convert list of feature data into data frame
    featuresDF.g <- ldply(req(features.g.filtered), .id = "filename")
    featuresDF.b <- ldply(req(features.b.filtered), .id = "filename")
    
    featuresDF.r$biocondition <- paste0(featuresDF.r$biocondition,".r")
    featuresDF.g$biocondition <- paste0(featuresDF.g$biocondition,".g")
    featuresDF.b$biocondition <- paste0(featuresDF.b$biocondition,".b")
    
    values$featuresDF.all <- rbind(featuresDF.r,featuresDF.g,featuresDF.b)
    
    count.r <- lapply(features.r.filtered, function(x){
      counts <- nrow(x)
      if(counts == 1 && x$b.mean == 0){
        counts <- 0
      }
      return(counts)
    })                                  # Object count determined using number of rows
    count.g <- lapply(features.g.filtered, function(x){
      counts <- nrow(x)
      if(counts == 1 && x$b.mean == 0){
        counts <- 0
      }
      return(counts)
    })
    count.b <- lapply(features.b.filtered, function(x){
      counts <- nrow(x)
      if(counts == 1 && x$b.mean == 0){
        counts <- 0
      }
      return(counts)
    })
    
    count.r <- ldply(count.r, data.frame, .id = "biocondition")                 # Converts list into data frame for Raw Data table
    count.g <- ldply(count.g, data.frame, .id = "biocondition")
    count.b <- ldply(count.b, data.frame, .id = "biocondition")
    
    metadata.r <- data.frame(                                                   # Sum Intensity and Object Count Data
      row.names = NULL,
      filename = values$img.files,
      condition = values$analysisTable$biocondition,
      stain = values$redStain, #input$imgsRedStain,
      sumImgIntensity = vapply(
        values$imgs, function(x) {sum(imageData(x)[,,1])}, numeric(1)),
      objCount = count.r$X..i..,
      sumObjIntensity = vapply(
        values$imgs.r.thresholded, function(x) {sum(imageData(x))},numeric(1)),
      meanObjIntensity = vapply(
        features.r.filtered, function(x){ mean(x$b.mean) }, numeric(1)),
      sumObjArea = vapply(
        features.r.filtered, function(x){ sum(x$s.area) }, numeric(1)),
      meanObjArea = vapply(
        features.r.filtered, function(x){ mean(x$s.area) }, numeric(1)),
      biocondition = count.r$biocondition,
      experiment = values$analysisTable$experiment,
      stringsAsFactors = FALSE
    )
    
    metadata.g <- data.frame(
      row.names = NULL,
      filename = values$img.files,
      condition = values$analysisTable$biocondition,
      stain = values$greenStain, #input$imgsGreenStain,
      sumImgIntensity = vapply(
        values$imgs, function(x) {sum(imageData(x)[,,2])}, numeric(1)),
      objCount = count.g$X..i..,
      sumObjIntensity = vapply(
        values$imgs.g.thresholded, function(x) {sum(imageData(x))},numeric(1)),
      meanObjIntensity = vapply(
        features.g.filtered, function(x){ mean(x$b.mean) }, numeric(1)),
      sumObjArea = vapply(
        features.g.filtered, function(x){ sum(x$s.area) }, numeric(1)),
      meanObjArea = vapply(
        features.g.filtered, function(x){ mean(x$s.area) }, numeric(1)),
      biocondition = count.g$biocondition,
      experiment = values$analysisTable$experiment,
      stringsAsFactors = FALSE
    )
    
    metadata.b <- data.frame(
      row.names = NULL,
      filename = values$img.files,
      condition = values$analysisTable$biocondition,
      stain = values$blueStain, #input$imgsBlueStain,
      sumImgIntensity = vapply(
        values$imgs, function(x) {sum(imageData(x)[,,3])}, numeric(1)),
      objCount = count.b$X..i..,
      sumObjIntensity = vapply(
        values$imgs.b.thresholded, function(x) {sum(imageData(x))},numeric(1)),
      meanObjIntensity = vapply(
        features.b.filtered, function(x){ mean(x$b.mean) }, numeric(1)),
      sumObjArea = vapply(
        features.b.filtered, function(x){ sum(x$s.area) }, numeric(1)),
      meanObjArea = vapply(
        features.b.filtered, function(x){ mean(x$s.area) }, numeric(1)),
      biocondition = count.b$biocondition,
      experiment = values$analysisTable$experiment,
      stringsAsFactors = FALSE
    )
    
    format.metadata <- function(red,green,blue){                                # function to format metadata table
      output <- NULL
      for(i in 1:nrow(red)){
        output <- rbind(output,red[i,],green[i,],blue[i,])
      }
      return(output)
    }
    
    values$barplotData <- format.metadata(metadata.r,metadata.g,metadata.b)
    
    # format the differences table
    diff.r <- intensityAndCountDifference(metadata.r)
    diff.g <- intensityAndCountDifference(metadata.g)
    diff.b <- intensityAndCountDifference(metadata.b)
    differenceData <- rbind(diff.r, diff.g, diff.b)
    values$differenceData <- differenceData[order(differenceData$experiment),]
    
    # update reactive feature values
    values$features.r <- features.r.filtered
    values$features.g <- features.g.filtered
    values$features.b <- features.b.filtered
  })
  
  observeEvent(input$statResults,{                                              # Allow user to access Viewer only once they have checked the Stat Design
    showElement(id = "gotoViewer")
  })
  

  output$threshMethodResult <- renderText({
    if(values$threshFail){
      "WARNING: at least one image has failed to threshold using selected method and was processed using the 'Otsu' method instead."
    } else {
      " "
    }
  })
  
  observeEvent(input$statResults, {

    values$groupedExperimentTable <- split(                                     # Subset  by experiment
      x = req(values$analysisTable), f = values$analysisTable$experiment)
    
    # check for missing biocondition pairs
    flag <- TRUE
    for(experiment in values$groupedExperimentTable){
      if(!any("test" == experiment$biocondition) || !any("control" == experiment$biocondition)){
        flag <- FALSE
      }
    }
    validate(need(flag, message = "All experimental groups must have at least one 'control' and one 'test' image for analysis."))
    
    meanIntensities.r <- lapply(                                                # Preparing mean data
      values$features.r, function(x) colExtract(x, "b.mean"))
    meanIntensities.g <- lapply(
      values$features.g, function(x) colExtract(x, "b.mean"))
    meanIntensities.b <- lapply(
      values$features.b, function(x) colExtract(x, "b.mean"))
    
    meanData <- lapply(values$groupedExperimentTable, function(x){
      controlGroup <- x[(x$biocondition == "control"),]$filename
      testGroup <- x[(x$biocondition == "test"),]$filename
      controlGroup.r <- unlist(
        meanIntensities.r[controlGroup], use.names = FALSE)
      testGroup.r <- unlist(meanIntensities.r[testGroup], use.names = FALSE)
      controlGroup.g <- unlist(
        meanIntensities.g[controlGroup], use.names = FALSE)
      testGroup.g <- unlist(meanIntensities.g[testGroup], use.names = FALSE)
      controlGroup.b <- unlist(
        meanIntensities.b[controlGroup], use.names = FALSE)
      testGroup.b <- unlist(meanIntensities.b[testGroup], use.names = FALSE)
      
      altHypothesis <- alternative(x)
      
      # get the correct staining info
      red <- ifelse(any(x$'color.frame' == 'red' & x$'stain' != ""),
                    yes = as.vector(x$'stain'[x$'color.frame' == 'red'])[1],
                    no = 'Red Frame')
      green <- ifelse(any(x$'color.frame' == 'green' & x$'stain' != ""),
                      yes = as.vector(x$'stain'[x$'color.frame' == 'green'])[1],
                      no = 'Green Frame')
      blue <- ifelse(any(x$'color.frame' == 'blue' & x$'stain' != ""),
                     yes = as.vector(x$'stain'[x$'color.frame' == 'blue'])[1],
                     no = 'Blue Frame')
      safeTest <- possibly(function(group1, group2){
        t.test(group1,group2,alternative = altHypothesis)
      }, otherwise = NA)
      experiment.r.results <- safeTest(controlGroup.r, testGroup.r)
      experiment.g.results <- safeTest(controlGroup.g, testGroup.g)
      experiment.b.results <- safeTest(controlGroup.b, testGroup.b)
      
      # result <- rbind(                                                          # Bind results as one data frame
      #   redFrame = tTestToDataFrame(
      #     experiment.r.results,input$imgsRedStain, altHypothesis),
      #   greenFrame = tTestToDataFrame(
      #     experiment.g.results,input$imgsGreenStain, altHypothesis),
      #   blueFrame = tTestToDataFrame(
      #     experiment.b.results,input$imgsBlueStain, altHypothesis))
      # Bind results as one data frame
      redFrame = tTestToDataFrame(
        experiment.r.results,red, altHypothesis)
      redFrame['Mean Object Intensity Difference'] <- mean(controlGroup.r) - mean(testGroup.r)
      greenFrame = tTestToDataFrame(
        experiment.g.results,green, altHypothesis)
      greenFrame['Mean Object Intensity Difference'] <- mean(controlGroup.g) - mean(testGroup.g)
      blueFrame = tTestToDataFrame(
        experiment.b.results,blue, altHypothesis)
      blueFrame['Mean Object Intensity Difference'] <- mean(controlGroup.b) - mean(testGroup.b)
      result <- rbind(                                                          
        redFrame = redFrame,
        greenFrame = greenFrame,
        blueFrame = blueFrame)
      

      data <- rbind(                                                            # T-test data pre-processing for plotting
        data.frame("mean" = controlGroup.r,stain = red, 
                   biocondition = "control.r"),
        data.frame("mean" = testGroup.r,stain = red,
                   biocondition = "test.r"),
        data.frame("mean" = controlGroup.g,stain = green, 
                   biocondition = "control.g"),
        data.frame("mean" = testGroup.g,stain = green, 
                   biocondition = "test.g"),
        data.frame("mean" = controlGroup.b,stain = blue, 
                   biocondition = "control.b"),
        data.frame("mean" = testGroup.b,stain = blue, 
                   biocondition = "test.b"))
      
      list(data = data ,result = result)                                        # Return list of t-test control group data, test group data, and t-test results
    })
    
    meanObjectData <- lapply(meanData, function(x){x$data})
    values$objectMeanData <- ldply(meanObjectData, .id = "experiment")
    
    meanResults <- lapply(meanData, function(x){x$result})
    values$objectMeanResults <- ldply(meanResults, .id = "experiment")
    
    areas.r <- lapply(values$features.r, function(x) colExtract(x, "s.area"))   # t-test of object area data
    areas.g <- lapply(values$features.g, function(x) colExtract(x, "s.area"))
    areas.b <- lapply(values$features.b, function(x) colExtract(x, "s.area"))
    
    areaData <- lapply(values$groupedExperimentTable, function(x){
      controlGroup <- x[(x$biocondition == "control"),]$filename
      testGroup <- x[(x$biocondition == "test"),]$filename
      controlGroup.r <- unlist(areas.r[controlGroup], use.names = FALSE)
      testGroup.r <- unlist(areas.r[testGroup], use.names = FALSE)
      controlGroup.g <- unlist(areas.g[controlGroup], use.names = FALSE)
      testGroup.g <- unlist(areas.g[testGroup], use.names = FALSE)
      controlGroup.b <- unlist(areas.b[controlGroup], use.names = FALSE)
      testGroup.b <- unlist(areas.b[testGroup], use.names = FALSE)
      
      altHypothesis <- alternative(x)
      # get the correct staining info
      red <- ifelse(any(x$'color.frame' == 'red' & x$'stain' != ""),
                    yes = as.vector(x$'stain'[x$'color.frame' == 'red'])[1],
                    no = 'Red Frame')
      green <- ifelse(any(x$'color.frame' == 'green' & x$'stain' != ""),
                      yes = as.vector(x$'stain'[x$'color.frame' == 'green'])[1],
                      no = 'Green Frame')
      blue <- ifelse(any(x$'color.frame' == 'blue' & x$'stain' != ""),
                     yes = as.vector(x$'stain'[x$'color.frame' == 'blue'])[1],
                     no = 'Blue Frame')
      safeTest <- possibly(function(group1, group2){                            # T-test control vs test biocondition
        t.test(group1,group2,alternative = altHypothesis)
      }, otherwise = NA)
      experiment.r.results <- safeTest(controlGroup.r, testGroup.r)
      experiment.g.results <- safeTest(controlGroup.g, testGroup.g)
      experiment.b.results <- safeTest(controlGroup.b, testGroup.b)
       
      # Bind results as one data frame
      redFrame = tTestToDataFrame(
        experiment.r.results,red, altHypothesis)
      redFrame['Mean Object Area Difference'] <- mean(controlGroup.r) - mean(testGroup.r)
      greenFrame = tTestToDataFrame(
        experiment.g.results,green, altHypothesis)
      greenFrame['Mean Object Area Difference'] <- mean(controlGroup.g) - mean(testGroup.g)
      blueFrame = tTestToDataFrame(
        experiment.b.results,blue, altHypothesis)
      blueFrame['Mean Object Area Difference'] <- mean(controlGroup.b) - mean(testGroup.b)
      result <- rbind(                                                          
        redFrame = redFrame,
        greenFrame = greenFrame,
        blueFrame = blueFrame)
      
      # T-test data pre-processing for plotting
      data <- rbind(                                                            
        data.frame("area" = controlGroup.r,stain = red, 
                   biocondition = "control.r"),
        data.frame("area" = testGroup.r,stain = red,
                   biocondition = "test.r"),
        data.frame("area" = controlGroup.g,stain = green, 
                   biocondition = "control.g"),
        data.frame("area" = testGroup.g,stain = green, 
                   biocondition = "test.g"),
        data.frame("area" = controlGroup.b,stain = blue, 
                   biocondition = "control.b"),
        data.frame("area" = testGroup.b,stain = blue, 
                   biocondition = "test.b"))
      
      list(data = data, result = result)                                        # Return list of t-test control group data, test group data, and t-test results
    })
    objectAreaData = lapply(areaData, function(x){x$data})
    values$objectAreaData = ldply(objectAreaData, .id = "experiment")
    
    objectAreaResults = lapply(areaData, function(x){x$result})
    values$objectAreaResults <- ldply(objectAreaResults, .id = "experiment")
  })

  ##############################
  # EXTRACTOR GRAPHIC ELEMENTS #
  ##############################
  
  # slider for thresholding by object area
  output$objectAreaThresholding <- renderUI({
    maxArea <- max(values$maxObjectArea)
    sliderInput(
      inputId = "minMaxObjectArea", 
      label = div("Threshold Object Area Range (total pixels):"), 
      min = 0, 
      max = maxArea, 
      value = c(0, maxArea)
    )
  })
  
  output$conditionalImageInput <- renderUI({                                    # Radio button selection in for viewing individual images
      selectInput(
        inputId = "uploaded", label = "View Image:", 
        choices = req(values$img.files), width = "100%")
  })
  
  output$extractorImagePlot <- renderPlot({
    c <- values$img.files == req(input$uploaded)                                # Logical vector to select the radio button image
    selectedImg <- req(values$imgs)[c][[1]]                                     # Extract user-selected image data
    selectedImg.r <- EBImage::channel(values$imgs.r[c][[1]],"asred")                                 # use req() to ensure thresholding button has been clicked
    selectedImg.g <- EBImage::channel(values$imgs.g[c][[1]],"asgreen")
    selectedImg.b <- EBImage::channel(values$imgs.b[c][[1]],"asblue")
    selectedImg.r.th <- EBImage::channel(values$imgs.r.thresholded[c][[1]],"asred")
    selectedImg.g.th <- EBImage::channel(values$imgs.g.thresholded[c][[1]],"asgreen")
    selectedImg.b.th <- EBImage::channel(values$imgs.b.thresholded[c][[1]],"asblue")
    selectedImg.r.clbl <- values$imgs.r.clrlabel[c][[1]]
    selectedImg.g.clbl <- values$imgs.g.clrlabel[c][[1]]
    selectedImg.b.clbl <- values$imgs.b.clrlabel[c][[1]]
    selectedImg.r.pntd <- values$imgs.r.pntd[c][[1]]
    selectedImg.g.pntd <- values$imgs.g.pntd[c][[1]]
    selectedImg.b.pntd <- values$imgs.b.pntd[c][[1]]
    blankSpot <- Image(matrix("white",dim(selectedImg)[1],dim(selectedImg)[2])) # Blank canvas for formatting
    print("try combine")
    EBImage::display(                                                           # Formatted Image display
      EBImage::combine(
        blankSpot, selectedImg, blankSpot,
        selectedImg.r, selectedImg.g, selectedImg.b,
        selectedImg.r.th, selectedImg.g.th, selectedImg.b.th,
        selectedImg.r.clbl, selectedImg.g.clbl, selectedImg.b.clbl,
        selectedImg.r.pntd, selectedImg.g.pntd, selectedImg.b.pntd
      ), nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE
    )
    text(x = (10 + dim(selectedImg)[1] + dim(selectedImg)[1]/2), y = -10,       # Vertical Labels
         label = values$img.files[c][[1]], adj = c(0.5, 0), col="black", cex=1)
    text(x = -10, y = (10 + dim(selectedImg)[2] + dim(selectedImg)[2]/2), 
         label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (20 + 2*dim(selectedImg)[2] + dim(selectedImg)[2]/2), 
         label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (30 + 3*dim(selectedImg)[2] + dim(selectedImg)[2]/2), 
         label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (40 + 4*dim(selectedImg)[2] + dim(selectedImg)[2]/2), 
         label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    
    text(x = selectedImg@dim[1]/2, y = (50 + 5*selectedImg@dim[2]),             # Horizontal Labels
         label = input$imgsRedStain, col = "black", cex = 1)
    text(x = 10 + 1.5*selectedImg@dim[1], y = (50 + 5*selectedImg@dim[2]),
         label = input$imgsGreenStain, col = "black", cex = 1)
    text(x = 20 + 2.5*selectedImg@dim[1], y = (50 + 5*selectedImg@dim[2]),
         label = input$imgsBlueStain, col = "black", cex = 1)
  })
  
  # Mean Object Intensity Boxplots (Pre-Analysis)
  
  output$intensityImgPlot <- renderPlot({                                       # Image-Level Intensity Plot
    outliers <- ifelse(input$removeGraphOutliers, yes = 19, no = NA)
    ggboxplot(
      data = values$featuresDF.all,
      x = "filename",
      y = "b.mean",
      combine = TRUE,
      fill = "stain",
      palette = values$colorCode,
      title = "Boxplot 3. Image-Level Object Mean Intensities",
      xlab = "",
      ylab = "Intensity (0-1 grayscale)",
      caption = paste0("BASIN session ID: ", values$sessionID),
      outlier.shape = outliers
    ) +
      rotate_x_text(angle = 60)
      # scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
   
      })

  output$areaImgPlot <- renderPlot({                                            # Image-Level Area Plot
    outliers <- ifelse(input$removeGraphOutliers, yes = 19, no = NA)
    ggboxplot(
      data = values$featuresDF.all,
      x = "filename",
      y = "s.area",
      combine = TRUE,
      fill = "stain",
      palette = values$colorCode,
      title = "Boxplot 4. Image-Level Object Areas",
      xlab = "",
      ylab = "Area (pixels)",
      caption = paste0("BASIN session ID: ", values$sessionID),
      outlier.shape = outliers
    ) +
      rotate_x_text(angle = 60)
      # scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
    
      })
  
  boxplot1 <- reactive({
    outliers <- ifelse(input$removeGraphOutliers, yes = 19, no = NA)
    ggboxplot(
      data = values$featuresDF.all,
      x = "biocondition",
      y = "b.mean",
      # combine = TRUE,
      fill = "stain",
      palette = values$colorCode,
      title = "Boxplot 1. Objects' Mean Intensities",
      xlab = "",
      ylab = "Intensity (0-1 grayscale)",
      panel.labs = list(experiment = paste0(
        "E", levels(as.factor(values$featuresDF.all$experiment)))),
      facet.by = "experiment",
      # caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
      caption = paste0("BASIN session ID: ", values$sessionID), 
      outlier.shape = outliers
    ) + 
      rotate_x_text(angle = 60) +
      scale_x_discrete(labels = rep(c("control", "test"), 3))
  
  })
  
  boxplot2 <- reactive({
    outliers <- ifelse(input$removeGraphOutliers, yes = 19, no = NA)
    ggboxplot(
      data = values$featuresDF.all,
      x = "biocondition",
      y = "s.area",
      # combine = TRUE,
      fill = "stain",
      palette = values$colorCode,
      title = "Boxplot 2. Object Areas",
      #subtitle = "Outliers not shown",
      xlab = "",
      ylab = "Area (pixels)",
      panel.labs = list(experiment = paste0(
        "E",levels(as.factor(values$featuresDF.all$experiment)))),
      facet.by = "experiment",
      # caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
      caption = paste0("BASIN session ID: ", values$sessionID),
      outlier.shape = outliers
    ) + 
      rotate_x_text(angle = 60) +
      scale_x_discrete(labels = rep(c("control", "test"), 3)) +
      scale_y_continuous(
        limits = quantile(values$featuresDF.all$s.area, c(0.1, 0.9)))
      })                                                                            # end reactive 'boxplot2'
  
  metadataDT <- reactive({                                                      # Summary Data table
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, "Filename"),
          th(rowspan = 2, "Biocondition"),
          th(rowspan = 2, "Stain"),
          th(colspan = 2, "Image Level"),
          th(colspan = 4, "Object Level")
        ),
        tr(lapply(c(
          "Total Raw Image Intensity", "Image Object Count", 
          "Object Cumulative Intensity", "Mean Object Intensity", 
          "Object Cumulative Area (pixels)", "Mean Object Area (pixels)"), th)
        )
      )
    ))
    dtable <- datatable(
      data = values$barplotData[, 1:9], 
      caption = "Data Summary (all raw data included)",
      container = sketch,
      rownames = FALSE,
      selection = "none",
      options = list(
        searching = FALSE, 
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% formatRound(columns = c(4,5,6,8,9), digits = 0) %>% 
      formatRound(columns = 7, digits = 4) 
    # %>% formatStyle(
    #   c('sumImgIntensity','objCount'), backgroundColor = 'gray'
    # )
  })                                                                            # end reactive 'metadataDT'
  
  output$meantTestTable <- renderTable({                                        # Statistical Test Results Tables
    values$objectMeanResults
  })
  output$areatTestTable <- renderTable({
    values$objectAreaResults
  })
  output$differenceTable <- renderTable({
    values$differenceData
  })
  
  # Barplot of summed-object intensities for each image, split by image frames
  barplot1 <- reactive({
    ggbarplot(
      data = values$barplotData,
      x = "filename",
      y = "sumObjIntensity",
      combine = FALSE,
      merge = TRUE,
      fill = "stain",
      palette = values$colorCode,
      title = "Barplot 1. Sum Object Intensities",
      subtitle = "post-thresholded",
      # add = "mean_se",
      # error.plot = "pointrange",
      xlab = "",
      ylab = "Intensity (0-1 grayscale/px)",
      caption = paste0("BASIN session ID: ", values$sessionID)
    ) + 
    rotate_x_text(angle = 60) 
    # +
    # scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
  })
  
  # Barplot of object counts for each image, split by image frames
  barplot2 <- reactive({
    ggbarplot(
      data = values$barplotData,
      x = "filename",
      y = "objCount",
      combine = FALSE,
      merge = TRUE,
      fill = "stain",
      palette = values$colorCode,
      title = "Barplot 2. Object Counts",
      subtitle = "post-thresholded, all objects included",
      # add = "mean_se",
      # error.plot = "errorbar",
      xlab = "",
      ylab = "Intensity (0-1 grayscale/px)",
      caption = paste0("BASIN session ID: ", values$sessionID)
    ) + 
    rotate_x_text(angle = 60) 
    # +
    # scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
  })
  
  boxplot3 <- reactive({
    outliers <- ifelse(input$removeGraphOutliers, yes = 19, no = NA)
    ggboxplot(
      data = values$objectMeanData,
      x = "biocondition",
      y = "mean",
      combine = TRUE,
      merge = FALSE,
      fill = "stain",
      palette = values$colorCode,
      facet.by = "experiment",
      panel.labs = list(experiment = paste0(
        "E",levels(as.factor(values$objectMeanData$experiment)))),
      title = "Boxplot 3. Object Mean Intensities",
      xlab = FALSE,
      ylab = "Intensity (0-1 grayscale)",
      outlier.shape = outliers
      # ylim = c(0,max(values$featuresDF.all$b.mean)*1.25)
      ) +
      rotate_x_text(angle = 60) +
      stat_compare_means(aes(
        label = paste0("p = ", ..p.format..)),
        method = "t.test",
        comparisons = list(
          c("control.r", "test.r"), 
          c("control.g", "test.g"), 
          c("control.b", "test.b")),
        label.y = max(values$featuresDF.all$b.mean)*1.15,
        bracket.size = 0) +
      stat_compare_means(
        label = "p.signif",
        method = "t.test",
        comparisons = list(
          c("control.r", "test.r"),
          c("control.g", "test.g"),
          c("control.b", "test.b")),
        label.y = max(values$featuresDF.all$b.mean)*1.05) +
      scale_x_discrete(labels = rep(c("control", "test"), 3))
  })
  
  boxplot4 <- reactive({
    outliers <- ifelse(input$removeGraphOutliers, yes = 19, no = NA)
    ggboxplot(
      data = values$objectAreaData,
      x = "biocondition",
      y = "area",
      combine = TRUE,
      merge = FALSE,
      fill = "stain",
      palette = values$colorCode,
      facet.by = "experiment",
      panel.labs = list(experiment = paste0(
        "E",levels(as.factor(values$objectAreaData$experiment)))),
      title = "Boxplot 4. Object Areas",
      xlab = FALSE,
      ylab = "Area (pixels)",
      outlier.shape = outliers
      ) +
      rotate_x_text(angle = 60) +
      stat_compare_means(aes(
        label = paste0("p = ", ..p.format..)),
        method = "t.test",
        comparisons = list(
          c("control.r", "test.r"), 
          c("control.g", "test.g"), 
          c("control.b", "test.b")),
        label.y = max(values$featuresDF.all$s.area)*1.15,
        bracket.size = 0) +
      stat_compare_means(
        label = "p.signif", 
        method = "t.test",
        comparisons = list(
          c("control.r", "test.r"), 
          c("control.g", "test.g"), 
          c("control.b", "test.b")),
        label.y = max(values$featuresDF.all$s.area)*1.05) +
      scale_x_discrete(labels = rep(c("control", "test"), 3))
  })
  
  output$boxplot1 <- renderPlot(boxplot1())                                     # Reactive Plots
  output$boxplot2 <- renderPlot(boxplot2())
  output$metadataDT <- renderDT(metadataDT())
  
  output$resultPlot1 <- renderPlot({
    flag <- TRUE
    for(experiment in values$groupedExperimentTable){
      if(!any("test" == experiment$biocondition) || !any("control" == experiment$biocondition)){
        flag <- FALSE
      }
    }
    validate(need(flag, message = "All experimental groups must have at least one 'control' and one 'test' image for analysis."))
    boxplot3()
  })
  output$resultPlot2 <- renderPlot({
    flag <- TRUE
    for(experiment in values$groupedExperimentTable){
      if(!any("test" == experiment$biocondition) || !any("control" == experiment$biocondition)){
        flag <- FALSE
      }
    }
    validate(need(flag, message = "All experimental groups must have at least one 'control' and one 'test' image for analysis."))
    boxplot4()
  })
  

  observeEvent(input$gotoViewer, {                                              # Confirmation for Extractor Module
    withProgress(message = 'Saving Extraction and Analysis Results', value = 0, {
      showTab(
        inputId = "modulePanel", target = "Viewer", select = FALSE, session)
      updateTabsetPanel(session, "modulePanel", selected = "Viewer")
      if(input$chooseProjDir != 0){
        id <- format.POSIXct(Sys.time(), format = "%m-%d-%Y-%H%M%S")              # Set unique id for each run under new conditions
        extractDir <- paste0(getwd(),"/Extractor")                                # Check for existing Extractor folder and subfolders
        rawDir <- paste0(getwd(),"/Extractor/Thresh_Data",id)
        analyzeDir <- paste0(getwd(),"/Extractor/Analysis_Results")
        maskDir <- paste0(getwd(), "/Extractor/Image_Masks")
        redMasks <- paste0(maskDir, "/Red")
        greenMasks <- paste0(maskDir, "/Green")
        blueMasks <- paste0(maskDir, "/Blue")
        if(!dir.exists(extractDir)){
          dir.create(extractDir)
        }
        if(!dir.exists(rawDir)){
          dir.create(rawDir)
        }
        if(!dir.exists(analyzeDir)){
          dir.create(analyzeDir)
        }
        if(!dir.exists(maskDir)){
          dir.create(maskDir)
          dir.create(redMasks)
          dir.create(greenMasks)
          dir.create(blueMasks)
        }
        incProgress(amount = 0.33, message = "Saving Image Masks")
        # save a copy of all image masks to the output folder
        for(i in seq_along(values$imgs.r.thresholded)){
          red <- colormap(values$imgs.r.thresholded[[i]], palette = c("white","red"))
          green <- colormap(values$imgs.g.thresholded[[i]], palette = c("white","green"))
          blue <- colormap(values$imgs.b.thresholded[[i]], palette = c("white","blue"))
          writeImage(red, files = file.path(redMasks,values$img.files[i]), type = "jpeg", quality = 80)
          writeImage(green, files = file.path(greenMasks,values$img.files[i]), type = "jpeg", quality = 80)
          writeImage(blue, files = file.path(blueMasks, values$img.files[i]), type = "jpeg", quality = 80)
        }
        
        incProgress(amount = 0.67, message = "Saving Tables")
        write.csv(ldply(req(values$features.r), .id = "filename"),                # Copy all raw data into Thresh_Data subfolder
                  file = paste0(rawDir,"/redFrameImageData.csv"))
        write.csv(ldply(req(values$features.g), .id = "filename"), 
                  file = paste0(rawDir,"/greenFrameImageData.csv"))
        write.csv(ldply(req(values$features.b), .id = "filename"), 
                  file = paste0(rawDir,"/blueFrameImageData.csv"))
        write.csv(req(values$barplotData), 
                  file = paste0(rawDir,"/imageMetadata.csv"))
        write.csv(req(values$objectMeanResults),                                  # Copy all analysis data into Analysis_Results subfolder
                  file = paste0(analyzeDir,"/meanDataTtest.csv"))
        write.csv(req(values$objectAreaResults), 
                  file = paste0(analyzeDir,"/objectAreaTtest.csv"))
        write.csv(req(values$differenceData),
                  file = file.path(analyzeDir,"intensityAndCountDifferences.csv"))
  
        file.create("Extractor/Log.txt")                                          # Create log file with Extractor info
        log <- file("Extractor/Log.txt", open = "a+")
        extractInfo <- paste(                                                     # Format extraction parameters and extraction ID
          "Extraction",id,"autothreshold method:",input$thresh.auto)
        # logInfo <- c("EXTRACTOR",extractInfo)
        writeLines(extractInfo, con = log)
        close(log)
      }
    })
  })
  
  ##################################
  # VIEWER PLOTS AND REACTIVE TEXT #
  ##################################
  
  # output$uploadText <- renderUI({
  #   line1 <- c("1. User uploaded the following images: ",req(values$img.files))
  #   HTML(paste(line1))
  # })
  
  output$origImgsViewer <- renderPlot({                                         # Plotting all original images
    par(mfrow=c(ceiling(sqrt(length(values$imgs))),                             # Format a plotting grid of sufficient size, then plot all images
                ceiling(sqrt(length(values$imgs)))))
    # par(mfrow = c(1, length(values$imgs)))
    mapply(function(img,name){
      EBImage::display(img, method = "raster", margin = c(0,30))
      text(x = img@dim[1]/2, y = img@dim[2]+15, label = name, cex = 1.25)
    },img = values$imgs, name = values$img.files)
  })
  
  output$viewerImagePlot <- renderPlot({                                        # Plotting all image thresholding panels
    par(mfrow = c(2, ceiling(length(values$img.files)/2)))
    output <- lapply(values$img.files, function(x){
      c <- values$img.files == x                                                # Logical vector for image selection
      selectedImg <- values$imgs[c][[1]]                                        # Selecting all corresponding image frames
      selectedImg.r <- EBImage::channel(values$imgs.r[c][[1]],"asred")                                 # use req() to ensure thresholding button has been clicked
      selectedImg.g <- EBImage::channel(values$imgs.g[c][[1]],"asgreen")
      selectedImg.b <- EBImage::channel(values$imgs.b[c][[1]],"asblue")
      selectedImg.r.th <- EBImage::channel(values$imgs.r.thresholded[c][[1]],"asred")
      selectedImg.g.th <- EBImage::channel(values$imgs.g.thresholded[c][[1]],"asgreen")
      selectedImg.b.th <- EBImage::channel(values$imgs.b.thresholded[c][[1]],"asblue")
      selectedImg.r.clbl <- values$imgs.r.clrlabel[c][[1]]
      selectedImg.g.clbl <- values$imgs.g.clrlabel[c][[1]]
      selectedImg.b.clbl <- values$imgs.b.clrlabel[c][[1]]
      selectedImg.r.pntd <- values$imgs.r.pntd[c][[1]]
      selectedImg.g.pntd <- values$imgs.g.pntd[c][[1]]
      selectedImg.b.pntd <- values$imgs.b.pntd[c][[1]]

      # blank canvas for formatting
      blankSpot <- Image(matrix("white", dim(selectedImg)[1], dim(selectedImg)[2]))
      # Formatted Image 
      tryCatch({EBImage::combine(
        blankSpot, selectedImg, blankSpot,
        selectedImg.r, selectedImg.g, selectedImg.b,
        selectedImg.r.th, selectedImg.g.th, selectedImg.b.th,
        selectedImg.r.clbl, selectedImg.g.clbl, selectedImg.b.clbl,
        selectedImg.r.pntd, selectedImg.g.pntd, selectedImg.b.pntd
      )}, error = function(cond){
        blankSpot
      })
    })
    mapply(function(x,lbl){
      EBImage::display(
        x, nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE)

      text(x = (10 + x@dim[1] + x@dim[1]/2), y = -10, label = lbl,              # Vertical Labels
           adj = c(0.5, 0), col = "black", cex = 1)
      text(x = -10, y = (10 + x@dim[2] + x@dim[2]/2), label = "Frame", 
           adj = c(0.5, 0), col = "black", cex = 0.9, srt = 90)
      text(x = -10, y = (20 + 2.5*x@dim[2]), label = "Threshold", 
           adj = c(0.5, 0), col = "black", cex = 0.9, srt = 90)
      text(x = -10, y = (30 + 3.5*x@dim[2]), label = "Label", 
           adj = c(0.5, 0), col = "black", cex = 0.9, srt = 90)
      text(x = -10, y = (40 + 4.5*x@dim[2]), label = "Outline", 
           adj = c(0.5, 0), col = "black", cex = 0.9, srt = 90)
      text(x = x@dim[1]/2, y = (50 + 5*x@dim[2]),                               # Horizontal Labels
           label = input$imgsRedStain, col = "black", cex = 1)
      text(x = 10 + 1.5*x@dim[1], y = (50 + 5*x@dim[2]), 
           label = input$imgsGreenStain, col = "black", cex = 1)
      text(x = 20 + 2.5*x@dim[1], y = (50 + 5*x@dim[2]), 
           label = input$imgsBlueStain, col = "black", cex = 1)
    }, x = output, lbl = values$img.files)
  })
  
  output$extractorText <- renderUI({
    line1 <- paste0(
      "1. Image frames were thresholded using the following method: ", 
      input$thresh.auto, " for ", input$imgsRedStain, " stain, ",
      input$imgsGreenStain, " stain, and ", input$imgsBlueStain,  " stain.")
    line2 <- paste(
      "2. Images were grouped into", length(values$groupedExperimentTable),
      " experimental group(s) and split by biocondition - 'control' or 'test' 
      - for statistical analysis:")
    line3 <- ""
    for(exp in values$groupedExperimentTable){
      line3 <- c(line3,paste0("    Experimental Group ",exp$experiment[1],": ",exp$filename, 
                              " with the biocondition ",exp$biocondition)
                 )
    }
    line4 <- paste0("Staining Info: ")
    line5 <- paste0(
      "  a. ", input$imgsRedStain, ": ", input$r.intensity.altHypothesis, 
      " for mean intensity and ", input$r.area.altHypothesis, " for area")
    line6 <- paste0(
      "  b. ", input$imgsGreenStain, ": ", input$g.intensity.altHypothesis, 
      " for mean intensity and ", input$g.area.altHypothesis, " for area")
    line7 <- paste0(
      "  c. ", input$imgsBlueStain, ": ", input$b.intensity.altHypothesis, 
      " for mean intensity and ", input$b.area.altHypothesis, " for area") 
    HTML(paste(
      c(line1, line2, line3, line4, line5, line6, line7),collapse = '<br/>'))
  })
  
  extractorInterpret1 <- reactive({
    safesignif <- possibly(signif, NULL)
    splitMeanResults <- split(
      x = req(values$objectMeanResults), f = values$objectMeanResults$experiment)
    splitAreaResults <- split(
      x = req(values$objectAreaResults), f = values$objectAreaResults$experiment)
    meanOutput <- lapply(splitMeanResults, function(x){
      alt.r <- x$Ha[1]
      alt.g <- x$Ha[2]
      alt.b <- x$Ha[3]
      p.r <- safesignif(x$"p-value"[1],3)
      p.g <- safesignif(x$"p-value"[2],3)
      p.b <- safesignif(x$"p-value"[3],3)
      e <- x$experiment[1]
      # Red Frame Interpretation
      if(!is.null(p.r) & !is.na(p.r)){
        if(p.r <= 0.05){
          if(alt.r == "two.sided"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we can 
            reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object intensities for the Experiment", e, input$imgsRedStain,
              "stain control and test groups are not equal.", sep = " ")
          }else if(alt.r == "less"){
            redConclusion <- paste0(
              "With a p-value of",p.r,"and a significance level of 0.05, we can 
            reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object intensities for the", e, input$imgsRedStain,
              "stain control group is less than the mean object intensities of 
            the test group.", sep = " ")
          }else if(alt.r == "greater"){
            redConclusion <- paste0(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically 
            significant evidence to support the alternative hypothesis that 
            the mean object intensities for the", e, input$imgsRedStain, 
              "stain control group is greater than the mean object intensities 
            of the test group.", sep = " ")
          }else{
            redConclusion <- paste0(
              "The mean object intensities for the", input$imgsRedStain, 
              "stain control and test groups were not tested.", sep = " ")
          }
        }else{
          if(alt.r == "two.sided"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsRedStain, "stain control and test 
            groups are equal.", sep = " ")
          }else if(alt.r == "less"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsRedStain, "stain control group is less 
            than the mean object intensities of the test group.", sep = " ")
          }else if(alt.r == "greater"){
            redConclusion <- paste(
              "With a p-value of",p.r,"and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsRedStain, "stain control group is 
            greater than the mean object intensities of the test group.", 
              sep = " ")
          }else{
            redConclusion <- paste0(
              "The mean object intensities for the", input$imgsRedStain, 
              "stain control and test groups were not tested.", sep = " ")
          }
        }
      } else{
        redConclusion <- "Not tested due to insufficient quantity of observations."
      }
      if(!is.null(p.g) & !is.na(p.g)){
        # Green Frame Interpretation
        if(p.g <= 0.05){
          if(alt.g == "two.sided"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically 
            significant evidence to support the alternative hypothesis that 
            the mean object intensities for the Experiment", e, 
              input$imgsGreenStain,"stain control and test groups are not 
            equal.", sep = " ")
          }else if(alt.g == "less"){
            greenConclusion <- paste0(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically 
            significant evidence to support the alternative hypothesis that 
            the mean object intensities for the", e, input$imgsGreenStain,
              "stain control group is less than the mean object intensities of 
            the test group.", sep = " ")
          }else if(alt.g == "greater"){
            greenConclusion <- paste0(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object intensities for the", e, input$imgsGreenStain, "stain 
            control group is greater than the mean object intensities of the 
            test group.", sep = " ")
          }else{
            greenConclusion <- paste0(
              "The mean object intensities for the", input$imgsGreenStain, 
              "stain control and test groups were not tested.", sep = " ")
          }
        }else{
          if(alt.g == "two.sided"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsGreenStain, "stain control and test 
            groups are equal.", sep = " ")
          }else if(alt.g == "less"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsGreenStain, "stain control group is less 
            than the mean object intensities of the test group.", sep = " ")
          }else if(alt.g == "greater"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsGreenStain, "stain control group is 
            greater than the mean object intensities of the test group.", 
              sep = " ")
          }else{
            greenConclusion <- paste0(
              "The mean object intensities for the", input$imgsGreenStain, 
              "stain control and test groups were not tested.", sep = " ")
          }
        }
      } else{
        greenConclusion <- "Not tested due to insufficient quantity of observations."
      }
      if(!is.null(p.b) & !is.na(p.b)){
        # Blue Frame Interpretation
        if(p.b <= 0.05){
          if(alt.b == "two.sided"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object intensities for the Experiment", e, input$imgsGreenStain,
              "stain control and test groups are not equal.", sep = " ")
          }else if(alt.b == "less"){
            blueConclusion <- paste0(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically 
            significant evidence to support the alternative hypothesis that 
            the mean object intensities for the", e, input$imgsGreenStain, 
              "stain control group is less than the mean object intensities of 
            the test group.", sep = " ")
          }else if(alt.b == "greater"){
            blueConclusion <- paste0(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object intensities for the", e, input$imgsGreenStain, "stain 
            control group is greater than the mean object intensities of the 
            test group.", sep = " ")
          }else{
            blueConclusion <- paste0(
              "The mean object intensities for the", input$imgsGreenStain, 
              "stain control and test groups were not tested.", sep = " ")
          }
        }else{
          if(alt.b == "two.sided"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsGreenStain, "stain control and test 
            groups are equal.", sep = " ")
          }else if(alt.b == "less"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsGreenStain, "stain control group is less 
            than the mean object intensities of the test group.", sep = " ")
          }else if(alt.b == "greater"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object intensities for the 
            Experiment", e, input$imgsGreenStain, "stain control group is 
            greater than the mean object intensities of the test group.", 
              sep = " ")
          }else{
            blueConclusion <- paste0(
              "The mean object intensities for the", input$imgsGreenStain, "stain 
            control and test groups were not tested.", sep = " ")
          }
        }
      } else{
        blueConclusion <- "Not tested due to insufficient quantity of observations."
      }
      line1 <- paste0("<br/>"," Experiment ",e,": ")
      line2 <- paste0("1. ", redConclusion)
      line3 <- paste0("2. ", greenConclusion)
      line4 <- paste0("3. ", blueConclusion,"<br/>")
      return(paste(line1,line2,line3,line4, sep = "<br/>"))
    })
    areaOutput <- lapply(splitAreaResults, function(x){
      alt.r <- x$Ha[1]
      alt.g <- x$Ha[2]
      alt.b <- x$Ha[3]
      p.r <- safesignif(x$"p-value"[1],3)
      p.g <- safesignif(x$"p-value"[2],3)
      p.b <- safesignif(x$"p-value"[3],3)
      e <- x$experiment[1]
      # Red Frame Interpretation
      if(!is.null(p.r) & !is.na(p.r)){
        if(p.r <= 0.05){
          if(alt.r == "two.sided"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we can 
            reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean object 
            areas for the Experiment", e, input$imgsRedStain, "stain control 
            and test groups are not equal.", sep = " ")
          }else if(alt.r == "less"){
            redConclusion <- paste0(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean object 
            areas for the", e, input$imgsRedStain, "stain control group is 
            less than the mean object areas of the test group.", sep = " ")
          }else if(alt.r == "greater"){
            redConclusion <- paste0(
              "With a p-value of", p.r, "and a significance level of 0.05, we can 
            reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean object 
            areas for the", e, input$imgsRedStain, "stain control group is 
            greater than the mean object areas of the test group.", sep = " ")
          }else{
            redConclusion <- paste0(
              "The object areas for the", input$imgsRedStain, "stain control and 
            test groups were not tested.", sep = " ")
          }
        }else{
          if(alt.r == "two.sided"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsRedStain, "stain control and test groups 
            are equal.", sep = " ")
          }else if(alt.r == "less"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsRedStain, "stain control group is less 
            than the mean object areas of the test group.", sep = " ")
          }else if(alt.r == "greater"){
            redConclusion <- paste(
              "With a p-value of", p.r, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsRedStain, "stain control group is 
            greater than the mean object areas of the test group.", sep = " ")
          }else{
            redConclusion <- paste0(
              "The object areas for the", input$imgsRedStain, "stain control 
            and test groups were not tested.", sep = " ")
          }
        }
      } else{
        redConclusion <- "Not tested due to insufficient quantity of observations."
      }
      if(!is.null(p.g) & !is.na(p.g)){
        # Green Frame Interpretation
        if(p.g <= 0.05){
          if(alt.g == "two.sided"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object areas for the Experiment", e, input$imgsGreenStain, "stain 
            control and test groups are not equal.", sep = " ")
          }else if(alt.g == "less"){
            greenConclusion <- paste0(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object areas for the", e, input$imgsGreenStain, "stain control 
            group is less than the mean object areas of the test group.", 
              sep = " ")
          }else if(alt.g == "greater"){
            greenConclusion <- paste0(
              "With a p-value of", p.g, "and a significance level of 0.05, we can 
            reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean object 
            areas for the", e, input$imgsGreenStain,"stain control group is 
            greater than the mean object areas of the test group.", sep = " ")
          }else{
            greenConclusion <- paste0(
              "The object areas for the", input$imgsGreenStain, "stain control 
            and test groups were not tested.", sep = " ")
          }
        }else{
          if(alt.g == "two.sided"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsGreenStain, "stain control and test 
            groups are equal.", sep = " ")
          }else if(alt.g == "less"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsGreenStain, "stain control group is less 
            than the mean object areas of the test group.", sep = " ")
          }else if(alt.g == "greater"){
            greenConclusion <- paste(
              "With a p-value of", p.g, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsGreenStain, "stain control group is 
            greater than the mean object areas of the test group.", sep = " ")
          }else{
            greenConclusion <- paste0(
              "The object areas for the", input$imgsGreenStain, "stain control 
            and test groups were not tested.", sep = " ")
          }
        }
      } else{
        greenConclusion <- "Not tested due to insufficient quantity of observations."
      }
      if(!is.null(p.b) & !is.na(p.b)){
        # Blue Frame Interpretation
        if(p.b <= 0.05){
          if(alt.b == "two.sided"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object areas for the Experiment", e, input$imgsGreenStain, "stain 
            control and test groups are not equal.", sep = " ")
          }else if(alt.b == "less"){
            blueConclusion <- paste0(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            can reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean 
            object areas for the", e, input$imgsGreenStain, "stain control 
            group is less than the mean object areas of the test group.", 
              sep = " ")
          }else if(alt.b == "greater"){
            blueConclusion <- paste0(
              "With a p-value of", p.b, "and a significance level of 0.05, we can 
            reject the null hypothesis. There is statistically significant 
            evidence to support the alternative hypothesis that the mean object 
            areas for the", e, input$imgsGreenStain, "stain control group is 
            greater than the mean object areas of the test group.", sep = " ")
          }else{
            blueConclusion <- paste0(
              "The object areas for the", input$imgsGreenStain, "stain control 
            and test groups were not tested.", sep = " ")
          }
        }else{
          if(alt.b == "two.sided"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsGreenStain, "stain control and test 
            groups are equal.", sep = " ")
          }else if(alt.b == "less"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsGreenStain, "stain control group is less 
            than the mean object areas of the test group.", sep = " ")
          }else if(alt.b == "greater"){
            blueConclusion <- paste(
              "With a p-value of", p.b, "and a significance level of 0.05, we 
            fail to reject the null hypothesis. There is not enough evidence 
            to support the claim that the mean object areas for the 
            Experiment", e, input$imgsGreenStain, "stain control group is 
            greater than the mean object areas of the test group.", sep = " ")
          }else{
            blueConclusion <- paste0(
              "The object areas for the", input$imgsGreenStain, "stain control 
            and test groups were not tested.", sep = " ")
          }
        }
      } else{
        blueConclusion <- "Not tested due to insufficient quantity of observations."
      }
      line1 <- paste0("<br/>"," Experiment ",e,": ")
      line2 <- paste0("1. ", redConclusion)
      line3 <- paste0("2. ", greenConclusion)
      line4 <- paste0("3. ", blueConclusion,"<br/>")
      return(paste(line1,line2,line3,line4,sep = "<br/>"))
    })
    
    m.title <- paste("Formal intensity conclusions:")
    a.title <- paste("Formal area conclusions:")
    m <- simplify(meanOutput)
    a <- simplify(areaOutput)
    paste(c(m.title,"<br/>",m,"<br/>",a.title,"<br/>",a))
  })
  output$extractorInterpret <- renderUI({
    HTML(extractorInterpret1())
  }) # end renderUI 'output$extractorInterpret'
  
  output$viewerInterpret <- renderUI({
    HTML(extractorInterpret1())
  }) # end renderUI 'output$viewerInterpret'
  
  output$boxplot1.v <- renderPlot(boxplot1())
  output$boxplot2.v <- renderPlot(boxplot2())
  output$metadataDT.v <- renderDT(metadataDT())
  # output$tTestDT.v <- renderDT({
  #   validate(need(
  #     values$meanAreaResults, 
  #     label="Please view and confirm Stat Design before viewing results table"))
  #   tTestDT()
  # })
  output$intensityTestDT.v <- renderTable({                                        # Statistical Test Results Tables for Viewer
    # validate(need(
    #   values$objectMeanResults,
    #   label="Please view and confirm Stat Design before viewing results table"))
    values$objectMeanResults
  })
  output$areaTestDT.v <- renderTable({
    # validate(need(
    #   values$objectAreaResults,
    #   label="Please view and confirm Stat Design before viewing results table"))
    values$objectAreaResults
  })        
  
  output$barplot1 <- renderPlot(barplot1())
  output$barplot2 <- renderPlot(barplot2())
  output$boxplot3 <- renderPlot({
    flag <- TRUE
    for(experiment in values$groupedExperimentTable){
      if(!any("test" == experiment$biocondition) || !any("control" == experiment$biocondition)){
        flag <- FALSE
      }
    }
    validate(need(flag, message = "All experimental groups must have at least one 'control' and one 'test' image for analysis."))
    boxplot3()
  })
  output$boxplot4 <- renderPlot({
    flag <- TRUE
    for(experiment in values$groupedExperimentTable){
      if(!any("test" == experiment$biocondition) || !any("control" == experiment$biocondition)){
        flag <- FALSE
      }
    }
    validate(need(flag, message = "All experimental groups must have at least one 'control' and one 'test' image for analysis."))
    boxplot4()
  })
  
  ###################
  # REPORTER MODULE #
  ###################
  # 
  # observe(input$reportAuthor || input$reportTitle,{
  #   values$reportAuthor <- input$reportAuthor
  #   values$reportTitle <- input$reportTitle
  # })
  reportAuthor <- reactive({input$reportAuthor})
  reportTitle <- reactive({input$reportTitle})
  
  # Markdown document for full report
  output$downloadFullReport <- downloadHandler(
    filename = function() {
      paste('my_figure', sep = '.', 
            switch(
              input$format, HTML = 'html', PDF = 'pdf', Word = 'docx'))
    },
    content = function(file) {
      withProgress(value = 0.5, message = "Generating Report. This may take a few minutes.",{
        src <- file.path(reportsDir, "fullReport.Rmd")
        basinOutDir <- getwd()
        owd <- setwd(tempdir())                                                   # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
        on.exit(expr = setwd(owd))
        file.copy(src, 'fullReport.Rmd', overwrite = TRUE)
        pandoc_toc_args(toc = TRUE, toc_depth = 2)
        out <- render('fullReport.Rmd', switch(
          input$format,
          HTML = html_document(), PDF = pdf_document(), Word = word_document()
        ))
        # save a copy of HTML and Word into output folder if it exists
        if(input$chooseProjDir != 0){
          path <- file.path(basinOutDir,"Reporter")
          if(!dir.exists(path)){
            dir.create(path)
          }
          if(input$format == "HTML"){
            file.copy(out, file.path(path, "full_report.html"))
            render('fullReport.Rmd', output_format = word_document(),output_file = "full_report.docx", output_dir = path)
          } else if(input$format == "Word"){
            file.copy(out, file.path(path, "full_report.docx"))
            render('fullReport.Rmd', output_format = html_document(),output_file = "full_report.html", output_dir = path)
          } else {
            render('fullReport.Rmd', output_format = word_document(),output_file = "full_report.docx", output_dir = path)
          }
        }
        file.rename(out, file)
      })
    }
  )
  
  ##############
  # REFERENCES #
  ##############
  
  output$thresholdReference <- renderUI({
    if(input$thresh.auto == "IJDefault") {
      HTML(paste(
        "<sup>2</sup>Ridler TW, Calvard S. Picture thresholding using an 
        iterative selection method. <i>IEEE Transactions on Systems, Man, 
        and Cybernetics</i>. 1978;8:630-632. doi:10.1109/TSMC.1978.4310039"))
    } else if(input$thresh.auto == "Huang") {
      HTML(paste(
        "<sup>2</sup>Huang L-K, Wang M-JJ. Image thresholding by minimizing 
        the measure of fuzziness. <i>Pattern Recognition</i>. 1995;18(1):41-51. 
        doi:10.1016/0031-3203(94)E0043-K"))
    } else if(input$thresh.auto == "Huang2") {
      HTML(paste(
        "<sup>2</sup>Landini G. Auto threshold. ImageJ website. 
        https://imagej.net/Auto_Threshold#Default. April 29, 2017. Accessed 
        December 23, 2019."))
    } else if(input$thresh.auto == "Intermodes") {
      HTML(paste(
        "<sup>2</sup>Prewitt JMS, Mendelsohn ML. The analysis of cell images. 
        <i>Annals of the New York Academy of Sciences</i>. 
        1966;128:1035-1053."))
    } else if(input$thresh.auto == "IsoData") {
      HTML(paste(
        "<sup>2</sup>Ridler TW, Calvard S. Picture thresholding using an 
        iterative selection method. <i>IEEE Transactions on Systems, Man, 
        and Cybernetics</i>. 1978;8:630-632. doi:10.1109/TSMC.1978.4310039"))
    } else if(input$thresh.auto == "Li") {
      HTML(paste(
        "<sup>2</sup>Li CH, Tam PKS. An iterative algorithm for minimum cross 
        entropy thresholding. <i>Pattern Recognition Letters</i>. 1998;18(8):
        771-776. doi:10.1016/S0167-8655(98)00057-9"))
    } else if(input$thresh.auto == "MaxEntropy") {
      HTML(paste(
        "<sup>2</sup>Kapur JN, Sahoo PK, Wong ACK. A new method for gray-level 
        picture thresholding using the entropy of the histogram. <i>Graphical 
        Models and Image Processing</i>. 1985;29(3):273-285. doi:10.1016/0734-
        189X(85)90125-2"))
    } else if(input$thresh.auto == "Mean") {
      HTML(paste(
        "<sup>2</sup>Glasbey CA. An analysis of histogram-based thresholding 
        algorithms. <i>CVGIP: Graphical Models and Image Processing</i>. 
        1993;55:532-537. doi:10.1006/cgip.1993.1040"))
    } else if(input$thresh.auto == "MinErrorI") {
      HTML(paste(
        "<sup>2</sup>Kittler J, Illingworth J. Minimum error thresholding. 
        <i>Pattern Recognition</i>. 1986;19:41-47. doi:10.1016/0031-3203(86)
        90030-0"))
    } else if(input$thresh.auto == "Minimum") {
      HTML(paste(
        "<sup>2</sup>Prewitt JMS, Mendelsohn ML. The analysis of cell images. 
        <i>Annals of the New York Academy of Sciences</i>. 1966;128:1035-1053."
        ))
    } else if(input$thresh.auto == "Moments") {
      HTML(paste(
        "<sup>2</sup>Tsai W. Moment-preserving thresholding: a new approach. 
        <i>Computer Vision, Graphics, and Image Processing</i>. 1985;29:377-
        393."))
    } else if(input$thresh.auto == "Otsu") {
      HTML(paste(
        "<sup>2</sup>Otsu N. A threshold selection method from gray-level 
        histograms. <i>IEEE Transactions on Systems, Man, and Cybernetics</i>. 
        1979;9:62-66. doi:10.1109/TSMC.1979.4310076"))
    } else if(input$thresh.auto == "Percentile") {
      HTML(paste(
        "<sup>2</sup>Doyle W. Operation useful for similarity-invariant pattern 
        recognition. <i>Journal of the Association for Computing Machinery</i>. 
        1962;9:259-267. doi:10.1145/321119.321123"))
    } else if(input$thresh.auto == "RenyiEntropy") {
      HTML(paste(
        "<sup>2</sup>Kapur JN, Sahoo PK, Wong ACK. A new method for gray-level 
        picture thresholding using the entropy of the histogram. <i>CVGIP: 
        Graphical Models and Image Processing</i>. 1985;29(3):273-285. doi:10.
        1016/0734-189X(85)90125-2"))
    } else if(input$thresh.auto == "Shanbhag") {
      HTML(paste(
        "<sup>2</sup>Shanbhag AG. Utilization of information measure as a 
        means of image thresholding. <i>CVGIP: Graphical Models and Image 
        Processing</i>. 1994;56(5):414-419. doi:10.1006/cgip.1994.1037"))
    } else if(input$thresh.auto == "Triangle") {
      HTML(paste(
        "<sup>2</sup>Zack GW, Rogers WE, Latt SA. Automatic measurement 
        of sister chromatid exchange frequency. <i>Journal of Histochemistry 
        and Cytochemistry</i>. 1977;25(7):741-753. doi:10.1177/25.7.70454"))
    } else if(input$thresh.auto == "Yen") {
      line1 <- "<sup>2.1</sup>Yen JC, Chang FJ, Chang S. A New Criterion for 
      Automatic Multilevel Thresholding. <i>IEEE Transactions on Image 
      Processing</i>. 1995;4(3):370-378. doi:10.1109/83.366472"
      line2 <- "<sup>2.2</sup>Sezgin M, Sankur B. Survey over Image 
      Thresholding Techniques and Quantitative Performance Evaluation. 
      <i>Journal of Electronic Imaging</i>. 2004;13(1):146-165."
      HTML(paste(line1, line2, sep = '<br/>'))
    }
  })
  
  ##############
  # ACTION LOG #
  ##############
  
  output$actionLog1 <- renderUI({
    time <- paste(Sys.time())
    line <- paste0(
      "New session started using local user '", Sys.getenv("USER"), "'")
    HTML(paste(time, "--", line, sep = " "))
  })
  

  observeEvent(input$chooseProjDir, {
    output$actionLog2 <- renderUI({
      time <- paste(Sys.time())
      line <- paste0(
        "Session folder chosen: '", req(values$projDir), "'")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(values$img.files, {
    output$actionLog3 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "User uploaded", length(values$img.files), 
        "image(s) recognized by BASIN")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(values$imgs, {
    output$actionLog4 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "EBImage package read", length(values$imgs), " local image(s)")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent({
    values$imgs.r
    values$imgs.g
    values$imgs.b
  }, {
    output$actionLog5 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "EBImage package split", length(values$imgs), 
        "local image(s) into separate frames;", length(values$imgs.r), "red,", 
        length(values$imgs.g), "green, and", length(values$imgs.b), 
        "blue frames retrieved")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(values$barplotData, {
    output$actionLog6 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "Base package created a metadata table for use in Viewer and Reporter")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent({
    values$imgs.r.label
    values$imgs.g.label
    values$imgs.b.label
  }, {
    output$actionLog7 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "EBImage package began reactive thresholding and object labeling for",
        length(values$imgs.r.label), "red,", length(values$imgs.g.label), 
        "green, and", length(values$imgs.b.label), "blue frames")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent({
    values$features.r
    values$features.g
    values$features.b
  }, {
    output$actionLog8 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "EBImage package began reactive object feature extraction for",
        length(values$features.r), "red,", length(values$features.g), 
        "green, and", length(values$features.b), "blue frames")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent({
    values$featuresDF.all
  }, {
    output$actionLog9 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "Base package formed feature data tables for use by ggplot package. 
        Object intensity and area boxplots ready to view in Extractor.")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(input$confirmUploadBttn, {
    output$actionLog10 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "Folder created in user's chosen directory:", paste(values$outputName), 
        ". An 'Upload' subfolder and Log.txt was created. The log file was 
        updated with uploaded image filenames.", sep = " ")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(input$confirmThresh, {
    output$actionLog11 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
        "User chose the following thresholding method: ", input$thresh.auto, 
                    ". Extractor subfolder was created which contains pre-
        processing data for RGB frames. Log file was updated with extraction 
        parameters.", sep = " ")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(input$downloadAnalysisTable, {
    output$actionLog12 <- renderUI({
      time <- paste(Sys.time())
      line <- paste("User downloaded the analysis table template")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(input$uploadAnalysisTable, {
    output$actionLog13 <- renderUI({
      time <- paste(Sys.time())
      line <- paste0(
        "User uploaded a CSV file with name '", 
        paste(input$uploadAnalysisTable$name), "'")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  observeEvent(input$statResults, {
    output$actionLog14 <- renderUI({
      time <- paste(Sys.time())
      line <- paste(
      "Analyses run based on Extractor parameters and uploaded analysis table. 
      An 'Analysis_Results' subfolder was created within Extractor folder; 
      the folder contains the uploaded analysis table and t-test results. The 
      log was updated with analysis information. Plots and t-test results ready 
      to view.")
      HTML(paste(time, "--", line, sep = " "))
    })
  })
  
  ############
  # HELP TAB #
  ############
  
  output$authorsUsed <- renderValueBox({
    valueBox("Authors", value = "##", icon = icon("pen"), color = "red")})
  output$imagesAnlzd <- renderValueBox({
    valueBox("Images Analyzed", value = "##", icon = icon("images"), 
             color = "green")})
  output$reportsPublsd <- renderValueBox({
    valueBox("Reports Published", value = "##", icon = icon("newspaper"), 
             color = "blue")})
}) # end shinyServer