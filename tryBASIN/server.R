####################################################################################################################################################
##############################################    tryBASIN server version 12.23.19    ##############################################################
####################################################################################################################################################

# Function to properly construct data frame of t.test results
tTestToDataFrame <- function(results, frame, altHypo){
  if(!is.na(results[[1]])){
    data.frame("stain" = frame, "altHypo" = altHypo, "t-statistic" = results$statistic, "df" = results$parameter, "pvalue" = results$p.value, 
               "lower conf int" = results$conf.int[1], "upper conf int" = results$conf.int[2], 
               "conf level" = attributes(results$conf.int)$conf.level, method = results$method, check.names = FALSE)
  } else {
    data.frame("stain" = frame, "altHypo" = "not tested", "t-statistic" = NA, "df" = NA, "pvalue" = NA, 
               "lower conf int" = NA, "upper conf int" = NA, 
               "conf level" = NA, method = NA, check.names = FALSE)
  }
}

# Function to extract the given column from feature data
colExtract <- function(x, column){
  tryCatch({
    as.vector(x[,column])
  },
  error = function(cond){
    x <- data.frame(b.mean = 0, b.sd = 0, b.mad = 0, s.area = 0)
    return(x[,column])
  })
}

# Determine alternative hypothesis
alternative <- function(x){
  if(x == "not equal to"){
    return("two.sided")
  } else if(x == "less than"){
    return("less")
  } else if(x == "greater than"){
    return("greater")
  } else if(x == "do not test"){
    return(NA)
}}

reportsDir <- getwd()

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  ################################################################
  # VISUAL & NAVIGATION FUNCTIONS   
  ################################################################
  
  hideTab(inputId = "modulePanel", target = "Extractor", session)
  hideTab(inputId = "modulePanel", target = "Viewer", session)
  hideTab(inputId = "modulePanel", target = "Reporter", session)
  hideElement(id = "previewBoxplots")
  
  observeEvent({
    values$img1
    values$img2
    }, enable(id = "gotoExtractor")
  )
  
  observeEvent(input$gotoExtractor, {
    values$sessionID <- format.POSIXct(Sys.time(), format = "%m-%d-%Y-%H%M%S")
    showTab(inputId = "modulePanel", target = "Extractor", select = FALSE, session)
    updateTabsetPanel(session, "modulePanel", selected = "Extractor")
  })
  
  observeEvent(input$closeStatDesign, 
  toggleModal(session, modalId = "statDesignPopup", toggle = "close")
  )
  
  observeEvent(input$gotoViewer, {
    showTab(inputId = "modulePanel", target = "Viewer", select = FALSE, session)
    updateTabsetPanel(session, "modulePanel", selected = "Viewer")
  })
  
  observeEvent(input$confirmViewerBttn,{
    showTab(inputId = "modulePanel", target = "Reporter", select = FALSE, session)
    updateTabsetPanel(session, "modulePanel", selected = "Reporter")
  })
  
  ################################################################
  # READ ORIGINAL IMAGES
  ################################################################
  
  observeEvent(input$tryOurExample, {
    values$img1 <- readImage("www/messerli_f3_astrocyte_vatalanib_mge.jpg")
    values$img2 <- readImage("www/weingart_f2_chla06_plko.jpg")
    values$imgsSource <- paste(
      "1. BASIN example images were uploaded by user: messerli_f3_astrocyte_vatalanib_mge.jpg for 
      biocondition 1 and weingart_f2_chla06_plko.jpg for biocondition 2."
    ) 
  })
  
   observeEvent(input$upload1,
     values$img1 <- readImage(input$upload1$datapath)
   )
  
  observeEvent(input$upload2, {
    values$img2 <- readImage(input$upload2$datapath)
    values$imgsSource <- paste(
      "1. User's images were uploaded:", input$upload1$name, "for biocondition 1 and", input$upload2$name, "for biocondition 2.", sep = " "
    )
  })
  


  # observeEvent(input$upload1, {
  #     unfiltered_files <- as.vector(stri_extract_all(input$upload1$name, regex = ".*jpg|.*png|.*tif", simplify = TRUE))
  #     values$img.files <- input$upload1$name[!is.na(unfiltered_files)]
  #     values$img.paths <- input$upload1$datapath[!is.na(unfiltered_files)]
  #     values$imgs <- lapply(values$img.paths, readImage)
  # })
  

  
  
  ################################################################
  # SPLIT, THRESHOLD IMAGES AND LABEL OBJECTS
  ################################################################
  
  observeEvent({
    input$confirmThresh
    }, {
    imgs <- list(values$img1, values$img2)
    names(imgs)[1] <- input$img1Condition
    names(imgs)[2] <- input$img2Condition
    values$imgs.r <- lapply(imgs, function(x) getFrame(x, 1))
    values$imgs.g <- lapply(imgs, function(x) getFrame(x, 2))
    values$imgs.b <- lapply(imgs, function(x) getFrame(x, 3))
    names(values$imgs.r)[c(1,2)] <- c(input$img1Condition, input$img2Condition)
    names(values$imgs.g)[c(1,2)] <- c(input$img1Condition, input$img2Condition)
    names(values$imgs.b)[c(1,2)] <- c(input$img1Condition, input$img2Condition)

    autothreshold <- function(x) {
      y <- x*(2^16-1)
      z <- mask(y, method = input$thresh.auto)
      x*z
    }
    
    values$imgs.r.thresholded <- lapply(values$imgs.r, autothreshold)
    values$imgs.g.thresholded <- lapply(values$imgs.g, autothreshold)
    values$imgs.b.thresholded <- lapply(values$imgs.b, autothreshold)
    
    values$imgs.r.label <- lapply(values$imgs.r.thresholded, bwlabel)
    values$imgs.g.label <- lapply(values$imgs.g.thresholded, bwlabel)
    values$imgs.b.label <- lapply(values$imgs.b.thresholded, bwlabel)
    
    values$imgs.r.clrlabel <- lapply(values$imgs.r.label, colorLabels)
    values$imgs.g.clrlabel <- lapply(values$imgs.g.label, colorLabels)
    values$imgs.b.clrlabel <- lapply(values$imgs.b.label, colorLabels)
    
    values$img1.r <- channel(values$imgs.r[[1]], "asred")
    values$img1.g <- channel(values$imgs.g[[1]], "asgreen")
    values$img1.b <- channel(values$imgs.b[[1]], "asblue")
    values$img2.r <- channel(values$imgs.r[[2]], "asred")
    values$img2.g <- channel(values$imgs.g[[2]], "asgreen")
    values$img2.b <- channel(values$imgs.b[[2]], "asblue")
    
    values$img1.r.th <- channel(values$imgs.r.thresholded[[1]], "asred")
    values$img1.g.th <- channel(values$imgs.g.thresholded[[1]], "asgreen")
    values$img1.b.th <- channel(values$imgs.b.thresholded[[1]], "asblue")
    values$img2.r.th <- channel(values$imgs.r.thresholded[[2]], "asred")
    values$img2.g.th <- channel(values$imgs.g.thresholded[[2]], "asgreen")
    values$img2.b.th <- channel(values$imgs.b.thresholded[[2]], "asblue")
    
    values$img1.r.clbl <- values$imgs.r.clrlabel[[1]]
    values$img1.g.clbl <- values$imgs.g.clrlabel[[1]]
    values$img1.b.clbl <- values$imgs.b.clrlabel[[1]]
    values$img2.r.clbl <- values$imgs.r.clrlabel[[2]]
    values$img2.g.clbl <- values$imgs.g.clrlabel[[2]]
    values$img2.b.clbl <- values$imgs.b.clrlabel[[2]]
    
    values$img1.r.pntd <- paintObjects(values$imgs.r.label[[1]], values$img1.r, col = "yellow")
    values$img1.g.pntd <- paintObjects(values$imgs.g.label[[1]], values$img1.g, col = "yellow")
    values$img1.b.pntd <- paintObjects(values$imgs.b.label[[1]], values$img1.b, col = "yellow")
    values$img2.r.pntd <- paintObjects(values$imgs.r.label[[2]], values$img2.r, col = "yellow")
    values$img2.g.pntd <- paintObjects(values$imgs.g.label[[2]], values$img2.g, col = "yellow")
    values$img2.b.pntd <- paintObjects(values$imgs.b.label[[2]], values$img2.b, col = "yellow")
    
    # print(values$img1.r.pntd)
    ################################################################
    # FEATURE EXTRACTION
    ################################################################
    
    names(values$imgs.r.label)[c(1,2)] <- c(input$img1Condition, input$img2Condition)
    names(values$imgs.g.label)[c(1,2)] <- c(input$img1Condition, input$img2Condition)
    names(values$imgs.b.label)[c(1,2)] <- c(input$img1Condition, input$img2Condition)
    
    basicFeatures.img1.r <- computeFeatures.basic(x = values$imgs.r.label[[1]], ref = values$imgs.r[[1]])
    basicFeatures.img1.g <- computeFeatures.basic(x = values$imgs.g.label[[1]], ref = values$imgs.g[[1]])
    basicFeatures.img1.b <- computeFeatures.basic(x = values$imgs.b.label[[1]], ref = values$imgs.b[[1]])
    basicFeatures.img2.r <- computeFeatures.basic(x = values$imgs.r.label[[2]], ref = values$imgs.r[[2]])
    basicFeatures.img2.g <- computeFeatures.basic(x = values$imgs.g.label[[2]], ref = values$imgs.g[[2]])
    basicFeatures.img2.b <- computeFeatures.basic(x = values$imgs.b.label[[2]], ref = values$imgs.b[[2]])

    features.img1.r <- data.frame(
      stain = input$imgsRedStain, 
      basicFeatures.img1.r, 
      computeFeatures.moment(x = values$imgs.r.label[[1]], ref = values$imgs.r[[1]]), 
      computeFeatures.shape(x = values$imgs.r.label[[1]]))
    features.img1.g <- data.frame(
      stain = input$imgsGreenStain, 
      basicFeatures.img1.g, 
      computeFeatures.moment(x = values$imgs.g.label[[1]], ref = values$imgs.g[[1]]), 
      computeFeatures.shape(x = values$imgs.g.label[[1]]))
    features.img1.b <- data.frame(
      stain = input$imgsBlueStain, 
      basicFeatures.img1.b, 
      computeFeatures.moment(x = values$imgs.b.label[[1]], ref = values$imgs.b[[1]]), 
      computeFeatures.shape(x = values$imgs.b.label[[1]]))
    features.img2.r <- data.frame(
      stain = input$imgsRedStain, 
      basicFeatures.img2.r, 
      computeFeatures.moment(x = values$imgs.r.label[[2]], ref = values$imgs.r[[2]]), 
      computeFeatures.shape(x = values$imgs.r.label[[2]]))
    features.img2.g <- data.frame(
      stain = input$imgsGreenStain, 
      basicFeatures.img2.g, 
      computeFeatures.moment(x = values$imgs.g.label[[2]], ref = values$imgs.g[[2]]), 
      computeFeatures.shape(x = values$imgs.g.label[[2]]))
    features.img2.b <- data.frame(
      stain = input$imgsBlueStain, 
      basicFeatures.img2.b, 
      computeFeatures.moment(x = values$imgs.b.label[[2]], ref = values$imgs.b[[2]]), 
      computeFeatures.shape(x = values$imgs.b.label[[2]]))

    values$features.r <- list(features.img1.r, features.img2.r)
    values$features.g <- list(features.img1.g, features.img2.g)
    values$features.b <- list(features.img1.b, features.img2.b)
    
    names(values$features.r)[c(1, 2)] <- c("cond1.r", "cond2.r")
    names(values$features.g)[c(1, 2)] <- c("cond1.g", "cond2.g")
    names(values$features.b)[c(1, 2)] <- c("cond1.b", "cond2.b")
    
    featuresDF.r <- lapply(values$features.r, data.frame)
    featuresDF.g <- lapply(values$features.g, data.frame)
    featuresDF.b <- lapply(values$features.b, data.frame)
    featuresDF.r <- ldply(featuresDF.r, .id = "biocondition")
    featuresDF.g <- ldply(featuresDF.g, .id = "biocondition")
    featuresDF.b <- ldply(featuresDF.b, .id = "biocondition")
    values$featuresDF.all <- rbind(featuresDF.r, featuresDF.g, featuresDF.b)
    
    # print(str(featuresDF.r))
    # Object count determined using number of rows
    count.r <- lapply(values$features.r, nrow)
    count.g <- lapply(values$features.g, nrow)
    count.b <- lapply(values$features.b, nrow)
    
    # Any images with 0 objects get assigned a count of 0
    count.r <- replace(count.r, vapply(count.r, is.null, logical(1)), 0)
    count.g <- replace(count.g, vapply(count.g, is.null, logical(1)), 0)
    count.b <- replace(count.b, vapply(count.b, is.null, logical(1)), 0)
    
    # Converts list into data frame for Raw Data table
    count.r <- ldply(count.r, data.frame, .id = "biocondition")
    count.g <- ldply(count.g, data.frame, .id = "biocondition")
    count.b <- ldply(count.b, data.frame, .id = "biocondition")   

    # Sum Intensity and Object Count Data
    metadata.r <- data.frame(
      row.names = NULL,
      condition = c(input$img1Condition, input$img2Condition),
      stain = input$imgsRedStain,
      sumImgIntensity = vapply(imgs, function(x) {sum(imageData(x)[,,1])}, numeric(1)),
      objCount = count.r$X..i..,
      sumObjIntensity = vapply(values$imgs.r.thresholded, function(x) {sum(imageData(x))}, numeric(1)),
      meanObjIntensity = c(mean(features.img1.r$b.mean), mean(features.img2.r$b.mean)),
      sumObjArea = c(sum(features.img1.r$s.area), sum(features.img2.r$s.area)),
      meanObjArea = c(mean(features.img1.r$s.area), mean(features.img2.r$s.area)),
      biocondition = count.r$biocondition
    )
    metadata.g <- data.frame(
      row.names = NULL,
      condition = c(input$img1Condition, input$img2Condition),
      stain = input$imgsGreenStain,
      sumImgIntensity = vapply(imgs, function(x) {sum(imageData(x)[,,2])}, numeric(1)),
      objCount = count.g$X..i..,
      sumObjIntensity = vapply(values$imgs.g.thresholded, function(x) {sum(imageData(x))}, numeric(1)),
      meanObjIntensity = c(mean(features.img1.g$b.mean), mean(features.img2.g$b.mean)),
      sumObjArea = c(sum(features.img1.g$s.area), sum(features.img2.g$s.area)),
      meanObjArea = c(mean(features.img1.g$s.area), mean(features.img2.g$s.area)),
      biocondition = count.g$biocondition
    )
    metadata.b <- data.frame(
      row.names = NULL,
      condition = c(input$img1Condition, input$img2Condition),
      stain = input$imgsBlueStain,
      sumImgIntensity = vapply(imgs, function(x) {sum(imageData(x)[,,3])}, numeric(1)),
      objCount = count.b$X..i..,
      sumObjIntensity = vapply(values$imgs.b.thresholded, function(x) {sum(imageData(x))}, numeric(1)),
      meanObjIntensity = c(mean(features.img1.b$b.mean), mean(features.img2.b$b.mean)),
      sumObjArea = c(sum(features.img1.b$s.area), sum(features.img2.b$s.area)),
      meanObjArea = c(mean(features.img1.b$s.area), mean(features.img2.b$s.area)),
      biocondition = count.b$biocondition
    )
    
    values$barplotData <- rbind(
      metadata.r[1,], metadata.g[1,], metadata.b[1,], metadata.r[2,], metadata.g[2,], metadata.b[2,]
    )
    # print(values$barplotData)
    showElement(id = "previewBoxplots")
    enable(id = "statDesign")
    
  }) # end observeEvent(input$confirmThresh)
  
  # Allow user to access Viewer only once they have checked the Stat Design
  observeEvent(input$statDesign,{
    enable(id = "gotoViewer")
  })
  
  ################################################################
  # T-TESTING
  ################################################################
  
  observeEvent(
    {
    input$confirmThresh
    input$r.intensity.altHypothesis
    input$g.intensity.altHypothesis
    input$b.intensity.altHypothesis
    input$r.area.altHypothesis
    input$g.area.altHypothesis
    input$b.area.altHypothesis
    input$minMaxObjectArea.r
    input$minMaxObjectArea.g
    input$minMaxObjectArea.b
    },
    {
    featuresDF.r <- lapply(req(values$features.r), data.frame)
    featuresDF.g <- lapply(req(values$features.g), data.frame)
    featuresDF.b <- lapply(req(values$features.b), data.frame)
    
    featuresDF.r.clean <- list(
      cond1.r = featuresDF.r$cond1.r[featuresDF.r$cond1.r$s.area >= input$minMaxObjectArea.r[1] & featuresDF.r$cond1.r$s.area <= input$minMaxObjectArea.r[2], c(1,2,15)],
      cond2.r = featuresDF.r$cond2.r[featuresDF.r$cond2.r$s.area >= input$minMaxObjectArea.r[1] & featuresDF.r$cond2.r$s.area <= input$minMaxObjectArea.r[2], c(1,2,15)]
    )
    featuresDF.g.clean <- list(
      cond1.g = featuresDF.g$cond1.g[featuresDF.g$cond1.g$s.area >= input$minMaxObjectArea.g[1] & featuresDF.g$cond1.g$s.area <= input$minMaxObjectArea.g[2], c(1,2,15)],
      cond2.g = featuresDF.g$cond2.g[featuresDF.g$cond2.g$s.area >= input$minMaxObjectArea.g[1] & featuresDF.g$cond2.g$s.area <= input$minMaxObjectArea.g[2], c(1,2,15)]
    )
    featuresDF.b.clean <- list(
      cond1.b = featuresDF.b$cond1.b[featuresDF.b$cond1.b$s.area >= input$minMaxObjectArea.b[1] & featuresDF.b$cond1.b$s.area <= input$minMaxObjectArea.b[2], c(1,2,15)],
      cond2.b = featuresDF.b$cond2.b[featuresDF.b$cond2.b$s.area >= input$minMaxObjectArea.b[1] & featuresDF.b$cond2.b$s.area <= input$minMaxObjectArea.b[2], c(1,2,15)]
    )
    
    features.r.clean <- ldply(featuresDF.r.clean, .id = "biocondition")
    features.g.clean <- ldply(featuresDF.g.clean, .id = "biocondition")
    features.b.clean <- ldply(featuresDF.b.clean, .id = "biocondition")
    values$featuresDF.all.clean <- rbind(features.r.clean, features.g.clean, features.b.clean)

    safeTest <- possibly(function(group1, group2, alt){
      t.test(x = group1, y = group2, alternative = alternative(alt))
    }, otherwise = NA)
    
    meanInt.r.results <- safeTest(featuresDF.r.clean[[1]]$b.mean, featuresDF.r.clean[[2]]$b.mean, input$r.intensity.altHypothesis)
    meanInt.g.results <- safeTest(featuresDF.g.clean[[1]]$b.mean, featuresDF.g.clean[[2]]$b.mean, input$g.intensity.altHypothesis)
    meanInt.b.results <- safeTest(featuresDF.b.clean[[1]]$b.mean, featuresDF.b.clean[[2]]$b.mean, input$b.intensity.altHypothesis)
    
    meanArea.r.results <- safeTest(featuresDF.r.clean[[1]]$s.area, featuresDF.r.clean[[2]]$s.area, input$r.area.altHypothesis)
    meanArea.g.results <- safeTest(featuresDF.g.clean[[1]]$s.area, featuresDF.g.clean[[2]]$s.area, input$g.area.altHypothesis)
    meanArea.b.results <- safeTest(featuresDF.b.clean[[1]]$s.area, featuresDF.b.clean[[2]]$s.area, input$b.area.altHypothesis)

    values$resultsDF <- rbind(
      tTestToDataFrame(meanInt.r.results, paste(input$imgsRedStain, "intensity", sep = " "), input$r.intensity.altHypothesis),
      tTestToDataFrame(meanInt.g.results, paste(input$imgsGreenStain, "intensity", sep = " "), input$g.intensity.altHypothesis),
      tTestToDataFrame(meanInt.b.results, paste(input$imgsBlueStain, "intensity", sep = " "), input$b.intensity.altHypothesis),
      tTestToDataFrame(meanArea.r.results, paste(input$imgsRedStain, "area", sep = " "), input$r.area.altHypothesis),
      tTestToDataFrame(meanArea.g.results, paste(input$imgsGreenStain, "area", sep = " "), input$g.area.altHypothesis),
      tTestToDataFrame(meanArea.b.results, paste(input$imgsBlueStain, "area", sep = " "), input$b.area.altHypothesis)
    )
  }) # end observeEvent

  ################################################################
  # REACTIVE GRAPHIC ELEMENTS
  ################################################################
  
  img1.all.seq <- reactive({
    blankSpot <- Image(matrix("white", dim(values$img1)[1], dim(values$img1)[2]))
    EBImage::display(
      EBImage::combine(
        blankSpot, values$img1, blankSpot,
        values$img1.r, values$img1.g, values$img1.b, 
        values$img1.r.th, values$img1.g.th, values$img1.b.th, 
        values$img1.r.clbl, values$img1.g.clbl, values$img1.b.clbl, 
        values$img1.r.pntd, values$img1.g.pntd, values$img1.b.pntd
      ), nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE
    )
    text(x = (10 + dim(values$img1)[1] + dim(values$img1)[1]/2), y = -10, label = input$img1Condition, adj = c(0.5, 0), col = "black", cex = 1)
    text(x = -10, y = (10 + dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (20 + 2*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (30 + 3*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (40 + 4*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
  })

  img2.all.seq <- reactive({
    blankSpot <- Image(matrix("white", dim(values$img2)[1], dim(values$img2)[2]))
    EBImage::display(
      EBImage::combine(
        blankSpot, values$img2, blankSpot,
        values$img2.r, values$img2.g, values$img2.b, 
        values$img2.r.th, values$img2.g.th, values$img2.b.th, 
        values$img2.r.clbl, values$img2.g.clbl, values$img2.b.clbl, 
        values$img2.r.pntd, values$img2.g.pntd, values$img2.b.pntd
      ), nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE
    )
    text(x = (10 + dim(values$img2)[1] + dim(values$img2)[1]/2), y = -10, label = input$img2Condition, adj = c(0.5, 0), col = "black", cex = 1)
    text(x = -10, y = (10 + dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (20 + 2*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (30 + 3*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    text(x = -10, y = (40 + 4*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
  })
  
  boxplot1 <- reactive({
    ggboxplot(
      data = values$featuresDF.all,
      x = "biocondition",
      y = "b.mean",
      combine = TRUE,
      fill = "stain",
      title = "Boxplot 1. Objects' Mean Intensities",
      xlab = "",
      ylab = "Intensity (0-1 grayscale)",
      caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
    ) + 
      rotate_x_text(angle = 45) + 
      scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
  })
  
  boxplot2 <- reactive({
    ggboxplot(
      data = values$featuresDF.all,
      x = "biocondition",
      y = "s.area",
      combine = TRUE,
      fill = "stain",
      title = "Boxplot 2. Object Areas",
      subtitle = "Outliers not shown",
      xlab = "",
      ylab = "Area (pixels)",
      caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName),
      outlier.shape = NA
    ) + 
      rotate_x_text(angle = 45) +
      scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3)) +
      scale_y_continuous(limits = quantile(values$featuresDF.all$s.area, c(0.1, 0.9)))
  }) # end reactive 'boxplot2'
  
  metadataDT <- reactive({
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, "Biocondition"),
          th(rowspan = 2, "Stain"),
          th(colspan = 2, "Image Level"),
          th(colspan = 4, "Object Level")
        ),
        tr(lapply(c("Total Raw Image Intensity", "Image Object Count", "Object Cumulative Intensity", "Mean Object Intensity", "Object Cumulative Area (pixels)", "Mean Object Area (pixels)"), th)
        )
      )
    ))
    dtable <- datatable(
      data = values$barplotData[, 1:8], 
      caption = "Table 1. Data Summary (all raw data included)",
      container = sketch,
      rownames = FALSE,
      selection = "none",
      options = list(
        searching = FALSE, 
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>% formatRound(columns = c(3, 5, 8), digits = 0) %>% formatRound(columns = 6, digits = 4) %>% formatStyle(
      c('sumImgIntensity','objCount'), backgroundColor = 'gray'
    )
  }) # end reactive 'metadataDT'
  
  tTestDT <- reactive({
    datatable(
    data = values$resultsDF, 
    caption = paste("Table 2. T-test results for", input$img1Condition, "versus", input$img2Condition, sep = " "),
    rownames = FALSE,
    colnames = c("Data", "Ha", "T-stat", "DF", "P-value", "Lower Confidence Interval", "Upper Confidence Interval", "Confidence Level", "Method"),
    selection = "none",
    options = list(
      searching = FALSE, 
      paging = FALSE,
      scrollX = TRUE
    )
  ) %>% formatRound(columns = c(3:7), digits = 3)
  })
  
  barplot1 <- reactive({
    print(values$barplotData)
    ggbarplot(
      data = values$barplotData,
      x = "biocondition",
      y = "sumObjIntensity",
      combine = TRUE,
      merge = FALSE,
      fill = "stain",
      title = "Barplot 1. Sum Object Intensities",
      subtitle = "post-thresholded",
      add = "mean_se",
      error.plot = "pointrange",
      xlab = "",
      ylab = "Intensity (0-1 grayscale/px)",
      caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
    ) + 
    rotate_x_text(angle = 45) +
    scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
  })
  
  barplot2 <- reactive({
    ggbarplot(
      data = values$barplotData,
      x = "biocondition",
      y = "objCount",
      combine = TRUE,
      merge = FALSE,
      fill = "stain",
      title = "Barplot 2. Object Counts",
      subtitle = "post-thresholded, all objects included",
      add = "mean_se",
      error.plot = "errorbar",
      xlab = "",
      ylab = "Intensity (0-1 grayscale/px)",
      caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
    ) + 
    rotate_x_text(angle = 45) +
    scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
  })
  
  boxplot3 <- reactive({
    print(str(values$featuresDF.all.clean))
    ggboxplot(
      data = values$featuresDF.all.clean,
      x = "biocondition",
      y = "b.mean",
      combine = TRUE,
      fill = "stain",
      title = "Boxplot 3. Object Intensities",
      subtitle = "user-defined object area range",
      xlab = "",
      ylab = "Intensity (0-1 grayscale)",
      caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
    ) +
    rotate_x_text(angle = 45) +
    stat_compare_means(
      aes(label = paste0("p = ", ..p.format..)),
      comparisons = list(c("cond1.r", "cond2.r"), c("cond1.g", "cond2.g"), c("cond1.b", "cond2.b")),
      label.y = 1,
      bracket.size = 0
    ) +
    stat_compare_means(
      label = "p.signif",
      method = "t.test",
      comparisons = list(c("cond1.r", "cond2.r"), c("cond1.g", "cond2.g"), c("cond1.b", "cond2.b")),
      label.y = 0.95
    ) +
    scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3))
  })
  
  boxplot4 <- reactive({
    ggboxplot(
      data = values$featuresDF.all.clean,
      x = "biocondition",
      y = "s.area",
      combine = TRUE,
      fill = "stain",
      title = "Boxplot 4. Object Areas",
      subtitle = "user-defined object area range",
      xlab = "",
      ylab = "Area (pixels)",
      caption = paste0("BASIN session ID: ", values$sessionID, "; experiment ID: ", input$experimentName)
    ) + 
    rotate_x_text(angle = 45) +
    stat_compare_means(
      aes(label = paste0("p = ", ..p.format..)),
      comparisons = list(c("cond1.r", "cond2.r"), c("cond1.g", "cond2.g"), c("cond1.b", "cond2.b")),
      label.y = max(values$featuresDF.all.clean$s.area)*1.15,
      bracket.size = 0
    ) +
    stat_compare_means(
      label = "p.signif", 
      method = "t.test", 
      comparisons = list(c("cond1.r", "cond2.r"), c("cond1.g", "cond2.g"), c("cond1.b", "cond2.b")),
      label.y = max(values$featuresDF.all.clean$s.area)*1.10
    ) +
    scale_x_discrete(labels = rep(c(input$img1Condition, input$img2Condition), 3)) 
  })
  
  ##########################################################################
  # OUTPUTS
  ##########################################################################
  
  output$img1 <- renderPlot({
    validate(need(values$img1, ""))
    plot(values$img1)
  })
  
  output$img2 <- renderPlot({
    validate(need(values$img2, ""))
    plot(values$img2)
  })
  
  output$viewStain <- renderUI({
    radioButtons(
      inputId = "viewStain", 
      label = "View stain:", 
      choices = c("all", input$imgsRedStain, input$imgsGreenStain, input$imgsBlueStain), 
      inline = TRUE
    )
  })
  
  output$minMaxObjectArea.r <- renderUI({
    quantiles <- quantile(c(values$features.r[[1]]$s.area, values$features.r[[2]]$s.area), c(0.25, 0.75))
    IQR <- quantiles[2]-quantiles[1]
    minArea <- min(c(values$features.r[[1]]$s.area, values$features.r[[2]]$s.area))
    maxArea <- max(c(values$features.r[[1]]$s.area, values$features.r[[2]]$s.area))
    sliderInput(
      inputId = "minMaxObjectArea.r", 
      label = div("Object Area Range (pixels):", style = "color:red"), 
      min = minArea, 
      max = maxArea, 
      value = c(quantiles[1]-1.5*IQR, quantiles[2]+1.5*IQR)
    )
  })
  
  output$minMaxObjectArea.g <- renderUI({
    quantiles <- quantile(c(values$features.g[[1]]$s.area, values$features.g[[2]]$s.area), c(0.25, 0.75))
    IQR <- quantiles[2]-quantiles[1]
    minArea <- min(c(values$features.g[[1]]$s.area, values$features.g[[2]]$s.area))
    maxArea <- max(c(values$features.g[[1]]$s.area, values$features.g[[2]]$s.area))
    sliderInput(
      inputId = "minMaxObjectArea.g", 
      label = div("Object Area Range (pixels):", style = "color:green"), 
      min = minArea, 
      max = maxArea, 
      value = c(quantiles[1]-1.5*IQR, quantiles[2]+1.5*IQR)
    )
  })
  
  output$minMaxObjectArea.b <- renderUI({
    quantiles <- quantile(c(values$features.b[[1]]$s.area, values$features.b[[2]]$s.area), c(0.25, 0.75))
    IQR <- quantiles[2]-quantiles[1]
    minArea <- min(c(values$features.b[[1]]$s.area, values$features.b[[2]]$s.area))
    maxArea <- max(c(values$features.b[[1]]$s.area, values$features.b[[2]]$s.area))
    sliderInput(
      inputId = "minMaxObjectArea.b", 
      label = div("Object Area Range (pixels):", style = "color:blue"), 
      min = minArea, 
      max = maxArea, 
      value = c(quantiles[1]-1.5*IQR, quantiles[2]+1.5*IQR)
    )
  })

  output$img1ConditionA <- renderText(input$img1Condition)
  output$img1ConditionB <- renderText(input$img1Condition)
  output$img2ConditionA <- renderText(input$img2Condition)
  output$img2ConditionB <- renderText(input$img2Condition)
  output$imgsRedStain <- renderText(input$imgsRedStain)
  output$imgsGreenStain <- renderText(input$imgsGreenStain)
  output$imgsBlueStain <- renderText(input$imgsBlueStain)
  output$img1ConditionC <- renderText(input$img1Condition)
  output$img2ConditionC <- renderText(input$img2Condition)
  output$img1ConditionD <- renderText(input$img1Condition)
  output$img2ConditionD <- renderText(input$img2Condition)
  
  output$img1.sequence <- renderPlot({
    validate(need(values$imgs.r[[1]], "Click 'Confirm' to see extraction sequence"))
    img1.text <- reactive({
      text(x = dim(values$img1)[1]/2, y = -10, label = input$img1Condition, adj = c(0.5, 0), col = "black", cex = 1)
      text(x = -10, y = dim(values$img1)[2]/2, label = "Original", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (10 + dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (20 + 2*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (30 + 3*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (40 + 4*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    })
    if(input$viewStain == input$imgsRedStain) {
      EBImage::display(EBImage::combine(values$img1, values$img1.r, values$img1.r.th, values$img1.r.clbl, values$img1.r.pntd), nx = 1, spacing = 10, margin = 40, method = "raster", all = TRUE)
      img1.text()
    } else if(input$viewStain == input$imgsGreenStain) {
      EBImage::display(EBImage::combine(values$img1, values$img1.g, values$img1.g.th, values$img1.g.clbl, values$img1.g.pntd), nx = 1, spacing = 10, margin = 40,  method = "raster", all = TRUE)
      img1.text()
    } else if(input$viewStain == input$imgsBlueStain) {
      EBImage::display(EBImage::combine(values$img1, values$img1.b, values$img1.b.th, values$img1.b.clbl, values$img1.b.pntd), nx = 1, spacing = 10, margin = 40,  method = "raster", all = TRUE)
      img1.text()
    } else if(input$viewStain == "all") {
      blankSpot <- Image(matrix("white", dim(values$img1)[1], dim(values$img1)[2]))
      EBImage::display(
        EBImage::combine(
          blankSpot, values$img1, blankSpot,
          values$img1.r, values$img1.g, values$img1.b, 
          values$img1.r.th, values$img1.g.th, values$img1.b.th, 
          values$img1.r.clbl, values$img1.g.clbl, values$img1.b.clbl, 
          values$img1.r.pntd, values$img1.g.pntd, values$img1.b.pntd
        ), nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE
      )
      text(x = (10 + dim(values$img1)[1] + dim(values$img1)[1]/2), y = -10, label = input$img1Condition, adj = c(0.5, 0), col = "black", cex = 1)
      text(x = -10, y = (10 + dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (20 + 2*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (30 + 3*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (40 + 4*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    }
  }) # end renderPlot 'img1.sequence'
  
  output$img2.sequence <- renderPlot({
    validate(need(values$imgs.r[[2]], "Click 'Confirm' to see extraction sequence"))
    img2.text <- reactive({
      text(x = dim(values$img2)[1]/2, y = -10, label = input$img2Condition, adj = c(0.5, 0), col = "black", cex = 1)
      text(x = -10, y = dim(values$img2)[2]/2, label = "Original", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (10 + dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (20 + 2*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (30 + 3*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (40 + 4*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    })
    
    if(input$viewStain == input$imgsRedStain) {
      EBImage::display(EBImage::combine(values$img2, values$img2.r, values$img2.r.th, values$img2.r.clbl, values$img2.r.pntd), nx = 1, spacing = 10, margin = 40,  method = "raster", all = TRUE)
      img2.text()
    } else if(input$viewStain == input$imgsGreenStain) {
      EBImage::display(EBImage::combine(values$img2, values$img2.g, values$img2.g.th, values$img2.g.clbl, values$img2.g.pntd), nx = 1, spacing = 10, margin = 40,  method = "raster", all = TRUE)
      img2.text()
    } else if(input$viewStain == input$imgsBlueStain) {
      EBImage::display(EBImage::combine(values$img2, values$img2.b, values$img2.b.th, values$img2.b.clbl, values$img2.b.pntd), nx = 1, spacing = 10, margin = 40,  method = "raster", all = TRUE)
      img2.text()
    } else if(input$viewStain == "all") {
      blankSpot <- Image(matrix("white", dim(values$img2)[1], dim(values$img2)[2]))
      EBImage::display(
        EBImage::combine(
          blankSpot, values$img2, blankSpot,
          values$img2.r, values$img2.g, values$img2.b, 
          values$img2.r.th, values$img2.g.th, values$img2.b.th, 
          values$img2.r.clbl, values$img2.g.clbl, values$img2.b.clbl, 
          values$img2.r.pntd, values$img2.g.pntd, values$img2.b.pntd
        ), nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE
      )
      text(x = (10 + dim(values$img2)[1] + dim(values$img2)[1]/2), y = -10, label = input$img2Condition, adj = c(0.5, 0), col = "black", cex = 1)
      text(x = -10, y = (10 + dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (20 + 2*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (30 + 3*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
      text(x = -10, y = (40 + 4*dim(values$img2)[2] + dim(values$img2)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
    }
  }) # end renderPlot 'img2.sequence'
  
  output$boxplot1 <- renderPlot(boxplot1())
  output$boxplot2 <- renderPlot(boxplot2())
  output$metadataDT <- renderDT(metadataDT())
    
  output$resultPlot1 <- renderPlot(boxplot3())
  output$resultPlot2 <- renderPlot(boxplot4())
  
  output$tTestDT <- renderDT({
    validate(need(values$resultsDF, label = "Please view and confirm Statistical Design before viewing your results"))
    tTestDT()
    })
  
  output$thresholdReference <- renderUI({
    if(input$thresh.auto == "IJDefault") {
      HTML(paste("<sup>2</sup>Ridler TW, Calvard S. Picture thresholding using an iterative selection method. <i>IEEE Transactions on Systems, Man, and Cybernetics</i>. 1978;8:630-632. doi:10.1109/TSMC.1978.4310039"))
    } else if(input$thresh.auto == "Huang") {
      HTML(paste("<sup>2</sup>Huang L-K, Wang M-JJ. Image thresholding by minimizing the measure of fuzziness. <i>Pattern Recognition</i>. 1995;18(1):41-51. doi:10.1016/0031-3203(94)E0043-K"))
    } else if(input$thresh.auto == "Huang2") {
      HTML(paste("<sup>2</sup>Landini G. Auto threshold. ImageJ website. https://imagej.net/Auto_Threshold#Default. April 29, 2017. Accessed December 23, 2019."))
    } else if(input$thresh.auto == "Intermodes") {
      HTML(paste("<sup>2</sup>Prewitt JMS, Mendelsohn ML. The analysis of cell images. <i>Annals of the New York Academy of Sciences</i>. 1966;128:1035-1053."))
    } else if(input$thresh.auto == "IsoData") {
      HTML(paste("<sup>2</sup>Ridler TW, Calvard S. Picture thresholding using an iterative selection method. <i>IEEE Transactions on Systems, Man, and Cybernetics</i>. 1978;8:630-632. doi:10.1109/TSMC.1978.4310039"))
    } else if(input$thresh.auto == "Li") {
      HTML(paste("<sup>2</sup>Li CH, Tam PKS. An iterative algorithm for minimum cross entropy thresholding. <i>Pattern Recognition Letters</i>. 1998;18(8):771-776. doi:10.1016/S0167-8655(98)00057-9"))
    } else if(input$thresh.auto == "MaxEntropy") {
      HTML(paste("<sup>2</sup>Kapur JN, Sahoo PK, Wong ACK. A new method for gray-level picture thresholding using the entropy of the histogram. <i>Graphical Models and Image Processing</i>. 1985;29(3):273-285. doi:10.1016/0734-189X(85)90125-2"))
    } else if(input$thresh.auto == "Mean") {
      HTML(paste("<sup>2</sup>Glasbey CA. An analysis of histogram-based thresholding algorithms. <i>CVGIP: Graphical Models and Image Processing</i>. 1993;55:532-537. doi:10.1006/cgip.1993.1040"))
    } else if(input$thresh.auto == "MinErrorI") {
      HTML(paste("<sup>2</sup>Kittler J, Illingworth J. Minimum error thresholding. <i>Pattern Recognition</i>. 1986;19:41-47. doi:10.1016/0031-3203(86)90030-0"))
    } else if(input$thresh.auto == "Minimum") {
      HTML(paste("<sup>2</sup>Prewitt JMS, Mendelsohn ML. The analysis of cell images. <i>Annals of the New York Academy of Sciences</i>. 1966;128:1035-1053."))
    } else if(input$thresh.auto == "Moments") {
      HTML(paste("<sup>2</sup>Tsai W. Moment-preserving thresholding: a new approach. <i>Computer Vision, Graphics, and Image Processing</i>. 1985;29:377-393."))
    } else if(input$thresh.auto == "Otsu") {
      HTML(paste("<sup>2</sup>Otsu N. A threshold selection method from gray-level histograms. <i>IEEE Transactions on Systems, Man, and Cybernetics</i>. 1979;9:62-66. doi:10.1109/TSMC.1979.4310076"))
    } else if(input$thresh.auto == "Percentile") {
      HTML(paste("<sup>2</sup>Doyle W. Operation useful for similarity-invariant pattern recognition. <i>Journal of the Association for Computing Machinery</i>. 1962;9:259-267. doi:10.1145/321119.321123"))
    } else if(input$thresh.auto == "RenyiEntropy") {
      HTML(paste("<sup>2</sup>Kapur JN, Sahoo PK, Wong ACK. A new method for gray-level picture thresholding using the entropy of the histogram. <i>CVGIP: Graphical Models and Image Processing</i>. 1985;29(3):273-285. doi:10.1016/0734-189X(85)90125-2"))
    } else if(input$thresh.auto == "Shanbhag") {
      HTML(paste("<sup>2</sup>Shanbhag AG. Utilization of information measure as a means of image thresholding. <i>CVGIP: Graphical Models and Image Processing</i>. 1994;56(5):414-419. doi:10.1006/cgip.1994.1037"))
    } else if(input$thresh.auto == "Triangle") {
      HTML(paste("<sup>2</sup>Zack GW, Rogers WE, Latt SA. Automatic measurement of sister chromatid exchange frequency. <i>Journal of Histochemistry and Cytochemistry</i>. 1977;25(7):741-753. doi:10.1177/25.7.70454"))
    } else if(input$thresh.auto == "Yen") {
      line1 <- "<sup>2.1</sup>Yen JC, Chang FJ, Chang S. A New Criterion for Automatic Multilevel Thresholding. <i>IEEE Transactions on Image Processing</i>. 1995;4(3):370-378. doi:10.1109/83.366472"
      line2 <- "<sup>2.2</sup>Sezgin M, Sankur B. Survey over Image Thresholding Techniques and Quantitative Performance Evaluation. <i>Journal of Electronic Imaging</i>. 2004;13(1):146-165."
      HTML(paste(line1, line2, sep = '<br/>'))
    }
  })
  
  output$img1.v <- renderPlot(
    EBImage::display(values$img1, nx = 1, spacing = 10, method = "raster", all = TRUE)
  )
  
  output$img2.v <- renderPlot(
    EBImage::display(values$img2, nx = 1, spacing = 10, method = "raster", all = TRUE)
  )
  
  output$uploadText <- renderUI({
    line1 <- values$imgsSource
    HTML(paste(line1))
  })
  
  output$extractorText <- renderUI({
    line1 <- paste0("1. Image frames were thresholded using the following methods: ", input$red.thresh.auto, " for ", input$imgsRedStain, " stain, ",
                   input$green.thresh.auto, " for ", input$imgsGreenStain, " stain, and ",
                   input$blue.thresh.auto, " for ", input$imgsBlueStain,  "stain.")
    line2 <- paste("2. User chose the following alternate hypotheses:")
    line3 <- paste0("  a. ", input$imgsRedStain, ": ", input$r.intensity.altHypothesis, " for mean intensity and ", input$r.area.altHypothesis, " for area")
    line4 <- paste0("  b. ", input$imgsGreenStain, ": ", input$g.intensity.altHypothesis, " for mean intensity and ", input$g.area.altHypothesis, " for area")
    line5 <- paste0("  c. ", input$imgsBlueStain, ": ", input$b.intensity.altHypothesis, " for mean intensity and ", input$b.area.altHypothesis, " for area") 
    line6 <- paste("3. User chose to exclude from t-testing:")
    line7 <- paste0("  a. ", input$imgsRedStain, " objects with area less than ", input$minMaxObjectArea.r[1], " and greater than ", input$minMaxObjectArea.r[2], " pixels ")
    line8 <- paste0("  b. ", input$imgsGreenStain, " objects with area less than ", input$minMaxObjectArea.g[1], " and greater than ", input$minMaxObjectArea.g[2], " pixels ")
    line9 <- paste0("  c. ", input$imgsBlueStain, " objects with area less than ", input$minMaxObjectArea.b[1], " and greater than ", input$minMaxObjectArea.b[2], " pixels ")
    HTML(paste(line1, line2, line3, line4, line5, line6, line7, line8, line9, sep = '<br/>'))
  })
  
  output$extractorInterpret <- renderUI({
    validate(need(values$resultsDF, label = "Please view and confirm Statistical Design before you may view interpretations"))
    if(values$resultsDF[1,2] == "not equal to" & values$resultsDF[1,5] <= 0.05){
      redIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is not equal to condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "not equal to" & values$resultsDF[1,5] > 0.05) {
      redIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is not equal to condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "greater than" & values$resultsDF[1,5] <= 0.05) {
      redIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is greater than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "greater than" & values$resultsDF[1,5] > 0.05) {
      redIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is greater than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "less than" & values$resultsDF[1,5] <= 0.05) {
      redIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is less than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "less than" & values$resultsDF[1,5] > 0.05) {
      redIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is less than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "not tested") {
      redIntConclusion <- paste0("You opted to not conduct hypothesis testing of red intensity.")
      redIntInterpret <- paste0("You opted to not conduct hypothesis testing of red intensity.")
    }
    if(values$resultsDF[2,2] == "not equal to" & values$resultsDF[2,5] <= 0.05){
      greenIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is not equal to condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "not equal to" & values$resultsDF[2,5] > 0.05) {
      greenIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is not equal to condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "greater than" & values$resultsDF[2,5] <= 0.05) {
      greenIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is greater than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "greater than" & values$resultsDF[2,5] > 0.05) {
      greenIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is greater than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "less than" & values$resultsDF[2,5] <= 0.05) {
      greenIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is less than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "less than" & values$resultsDF[2,5] > 0.05) {
      greenIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is less than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "not tested") {
      greenIntConclusion <- paste0("You opted to not conduct hypothesis testing of green intensity.")
      greenIntInterpret <- paste0("You opted to not conduct hypothesis testing of green intensity.")
    }
    if(values$resultsDF[3,2] == "not equal to" & values$resultsDF[3,5] <= 0.05){
      blueIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is not equal to condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "not equal to" & values$resultsDF[3,5] > 0.05) {
      blueIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is not equal to condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "greater than" & values$resultsDF[3,5] <= 0.05) {
      blueIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is greater than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "greater than" & values$resultsDF[3,5] > 0.05) {
      blueIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is greater than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "less than" & values$resultsDF[3,5] <= 0.05) {
      blueIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is less than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "less than" & values$resultsDF[3,5] > 0.05) {
      blueIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is less than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "not tested") {
      blueIntConclusion <- paste0("You opted to not conduct hypothesis testing of blue intensity.")
      blueIntInterpret <- paste0("You opted to not conduct hypothesis testing of blue intensity.")
    }
    if(values$resultsDF[4,2] == "not equal to" & values$resultsDF[4,5] <= 0.05){
      redAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is not equal to condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "not equal to" & values$resultsDF[4,5] > 0.05) {
      redAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is not equal to condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "greater than" & values$resultsDF[4,5] <= 0.05) {
      redAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is greater than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "greater than" & values$resultsDF[4,5] > 0.05) {
      redAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is greater than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "less than" & values$resultsDF[4,5] <= 0.05) {
      redAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is less than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "less than" & values$resultsDF[4,5] > 0.05) {
      redAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is less than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "not tested") {
      redAreaConclusion <- paste0("You opted to not conduct hypothesis testing of red area.")
      redAreaInterpret <- paste0("You opted to not conduct hypothesis testing of red area.")
    }
    if(values$resultsDF[5,2] == "not equal to" & values$resultsDF[5,5] <= 0.05){
      greenAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is not equal to condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "not equal to" & values$resultsDF[5,5] > 0.05) {
      greenAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is not equal to condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "greater than" & values$resultsDF[5,5] <= 0.05) {
      greenAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is greater than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "greater than" & values$resultsDF[5,5] > 0.05) {
      greenAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is greater than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "less than" & values$resultsDF[5,5] <= 0.05) {
      greenAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is less than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "less than" & values$resultsDF[5,5] > 0.05) {
      greenAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is less than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "not tested") {
      greenAreaConclusion <- paste0("You opted to not conduct hypothesis testing of green area.")
      greenAreaInterpret <- paste0("You opted to not conduct hypothesis testing of green area.")
    }
    if(values$resultsDF[6,2] == "not equal to" & values$resultsDF[6,5] <= 0.05){
      blueAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is not equal to condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "not equal to" & values$resultsDF[6,5] > 0.05) {
      blueAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is not equal to condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "greater than" & values$resultsDF[6,5] <= 0.05) {
      blueAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is greater than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "greater than" & values$resultsDF[6,5] > 0.05) {
      blueAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is greater than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "less than" & values$resultsDF[6,5] <= 0.05) {
      blueAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is less than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "less than" & values$resultsDF[6,5] > 0.05) {
      blueAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is less than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "not tested") {
      blueAreaConclusion <- paste0("You opted to not conduct hypothesis testing of blue area.")
      blueAreaInterpret <- paste0("You opted to not conduct hypothesis testing of blue area.")
    }
    line1 <- paste("Formal intensity conclusions:")
    line2 <- paste0("1. ", redIntConclusion)
    line3 <- paste0("2. ", greenIntConclusion)
    line4 <- paste0("3. ", blueIntConclusion)
    line5 <- paste("Formal area conclusions:")
    line6 <- paste0("1. ", redAreaConclusion)
    line7 <- paste0("2. ", greenAreaConclusion)
    line8 <- paste0("3. ", blueAreaConclusion)
    line9 <- paste0("Informal intensity interpretations:")
    line10 <- paste0("1. ", redIntInterpret)
    line11 <- paste0("2. ", greenIntInterpret)
    line12 <- paste0("3. ", blueIntInterpret)
    line13 <- paste0("Informal area interpretations:")
    line14 <- paste0("1. ", redAreaInterpret)
    line15 <- paste0("2. ", greenAreaInterpret)
    line16 <- paste0("3. ", blueAreaInterpret)
    HTML(paste(line1, line2, line3, line4, line5, line6, line7, line8, line9, line10, line11, line12, line13, line14, line15, line16, sep = '<br/>'))
  }) # end renderUI 'output$extractorInterpret'
  
  output$viewerInterpret <- renderUI({
    validate(need(values$resultsDF, label = "Please view and confirm Statistical Design before you may view interpretations"))
    if(values$resultsDF[1,2] == "not equal to" & values$resultsDF[1,5] <= 0.05){
      redIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is not equal to condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "not equal to" & values$resultsDF[1,5] > 0.05) {
      redIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is not equal to condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "greater than" & values$resultsDF[1,5] <= 0.05) {
      redIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is greater than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "greater than" & values$resultsDF[1,5] > 0.05) {
      redIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is greater than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "less than" & values$resultsDF[1,5] <= 0.05) {
      redIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is less than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "less than" & values$resultsDF[1,5] > 0.05) {
      redIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red intensity is less than condition ", input$img2Condition, "'s.")
      redIntInterpret <- paste0("BASIN is ", values$resultsDF[1,8]*100, "% confident that ", input$img1Condition, "'s mean red intensity is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[1,2] == "not tested") {
      redIntConclusion <- paste0("You opted to not conduct hypothesis testing of red intensity.")
      redIntInterpret <- paste0("You opted to not conduct hypothesis testing of red intensity.")
    }
    if(values$resultsDF[2,2] == "not equal to" & values$resultsDF[2,5] <= 0.05){
      greenIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is not equal to condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "not equal to" & values$resultsDF[2,5] > 0.05) {
      greenIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is not equal to condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "greater than" & values$resultsDF[2,5] <= 0.05) {
      greenIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is greater than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "greater than" & values$resultsDF[2,5] > 0.05) {
      greenIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is greater than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "less than" & values$resultsDF[2,5] <= 0.05) {
      greenIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is less than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "less than" & values$resultsDF[2,5] > 0.05) {
      greenIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green intensity is less than condition ", input$img2Condition, "'s.")
      greenIntInterpret <- paste0("BASIN is ", values$resultsDF[2,8]*100, "% confident that ", input$img1Condition, "'s mean green intensity is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[2,2] == "not tested") {
      greenIntConclusion <- paste0("You opted to not conduct hypothesis testing of green intensity.")
      greenIntInterpret <- paste0("You opted to not conduct hypothesis testing of green intensity.")
    }
    if(values$resultsDF[3,2] == "not equal to" & values$resultsDF[3,5] <= 0.05){
      blueIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is not equal to condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "not equal to" & values$resultsDF[3,5] > 0.05) {
      blueIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is not equal to condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "greater than" & values$resultsDF[3,5] <= 0.05) {
      blueIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is greater than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "greater than" & values$resultsDF[3,5] > 0.05) {
      blueIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is greater than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "less than" & values$resultsDF[3,5] <= 0.05) {
      blueIntConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is less than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "less than" & values$resultsDF[3,5] > 0.05) {
      blueIntConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue intensity is less than condition ", input$img2Condition, "'s.")
      blueIntInterpret <- paste0("BASIN is ", values$resultsDF[3,8]*100, "% confident that ", input$img1Condition, "'s mean blue intensity is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[3,2] == "not tested") {
      blueIntConclusion <- paste0("You opted to not conduct hypothesis testing of blue intensity.")
      blueIntInterpret <- paste0("You opted to not conduct hypothesis testing of blue intensity.")
    }
    if(values$resultsDF[4,2] == "not equal to" & values$resultsDF[4,5] <= 0.05){
      redAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is not equal to condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "not equal to" & values$resultsDF[4,5] > 0.05) {
      redAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is not equal to condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "greater than" & values$resultsDF[4,5] <= 0.05) {
      redAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is greater than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "greater than" & values$resultsDF[4,5] > 0.05) {
      redAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is greater than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "less than" & values$resultsDF[4,5] <= 0.05) {
      redAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is less than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "less than" & values$resultsDF[4,5] > 0.05) {
      redAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean red area is less than condition ", input$img2Condition, "'s.")
      redAreaInterpret <- paste0("BASIN is ", values$resultsDF[4,8]*100, "% confident that ", input$img1Condition, "'s mean red area is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[4,2] == "not tested") {
      redAreaConclusion <- paste0("You opted to not conduct hypothesis testing of red area.")
      redAreaInterpret <- paste0("You opted to not conduct hypothesis testing of red area.")
    }
    if(values$resultsDF[5,2] == "not equal to" & values$resultsDF[5,5] <= 0.05){
      greenAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is not equal to condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "not equal to" & values$resultsDF[5,5] > 0.05) {
      greenAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is not equal to condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "greater than" & values$resultsDF[5,5] <= 0.05) {
      greenAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is greater than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "greater than" & values$resultsDF[5,5] > 0.05) {
      greenAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is greater than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "less than" & values$resultsDF[5,5] <= 0.05) {
      greenAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is less than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "less than" & values$resultsDF[5,5] > 0.05) {
      greenAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean green area is less than condition ", input$img2Condition, "'s.")
      greenAreaInterpret <- paste0("BASIN is ", values$resultsDF[5,8]*100, "% confident that ", input$img1Condition, "'s mean green area is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[5,2] == "not tested") {
      greenAreaConclusion <- paste0("You opted to not conduct hypothesis testing of green area.")
      greenAreaInterpret <- paste0("You opted to not conduct hypothesis testing of green area.")
    }
    if(values$resultsDF[6,2] == "not equal to" & values$resultsDF[6,5] <= 0.05){
      blueAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is not equal to condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "not equal to" & values$resultsDF[6,5] > 0.05) {
      blueAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is not equal to condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is NOT significantly different from ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "greater than" & values$resultsDF[6,5] <= 0.05) {
      blueAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is greater than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "greater than" & values$resultsDF[6,5] > 0.05) {
      blueAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is greater than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is NOT significantly greater than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "less than" & values$resultsDF[6,5] <= 0.05) {
      blueAreaConclusion <- paste0("There is sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is less than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "less than" & values$resultsDF[6,5] > 0.05) {
      blueAreaConclusion <- paste0("There is not sufficient evidence to support the claim that condition ", input$img1Condition, "'s mean blue area is less than condition ", input$img2Condition, "'s.")
      blueAreaInterpret <- paste0("BASIN is ", values$resultsDF[6,8]*100, "% confident that ", input$img1Condition, "'s mean blue area is NOT significantly less than ", input$img2Condition, "'s.")
    } else if(values$resultsDF[6,2] == "not tested") {
      blueAreaConclusion <- paste0("You opted to not conduct hypothesis testing of blue area.")
      blueAreaInterpret <- paste0("You opted to not conduct hypothesis testing of blue area.")
    }
    line1 <- paste("Formal intensity conclusions:")
    line2 <- paste0("1. ", redIntConclusion)
    line3 <- paste0("2. ", greenIntConclusion)
    line4 <- paste0("3. ", blueIntConclusion)
    line5 <- paste("Formal area conclusions:")
    line6 <- paste0("1. ", redAreaConclusion)
    line7 <- paste0("2. ", greenAreaConclusion)
    line8 <- paste0("3. ", blueAreaConclusion)
    line9 <- paste0("Informal intensity interpretations:")
    line10 <- paste0("1. ", redIntInterpret)
    line11 <- paste0("2. ", greenIntInterpret)
    line12 <- paste0("3. ", blueIntInterpret)
    line13 <- paste0("Informal area interpretations:")
    line14 <- paste0("1. ", redAreaInterpret)
    line15 <- paste0("2. ", greenAreaInterpret)
    line16 <- paste0("3. ", blueAreaInterpret)
    HTML(paste(line1, line2, line3, line4, line5, line6, line7, line8, line9, line10, line11, line12, line13, line14, line15, line16, sep = '<br/>'))
  }) # end renderUI 'output$viewerInterpret'
  
  output$img1.sequence.v <- renderPlot({
    blankSpot <- Image(matrix("white", dim(values$img1)[1], dim(values$img1)[2]))
    EBImage::display(
    EBImage::combine(
      blankSpot, values$img1, blankSpot,
      values$img1.r, values$img1.g, values$img1.b, 
      values$img1.r.th, values$img1.g.th, values$img1.b.th, 
      values$img1.r.clbl, values$img1.g.clbl, values$img1.b.clbl, 
      values$img1.r.pntd, values$img1.g.pntd, values$img1.b.pntd
    ), nx = 3, spacing = 10, margin = 40, method = "raster", all = TRUE
  )
  text(x = (10 + dim(values$img1)[1] + dim(values$img1)[1]/2), y = -30, label = input$img1Condition, adj = c(0.5, 0), col = "black", cex = 1)
  text(x = -10, y = (10 + dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Frame", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
  text(x = -10, y = (20 + 2*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Threshold", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
  text(x = -10, y = (30 + 3*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Label", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
  text(x = -10, y = (40 + 4*dim(values$img1)[2] + dim(values$img1)[2]/2), label = "Outline", adj = c(0.5, 0), col = "black", cex = 1, srt = 90)
  })
  
  output$img1.sequence.v <- renderPlot(img1.all.seq())
  output$img2.sequence.v <- renderPlot(img2.all.seq())
  
  output$boxplot1.v <- renderPlot(boxplot1())
  output$boxplot2.v <- renderPlot(boxplot2())
  output$metadataDT.v <- renderDT(metadataDT())
  output$tTestDT.v <- renderDT({
    validate(need(values$resultsDF, label = "Please view and confirm Stat Design before you may view results table"))
    tTestDT()
    })
  
  output$barplot1 <- renderPlot(barplot1())
  output$barplot2 <- renderPlot(barplot2())
  output$boxplot3 <- renderPlot(boxplot3())
  output$boxplot4 <- renderPlot(boxplot4())
  
  ####################################################### REPORTER MODULE ##########################################################
  
  # Markdown document for full report
  options(tinytex.verbose = TRUE)
  output$downloadFullReport <- downloadHandler(
    filename = function() {
      paste('my_figure', sep = '.', switch(input$format, HTML = 'html', PDF = 'pdf', Word = 'docx'))
    },
    content = function(file) {
      src <- file.path(reportsDir, "fullReport.Rmd")
      # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(expr = setwd(owd))
      file.copy(src, 'fullReport.Rmd', overwrite = TRUE)
      library(rmarkdown)
      out <- render('fullReport.Rmd', switch(
        input$format,
        HTML = html_document(), PDF = pdf_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}) # end shinyServer