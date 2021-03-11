# Program: BASINlite Version 1.0 (ui.R)
# Authors: Evgeni Radichev, Tim Hartman
# Date last modified: 16 April 2020
# Description: This program takes a user's 2D, color images as an initial input
# and reads them. It then creates a CSV file that the user can fill in to
# describe the images. The user uploads the file, and the program processes the
# images and CSV file to give the user information and hypothesis test results
# related to those images. The user may then download a report of these results.

#####################
# REQUIRED PACKAGES #
#####################

library(EBImage)                                                                # To read and display images (https://www.rdocumentation.org/packages/EBImage)
# library(ijtiff)                                                               # To read 3D TIFF images (https://www.rdocumentation.org/packages/ijtiff)
library(purrr)                                                                  # For 'split' function in server.R (https://www.rdocumentation.org/packages/purrr)
library(plyr)                                                                   # For 'ldply' function in server.R (https://www.rdocumentation.org/packages/plyr)
library(shiny)                                                                  # For various functions in UI and server (https://www.rdocumentation.org/packages/shiny)
library(shinyBS)                                                                # For collapsePanels and Modals in UI and server (https://www.rdocumentation.org/packages/shinyBS)
library(shinyjs)                                                                # For various functions including hiding/disabling buttons (https://www.rdocumentation.org/packages/shinyjs)
library(shinydashboard)                                                         # For value boxes (https://www.rdocumentation.org/packages/shinydashboard)
library(shinycssloaders)                                                        # For 'withSpinner' functions in ui.R (https://www.rdocumentation.org/packages/shinycssloaders)
library(shinythemes)                                                            # For shinytheme('spacelab') UI theme in ui.R (https://www.rdocumentation.org/packages/shinythemes)
library(shinyWidgets)                                                           # "Custom input controls and UI components" (https://www.rdocumentation.org/packages/shinyWidgets)
library(DT)                                                                     # For datatable functions in server.R (https://www.rdocumentation.org/packages/DT)
library(stringi)                                                                # For 'stri_...' functions in server.R (https://www.rdocumentation.org/packages/stringi)
#library(reshape2)                                                              # For 'melt' function (retired package, https://www.rdocumentation.org/packages/reshape2)
library(ggpubr)                                                                 # For publish-quality plots ('ggboxplot' etc, https://www.rdocumentation.org/packages/ggpubr)
# library(ggsci)                                                                # For journal-specific palettes in ggpubr plots (https://www.rdocumentation.org/packages/ggsci)
library(tcltk)                                                                  # For 'tclvalue' functions in server.R (https://www.rdocumentation.org/packages/tcltk)
# library(gridExtra)                                                            # For arranging plots (https://www.rdocumentation.org/packages/gridExtra)
# library(RBioFormats)                                                          # For image metadata extraction (requires Java 8, https://github.com/aoles/RBioFormats)
library(autothresholdr)                                                         # For auto-thresholding function 'mask' in server.R (https://www.rdocumentation.org/packages/autothresholdr)

######################
# INITIAL CONDITIONS #
######################

# Code to limit the number of files in fileInput("uploadImages")

# js <- "
# $(document).ready(function(){
# document.getElementById('uploadImages').addEventListener('change', function() { 
#   if(document.getElementById('uploadImages').files.length == 2) {             # Change '...files.length...' to whatever max file count you want
#     return true;
#   } else {
#     alert('Please upload only n images of PNG, JPEG, and/or TIFF format');    # Change 'n' to whatever max file count you want
#     this.value = '';
#     return false;
#   } 
# })
# })"

########
# BODY #
########

ui <- fluidPage(theme = shinytheme("spacelab"),
  useShinyjs(),                                                                 # This is required in order to use shinyjs functions
  tags$head(tags$style(HTML('.modal-lg {width: 90%;}'))),                       # Changes the size of all bsModal windows sized 'large'
  tags$style(".fa-check {color:green}"),                                        # Changes color of all font-awesome check icons to green
  navbarPage(                                                                   # Puts a bar at the top that user can use for backwards navigation
    title = "BASINlite", id = "modulePanel", position = "static-top",
    collapsible = TRUE, selected = "Get Started", windowTitle = "tryBASIN",
    tabPanel(
      title = "Get Started",
      tags$h1("Welcome to BASINlite!", align = "center",
              style = "position: relative; top: 0; width: 100%"),
      fillRow(
        actionButton(inputId="getOntoBIDSdata", label="Get OntoBIDS Data..."),  # Button only opens a popup at this time
        div(
          align = "right", 
          actionButton(inputId="tryBASINcloud", label="Try BASINcloud..."))),   # Button only opens a popup at this time
      br(), br(),
      div(
        align = "center",
        actionButton(
          inputId = "chooseProjDir", label = "Start New Project",               # This button allows the user to choose a custom folder for the BASIN output
          style = "height:200px; width:200px; border-radius:50%; 
          font-size:150%;"),                                                    # Buttons are made circular using 'border-radius:50%'
        h3("OR"),
        actionButton(
        inputId = "gotoUpload", label = div("Quick Run"), 
        style = "height:100px; width:100px; border-radius:50%")                 # Simply a hyperlink to Upload page
        ) # end div
      ), # end tabPanel "Get Started"
    tabPanel(title = "Upload",
      tags$h1("Upload 2D Images", align = "center", 
              style = "position: relative; top: 0; width: 100%"),
      fileInput(
        inputId = "uploadImages", label = "", multiple = TRUE, width = "100%",  # Max file count can be changed with code above
        buttonLabel = "Browse Files", placeholder = "Drag and drop here"),
      uiOutput(outputId = "statDesignUpload")                                   # This is the Stat Design collapsePanel 
    ), # end tabPanel "Upload"
    tabPanel(title = "Extractor",
      tags$h3("Extraction & Pre-processing", align = "center", 
              style = "position: relative; top: 0; width: 100%"),
      tags$img(                                                                 # This is the image just below the 'Extractor' header
        src = "extractor1.png",
        style="display:block; margin-left:auto; margin-right:auto; width:75%"),
      br(),
      sidebarLayout(
        sidebarPanel(width = 4,
          selectInput(
            inputId="thresh.auto", label=HTML("Threshold method<sup>2</sup>:"), # The HTML label allows for a superscript number
            choices = c(
              "IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", 
              "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu", 
              "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen"), 
            selected = "Otsu", width = "100%", multiple = FALSE),
          selectInput(
            inputId="mlThresh", label=HTML("ML-based Segmentation<sup>3</sup>:"),
            choices = c("None","cellpose","U-net","Upload"),
            selected = "None", width = "100%", multiple = FALSE
          ),
          conditionalPanel(condition = "input.mlThresh == 'Upload'",
                           h5("Note: Your model must return segmentation masks for the input images in order for BASIN to threshold them properly."),
                           fileInput("ml.upload", label = "Upload Your Image Mask Model (h5 format)")),
          div(style = "display: inline-block; vertical-align: center;",         # Allows button and checkmark to be aligned side-by-side
          actionButton(inputId = "confirmThresh", width = "100%",
                       label = "Threshold All Images")),
          div(style = "display: inline-block; vertical-align: center;",         # Allows button and checkmark to be aligned side-by-side
          conditionalPanel("input.confirmThresh", icon("fas fa-check"))),       # Gives user a checkmark indicating that the button was pressed
          textOutput("threshMethodResult"),
          hr(),
          actionButton(inputId = "previewBoxplots", width = "100%",             # Opens a popup window
                       label = "Boxplots & Summary"),
          hr(),
          actionButton("statResults", label="Analysis Results", width="100%"),  # Opens a popup window
          hr(),
          actionButton("gotoViewer", label="Go To Viewer", width="100%"),       # Just a button to open the Viewer tabPanel
          bsModal(
            id = "previewBoxplotsPopup", title = "Boxplots & Summary", 
            trigger = "previewBoxplots", size = "large",
            column(width = 6, 
                   withSpinner(plotOutput("boxplot1", height = "400px"))),      # 'withSpinner' gives user a visual aid to see that something is coming
            column(width = 6, 
                   withSpinner(plotOutput("boxplot2", height = "400px"))),
            column(width = 6, 
                   withSpinner(plotOutput("intensityImgPlot", height="400px"))),
            column(width = 6,
                   withSpinner(plotOutput("areaImgPlot", height = "400px"))),
            div(style = "font-size:80%",
              withSpinner(DTOutput("metadataDT", width = "100%")))
          ), # end bsModal 'previewBoxplotsPopup'
          bsModal(
            id = "statResultsPopup", title = "Analysis Results", 
            trigger = "statResults", size = "large",
            bsCollapse(
              id = "statResultsPanels", multiple = TRUE,
              open = list("1. Graphs", "2. Stat Result Tables",                 # Tells bsCollapse function which panels should be open by default
                          "3. Draft Interpretations"),
              bsCollapsePanel(
                style = "warning",
                title = "1. Graphs",
                withSpinner(plotOutput(outputId="resultPlot1", height=400)),    # A group of boxplots for object mean intensity data
                withSpinner(plotOutput(outputId="resultPlot2", height=400))),   # A group of boxplots for object area data 
              bsCollapsePanel(
                style = "warning", title = "2. Stat Results Table",
                h5("Mean Object Intensity T-test Results"),
                tableOutput(outputId = "meantTestTable"),
                h5("Object Area T-test Results"),
                tableOutput(outputId = "areatTestTable")),
              bsCollapsePanel(
                style = "warning", title = "3. Draft Interpretations",
                htmlOutput("extractorInterpret"))
            ), # end bsCollapse
            actionButton(inputId = "closestatResults", label = "Save & Close"), # 'Save' assures the user that what they see is what will be. There isn't any 'save' function here
            tags$head(
              tags$style("#statResultsPopup .modal-footer{display:none}"))      # Hides the default 'Close' button so that 'Save & Close' is used instead
          ) # end bsModal for Stat Design
        ), # end sidebarPanel
        mainPanel(width = 8,
          conditionalPanel("input.confirmThresh",                               # Once the 'Threshold All Images' button is pressed, this panel will be shown
                           uiOutput("conditionalImageInput")),                  # A selectInput box to choose an image to view
          withSpinner(plotOutput("extractorImagePlot", height = "500px"))       # A plot object that shows the image sequence for a particular uploaded image
        ) # end mainPanel
      ), # end sidebarLayout
      "Extractor references:",
      br(),
      HTML("<sup>1</sup>Messerli SM, Hoffman MM, Gnimpieba EZ, Bhardwaj RD.     
      Therapeutic targeting of ptk7 is cytotoxic in atypical teratoid rhabdoid 
      tumors. <i>Molecular Cancer Research: MCR.</i> 2017;15(8):973-983. 
           doi:10.1158/1541-7786.MCR-16-0432."),                                # This is a reference to the original image used in 'extractor.png'
      htmlOutput(outputId = "thresholdReference"),                               # A reactive text output depending on which threshold method is used
      htmlOutput(outputId = "mlSegmentationReference")
    ), # end tabPanel 'extractor'
  tabPanel(title = "Viewer", 
    tags$h1("View Results", align = "center", 
            style = "position: relative; top: 0; width: 100%"),
    actionButton(inputId = "confirmViewerBttn", label = "Continue to Reporter"),# Just a button to open the Reporter tabPanel
    hr(),
    bsCollapse(
      open = NULL,  multiple = TRUE,
      bsCollapsePanel(
        title = "1. Introduction", style = "primary",
        "The Bioinformatic Analysis, Statistic, and Image Comparison (BASIN) 
        app extracts object feature data and statistics from fluorescent 
        micrographs. The user uploads images plus a custom analysis table which 
        contains image and experiment information. The user then chooses the 
        desired threshold method for each color frame and runs the analyses for 
        each image. BASIN runs the analyses and returns graphs and t-test 
        results which the user may then incorporate in his or her paper. Coming 
        soon, the analysis parameters and results may also be published in 
        OntoBIDS."),
      bsCollapsePanel(
        title = "2. Uploaded Images", style = "primary",
        # htmlOutput(outputId = "uploadText"),                                  # This is a reactive step-by-step procedure that is displayed as text for the user
        withSpinner(plotOutput(outputId="origImgsViewer", height="600px"))),    # Shows all the uploaded images
      bsCollapsePanel(style = "primary",
        title = "3. Extraction & Pre-processing",
        htmlOutput(outputId = "extractorText"),                                 # This is a reactive step-by-step procedure that is displayed as text for the user
        bsCollapsePanel(style = "danger",
          title = div(style = "color: black", "Object Extraction Sequence"),    # Display 2 original images, then associated boxplots, then auto-threshold workflow composite
          fluidRow(
            column(width = 6, withSpinner(plotOutput(outputId="boxplot1.v"))),  # A boxplot of object mean intensity data
            column(width = 6, withSpinner(plotOutput(outputId="boxplot2.v")))), # A boxplot of object area data
          div(align = "center", h5("All Images' Extraction Sequences")),
          withSpinner(plotOutput("viewerImagePlot", height = "600px"))          # Shows all the image sequences
        ), # end bsCollapsePanel
        bsCollapsePanel(style = "danger",
          title = div(style = "color: black", "Raw Image Data"),
          fluidRow(
            column(width = 6, withSpinner(plotOutput(outputId = "barplot1"))),  # A barplot of sum object intensity for each image and frame
            column(width = 6, withSpinner(plotOutput(outputId = "barplot2")))), # A barplot of object counts for each image and frame
          DTOutput("metadataDT.v")                                              # A table of raw statistics for each image and frame
        ) # end bsCollapsePanel "Raw image Data"
      ), # end bsCollapsePanel '3. Extraction...'
      bsCollapsePanel(
        title = "4. Analysis Results", style = "primary",
        fluidRow(
          column(width = 6, withSpinner(plotOutput("boxplot3", height = 400))), # A boxplot of object mean intensities with p-values
          column(width = 6, withSpinner(plotOutput("boxplot4", height = 400)))), # A boxplot of object areas with p-values
        # DTOutput(outputId = "tTestDT.v"),                                       # A table of t-test results for each image and frame
        h5("Object Mean Intensity T-test Results"),
        tableOutput(outputId = "intensityTestDT.v"), # A boxplot of object mean intensities with p-values
        h5("Object Mean Area T-test Results"),
        tableOutput(outputId = "areaTestDT.v")
      ),
      bsCollapsePanel(
        title = "5. Interpretations", style = "primary",
        htmlOutput("viewerInterpret")),
      bsCollapsePanel(
        title = "6. References", style = "primary",
        "Package Citations", 
        br(),
        "[1] Yihui Xie, Joe Cheng and Xianying Tan (2019). DT: A Wrapper of the 
        JavaScript Library 'DataTables'. R package version 0.7. 
        https://CRAN.R-project.org/package=DT",
        br(),
        "[2] Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and 
        Wolfgang Huber (2010): EBImage - an R package for image processing with 
        applications to cellular phenotypes. Bioinformatics, 26(7), pp. 979-981, 
        10.1093/bioinformatics/btq046", 
        br(),
        "[3] H. Wickham. ggplot2: Elegant Graphics for Data Analysis. 
        Springer-Verlag New York, 2016.", 
        br(),
        "[4] Alboukadel Kassambara (2019). ggpubr: 'ggplot2' Based Publication 
        Ready Plots. R package version 0.2.1. 
        https://CRAN.R-project.org/package=ggpubr", 
        br(),
        "[5] Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data 
        Analysis. Journal of Statistical Software, 40(1), 1-29. URL 
        http://www.jstatsoft.org/v40/i01/.", 
        br(),
        "[6] Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan 
        McPherson (2019). shiny: Web Application Framework for R. R package 
        version 1.3.2. https://CRAN.R-project.org/package=shiny", 
        br(),
        "[7] Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for 
        Shiny. R package version 0.61. 
        https://CRAN.R-project.org/package=shinyBS", 
        br(),
        "[8] Andras Sali (2017). shinycssloaders: Add CSS Loading Animations to 
        'shiny' Outputs. R package version 0.2.0. 
        https://CRAN.R-project.org/package=shinycssloaders", 
        br(),
        "[9] Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: 
        Create Dashboards with 'Shiny'. R package version 0.7.1. 
        https://CRAN.R-project.org/package=shinydashboard", 
        br(),
        "[10] Winston Chang (2018). shinythemes: Themes for Shiny. R package 
        version 1.1.2. https://CRAN.R-project.org/package=shinythemes",
        br(),
        "[11] Victor Perrier, Fanny Meyer and David Granjon (2019). 
        shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.4.9. 
        https://CRAN.R-project.org/package=shinyWidgets",
        br(),
        "[12] Gagolewski M. and others (2019). R package stringi: Character 
        string processing facilities. 
        http://www.gagolewski.com/software/stringi/.",
        br(),
        "[13] Hadley Wickham (2007). Reshaping Data with the reshape Package. 
        Journal of Statistical Software, 21(12), 1-20. URL
        http://www.jstatsoft.org/v21/i12/.",
        br(),
        "[14] R Core Team (2019). R: A language and environment for statistical 
        computing. R Foundation for Statistical Computing, Vienna, Austria. 
        URL https://www.R-project.org/.",
        br(),
        "[15] Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for 
        'Grid' Graphics. R package version 2.3.
        https://CRAN.R-project.org/package=gridExtra"
      ) # end bsCollapsePanel '6. References'
    ) # end bsCollapse
  ), # end tabPanel 'Viewer'
  tabPanel(title = "Reporter", 
    tags$h1("Report Results", align = "center", 
            style = "position: relative; top: 0; width: 100%"),
    radioButtons('format', 'Document format', c('HTML', 'PDF', 'Word'),         # Lets the user choose which format in which to download the full report
                 inline = TRUE, selected = 'HTML'),
    downloadButton(outputId = "downloadFullReport", label = "Generate Report"), # Downloads the full report to a local, user-specified folder
    br(),
    "Note that it may take up to a few minutes to generate the full report 
    depending on the total number of uploaded images."
  ) # end tabPanel 'reporter'
), # end navbarPage
bsCollapse(
  open = "Action Log", multiple = TRUE,
  bsCollapsePanel(
    title = "Action Log", style = "danger",
    htmlOutput(outputId = "actionLog1"),
    htmlOutput(outputId = "actionLog2"),
    htmlOutput(outputId = "actionLog3"),
    htmlOutput(outputId = "actionLog12"),
    htmlOutput(outputId = "actionLog13"),
    htmlOutput(outputId = "actionLog4"),
    htmlOutput(outputId = "actionLog5"),
    htmlOutput(outputId = "actionLog6"),
    htmlOutput(outputId = "actionLog7"),
    htmlOutput(outputId = "actionLog8"),
    htmlOutput(outputId = "actionLog9"),
    htmlOutput(outputId = "actionLog10"),
    htmlOutput(outputId = "actionLog11"),
    htmlOutput(outputId = "actionLog14")
  ), # end bsCollapsePanel 'Action Log'
  bsCollapsePanel(
    title = "Learn more about BASIN...", style = "primary",
    h3("BASIN Stats (coming soon)"),
    div(
      align = "center",
      valueBoxOutput(outputId = "authorsUsed"),                                 # (not active yet) Shows the user how many authors have used BASIN in the past
      valueBoxOutput(outputId = "imagesAnlzd"),                                 # (not active yet) Shows the user how many images have been analyzed in the past
      valueBoxOutput(outputId = "reportsPublsd")),                              # (not active yet) Shows the user how many reports have been published in the past
    br(),
    fillRow(
      div(align = "left", 
          actionButton(inputId = "tutorialPopup", label = "Tutorials...")),     # Opens a popup window with BASIN instructions
      div(align = "center", 
          actionButton(inputId = "videosPopup", label = "Videos...")),          # Opens a popup window with BASIN YouTube videos (none yet)
      div(align = "right", 
          actionButton(inputId = "aboutUsPopup", label = "About Us..."),        # Opens a popup window with BASIN author & contact information
          actionLink(
            inputId = "BLPLink", 
            label = "Bioinformatics Learning Portal", 
            onclick="window.open('http://brin.usd.edu/Bioinformatics_Portal/')")# Invalid link as of 15 April 2020
      ) # end div align = 'right'
    ), # end fillRow
    br(), br(),
    bsModal(
      id = "tutorialWindow", title = "Tutorial", trigger = "tutorialPopup",
      size = "large",
      bsCollapse(open = NULL, multiple = TRUE,
        bsCollapsePanel(
          title = "A. Get Started", style = "primary",
          "1. If you're new to BASIN, then you may want to use tryBASIN 
          (BASINcloud) first.", br(),
          "2. Choose where you want the results to go: either a folder of your 
          choice with 'Start New Project' or the local folder containing this 
          Shiny app ('Quick Run').", br(),
          "3. You'll be taken to the Upload page to insert your images."
          # ,
          # HTML('<iframe width="560" height="315"                              # This code for YouTube embed reference only. 
            # src="https://www.youtube.com/embed/7KJjVMqNIgA" frameborder="0"   # It's not actually a BASIN video.
            # allow="accelerometer; autoplay; encrypted-media; gyroscope; 
            # picture-in-picture" allowfullscreen></iframe>')
        ),
        bsCollapsePanel(
          title = "B. Upload", style = "primary",
          "1. Browse for your (2D only) images or drag and drop to upload 
          them to BASIN.", br(),
          "2. If the stains that you used were red, green, or blue colored,
          then you may enter those names if you wish.",
          "3. In order for images to be grouped, you need to complete a Stat 
          Design spreadsheet. Click 'Get Stat Design Template' to have a 
          starter template downloaded to the folder of your choice.", br(),
          "4. Complete the spreadsheet. Required columns: 'experiment' 
          (integers only), biocondition ('control' or 'test' only) and 
          'alternative' ('not=', '>', or '<') which is the alternative 
          hypothesis. You need one alternative hypothesis per experiment.",br(),
          bsCollapsePanel(
            style = "danger", title = 
              div(style = "color:black", "Hypotheses explained"),               # Makes title color black to make it easier to see
            "By default, the null hypothesis for all tests is equal to zero. 
            That is, there is no difference between the control condition's 
            mean object intensity/area and the test condition's mean object 
            intensity/area. (This does not have to be your experiment's 
            claim!)",
            br(), br(),
            "For alternate hypotheses (Ha):", 
            br(),
            "a. 'not=' ('not equal to') means BASIN will test the claim that 
            the control condition's mean object intensity or area is simply 
            different from the test condition's. In other words, whether the 
            control condition's mean is greater or less than the test 
            condition's is irrelevant to your experiment.",
            br(),
            "b. '>' ('greater than') means BASIN will test the claim that the 
            control condition's mean object intensity or area is greater than 
            the test condition's.",
            br(),
            "c. '<' ('less than') means BASIN will test the claim that the 
            control condition's mean object intensity or area is less than the 
            test condition's.",
            br(),
            "Important note: These tests can only draw conclusions about three 
            colors: red, green, and blue. Since some stains emit color that is 
            a mix of red, green, and/or blue, use care when drawing conclusions 
            about stains. For example, if a stain emits a violet color, you 
            cannot use only red or only blue to draw your conclusions about 
            that violet stain."
          ), # end bsCollapsePanel 'Hypotheses explained..."
          "5. When you've finished and saved the spreadsheet (as a CSV), upload 
          it back into BASIN", br(),
          "6. Click 'Confirm Upload' to go to Extractor."
        ),
        bsCollapsePanel(
          title = "C. Extract", style = "primary",
          "1. Choose a threshold method that will be used for all images.",br(),
          "2. Click 'Threshold All Images' to begin extraction sequence.", br(),
          "3. You may view each image's sequence if you desire.", br(),
          "4. Click 'Boxplots & Summary' to view pre-thresholded image data in 
          table and post-thresholded image and object data in the plots and 
          table.", br(),
          "5. Click 'Analysis Results' to complete Welch two-sample t-testing 
          of your experiment sets. You may view plots, tables, and draft 
          interpretations of each t-test.", br(),
          "6. Click 'Go To Viewer' when you are ready to view all of your 
          results"
        ),
        bsCollapsePanel(
          title = "D. View", style = "primary",
          "1. You may continue to the Reporter page to download the full 
          results report, or you may view the results within the app first.", 
          br(),
          "2. In Introduction, you may read BASIN's background information from 
          which you may include in your paper", br(),
          "3. In 'Uploaded Images,' you may see all of your uploaded images 
          side-by-side", br(),
          "4. In 'Extraction & Pre-processing,' you may confirm your 
          experiments' conditions, view sequences, and see image- and object-
          level data before the t-testing", br(),
          "5. In 'Analysis Results,' you may view results after your experiment 
          sets were t-tested", br(),
          "6. In 'Interpretations,' you may see formal interpretations for 
          the t-tests which you may add to your paper",
          "7. The 'References' panel cites all of the R packages used in BASIN's 
          development"
        ),
        bsCollapsePanel(
          title = "E. Report", style = "primary",
          "1. Choose the format in which you would like your report downloaded", 
          br(),
          "2. Click 'Generate Report' to begin report compilation", br(),
          "3. Choose the folder into which your report will be downloaded"
        )
        # bsCollapsePanel(
        #   title = "F. Publish", style = "primary",
        #   "1.", br(),
        #   "2.", br(),
        #   "3.", br(),
        #   "4.", br(),
        #   "5.", br()
        # )
      ) # end bsCollapse
    ), # end bsModal "tutorialWindow"
    bsModal(
      id = "videosWindow", title = "Videos", trigger = "videosPopup", 
      size = "small",
      "Videos coming soon!"
    ), # end bsModal "videosWindow"
    bsModal(
      id = "aboutUsWindow", title = "About Us", trigger = "aboutUsPopup",
      size = "large",
      h2("What is BASIN?"),
      "Despite available high-quality imaging technologies, many still rely on 
      visual observation to compare images. Image comparison is used for protein 
      expression, therapy testing, and drug response assessment. The image data 
      repository Open-i stores over ten million image comparison experiments 
      from peer-reviewed publications. The Bioinformatic Analysis, Statistic, 
      and Image Comparison (BASIN) toolkit, an extension of our Ontology-Driven 
      BioImage Dataset Discovery System (OntoBIDS), aims to enhance image 
      comparison using a non-biased, computational method. BASIN would allow 
      users to extract, analyze, and view a wide range of image data to improve 
      research conclusions and statistical strength. This data currently 
      includes object positions, size, area, and fluorescence intensities. 
      Researchers could then run tests on data of interest and publish these 
      results in a reproducible fashion. The toolkit's power lies in being 
      interactive yet able to process multiple images semi-automatically 
      combining data acquisition and statistics technologies into one 
      easy-to-use application.",
      h2("Developers"),
      "Tim Hartman, Research Assistant, University of South Dakota", br(),
      "Evgeni Radichev, Research Assistant, University of South Dakota", br(), 
      "Carrie Minette, Research Assistant, University of South Dakota", br(), 
      "Maria Hoffman, Research Assistant, University of South Dakota", br(), 
      "Etienne Gnimpieba PhD, Research Assistant Professor, University of South 
      Dakota", br(), 
      "Funding provided by the South Dakota Biomedical Research Infrastructure 
      Network (SDBRIN) and BioSystems Networks/Translational Research (BioSNTR)"
    ) # end bsModal "aboutUsWindow"
  ) # end bsCollapsePanel
), # end bsCollapse
tags$footer(                                                                    # tags$footer keeps the text at the bottom of the app regardless of which page is open
  "Copyright © 2021 The University of South Dakota. All rights reserved.",
  align = "center", style = "position: relative; bottom: 0; width: 100%")
) # end ui <- fluidPage(...)