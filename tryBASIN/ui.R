####################################################################################################################################################
##################################################    tryBASIN UI version 12.23.19    ##############################################################
####################################################################################################################################################

library(EBImage)
library(ijtiff)
library(purrr)
library(plyr)
library(shiny)
library(shinyBS)
library(shinyjs)
#library(shinydashboard)
library(shinycssloaders)
#library(shinythemes)
library(shinyWidgets)
library(DT)
library(stringi)
#library(reshape2)
#library(ggplot2)
library(ggpubr) # for publish-quality graphs
# library(ggsci) # for "npg" palette
#library(tcltk) # for project directory
library(gridExtra) # for arranging graphs
# library(RBioFormats) #for image metadata
library(autothresholdr) # for auto-thresholding functions

# Code to limit the number of files in fileInput("upload1")
# js <- "
# $(document).ready(function(){
# document.getElementById('upload1').addEventListener('change', function() { 
#   if(document.getElementById('upload1').files.length == 2) {
#     return true;
#   } else {
#     alert('Please upload only two images of PNG, JPEG, and/or TIFF format');
#     this.value = '';
#     return false;
#   } 
# })
# })"

ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    title = "tryBASIN",
    id = "modulePanel",
    position = "static-top",
    collapsible = TRUE,
    selected = "Upload",
    windowTitle = "tryBASIN",
  tabPanel(title = "Upload",
           tags$head(tags$script(HTML("js"))),
    h3("Upload Your Images", align = "center", style = "position: relative; top: 0; width: 100%"),
    h4(
      actionLink(
        inputId = "tryOurExample", 
        label = "(or try our example images)"), 
      align = "center", 
      style = "position: relative; top: 0; width: 100%"),
    h5("Image size is limited to 10MB per image", align="center"),
    br(),
    # div(align = "center",
    #   fileInput(
    #     inputId = "upload1",
    #     label = "",
    #     multiple = TRUE,
    #     buttonLabel = "Browse...",
    #     placeholder = "...or drag and drop two images",
    #     width = "40%"
    #   ) # end fileInput
    # ), # end div
    fluidRow(
      column(width = 6,
        fileInput(inputId = "upload1",
          label = "Upload condition 1 image:",
          multiple = FALSE,
          buttonLabel = "Browse...",
          placeholder = "...or drag and drop here",
          width = "100%"
        ), 
        plotOutput(outputId = "img1", height = "100px"),
      ),
      column(width = 6,
        fileInput(
          inputId = "upload2",
          label = "Upload condition 2 image:",
          multiple = FALSE,
          buttonLabel = "Browse...",
          placeholder = "...or drag and drop here",
          width = "100%"
        ),
        plotOutput(outputId = "img2", height = "100px"),
      )
    ),
    br(),
    bsCollapse(     
      bsCollapsePanel(
        title = "Optional image identifiers:", style = "primary",
        column(width = 6,
        textInput(inputId = "experimentName", label = "Experiment Name:", value = "Experiment 1"),
        textInput(inputId = "img1Condition", label = "'Condition 1' image's biocondition:", value = "Control"),
        textInput(inputId = "img2Condition", label = "'Condition 2' image's biocondition:", value = "Test")
        ),
        column(width = 6,
        textInput(inputId = "imgsRedStain", label = "Red stain:", value = "Red"),
        textInput(inputId = "imgsGreenStain", label = "Green stain:", value = "Green"),
        textInput(inputId = "imgsBlueStain", label = "Blue stain:", value = "Blue")
        )
      )
    ),
    hr(),
    disabled(actionButton(inputId = "gotoExtractor", label = "Next"))
  ), # end tabPanel 'upload'
  tabPanel(title = "Extractor",
    tags$h3("Extraction & Pre-processing", align = "center", style = "position: relative; top: 0; width: 100%"),
    tags$img(src = "extractor1.png", style="display: block; margin-left: auto; margin-right: auto; width: 75%"),
    br(),
    sidebarLayout(
      sidebarPanel(width = 4,
        selectInput(
          inputId = "thresh.auto", 
          label = HTML("Threshold method<sup>2</sup>:"), 
          choices = c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", 
                      "Mean", "MinErrorI", "Minimum", "Moments", "Otsu", "Percentile", "RenyiEntropy", 
                      "Shanbhag", "Triangle", "Yen"), 
          selected = "Otsu", 
          multiple = FALSE
        ),
        actionButton(inputId = "confirmThresh", label = "Confirm Method"),
        actionButton(inputId = "previewBoxplots", label = "Summary..."),
        hr(),
        disabled(actionButton(inputId = "statDesign", label = "Design of Analysis..."),
                 # conditionalPanel(
                 #   condition = "input.statDesign != '0'",
                   actionButton(inputId = "gotoViewer", label = "Viewer Module")
                 # )
        ),
        bsModal(
          id = "previewBoxplotsPopup",
          title = "Boxplots & Summary", 
          trigger = "previewBoxplots", 
          size = "large",
          column(width = 6, plotOutput("boxplot1", height = "300px")),
          column(width = 6, plotOutput("boxplot2", height = "300px")),
          div(DTOutput("metadataDT", width = "100%"), style = "font-size:80%")
        ), # end bsModal 'previewBoxplots'
        bsModal(
          id = "statDesignPopup", 
          title = "Design of Analysis", 
          trigger = "statDesign", 
          size = "large",
          bsCollapse(
            id = "statDesignPanels",
            open = list("1. Design Statistical Test","2. View Results"), 
            multiple = TRUE,
            bsCollapsePanel(
              style = "warning",
              title = "1. Design Statistical Test",
              fluidRow(
                setSliderColor(color = c("red", "green", "blue"), sliderId = c(1:3)),
                column(width = 4,
                  h4("Red channel", align = "center", style = "color:red"),
                  selectInput(
                    inputId = "r.intensity.altHypothesis", 
                    label = div("Intensity alternate hypothesis (Ha):", style = "color:red"),
                    choices = c("not equal to", "greater than", "less than", "do not test"),
                    selected = "not equal to"
                  ),
                  selectInput(
                    inputId = "r.area.altHypothesis", 
                    label = div("Area Ha:", style = "color:red"),
                    choices = c("not equal to", "greater than", "less than", "do not test"),
                    selected = "not equal to" 
                  ),
                  uiOutput(outputId = "minMaxObjectArea.r"),
                ), # end column
                column(width = 4,
                  h4("Green channel", align = "center", style = "color:green"),
                  selectInput(
                    inputId = "g.intensity.altHypothesis", 
                    label = div("Intensity Ha:", style = "color:green"),
                    choices = c("not equal to", "greater than", "less than", "do not test"),
                    selected = "not equal to" 
                  ),
                  selectInput(
                    inputId = "g.area.altHypothesis", 
                    label = div("Area Ha:", style = "color:green"),
                    choices = c("not equal to", "greater than", "less than", "do not test"),
                    selected = "not equal to" 
                  ),
                  uiOutput(outputId = "minMaxObjectArea.g"),
                ), # end column
                column(width = 4,
                  h4("Blue channel", align = "center", style = "color:blue"),
                  selectInput(
                    inputId = "b.intensity.altHypothesis", 
                    label = div("Intensity Ha:", style = "color:blue"),
                    choices = c("not equal to", "greater than", "less than", "do not test"),
                    selected = "not equal to"  
                  ),
                  selectInput(
                    inputId = "b.area.altHypothesis", 
                    label = div("Area Ha:", style = "color:blue"),
                    choices = c("not equal to", "greater than", "less than", "do not test"),
                    selected = "not equal to" 
                  ),
                  uiOutput(outputId = "minMaxObjectArea.b")
                ) # end column
              ), # end fluidRow
              h6(HTML("By default, sliders are initially set to exclude outliers (defined as >1.5*IQR outside 25% and 75% quantiles)<sup>3</sup>"), align = "center", style = "color:gray")
            ), # end bsCollapsePanel
            bsCollapsePanel(
              style = "warning",
              title = "2. View Results",
              bsCollapsePanel(
                title = "2.1. Result Plots",
                fluidRow(
                  column(width = 6, plotOutput(outputId = "resultPlot1", height = 600)),
                  column(width = 6, plotOutput(outputId = "resultPlot2", height = 600))
                )
              ),
              bsCollapsePanel(
                title = "2.2. Result tables",
                DTOutput(outputId = "tTestDT"),
                h5("Net Intensity and Object Count Differences"),
                tableOutput(outputId = "differenceTable")
              )
            ), # end bsCollapsePanel
            bsCollapsePanel(
              style = "warning", 
              title = "3. Draft Interpretations",
              htmlOutput("extractorInterpret")
            ),
            bsCollapsePanel(
              style = "warning",
              title = "Hypotheses explained...",
              "By default, the null hypothesis for all tests is equal to zero. 
              That is, there is no difference between one condition's mean object intensity/area and the other condition's mean object intensity/area. 
              (This does not have to be your experiment's claim!)",
              br(), br(),
              "For alternate hypotheses (Ha):", 
              br(),
              "a.  'Not equal to' means you claim that condition 1's mean object intensity or area is simply different from condition 2's. 
              (Whether condition 1's mean is greater or less than condition 2's is irrelevant to your experiment.)",
              br(),
              "b.  'Greater than' means you claim that condition 1's mean object intensity or area is greater than condition 2's.",
              br(),
              "c.  'Less than' means you claim that condition 1's mean object intensity or area is less than condition 2's.",
              br(),
              "Important note: These tests can only draw conclusions about three colors. Since some stains emit color between red, green, and blue, 
              use care when drawing conclusions about stains. For example, if a stain emits a violet color, you cannot use only red or only blue 
              to draw your conclusions about that violet stain."
            )
          ), # end bsCollapse
          actionButton(inputId = "closeStatDesign", label = "Save & Close"),
          tags$head(tags$style("#statDesignPopup .modal-footer{ display:none}"))
        ) # end bsModal for Stat Design
      ), # end sidebarPanel
      mainPanel(width = 8,
        uiOutput("viewStain"),
        column(width = 6,
          h6("(biocondition 1)", align = "center", style = "color:gray"),
          plotOutput("img1.sequence", height = "400px"),
        ),
        column(width = 6,
          h6("(biocondition 2)", align = "center", style = "color:gray"),
          plotOutput("img2.sequence", height = "400px")    
        )        
      ) # end mainPanel
    ), # end sidebarLayout
    "Extractor references:",
    br(),
    HTML("<sup>1</sup>Messerli SM, Hoffman MM, Gnimpieba EZ, Bhardwaj RD. Therapeutic targeting of ptk7 is cytotoxic in atypical teratoid rhabdoid tumors. 
         <i>Molecular Cancer Research: MCR.</i> 2017;15(8):973-983. doi:10.1158/1541-7786.MCR-16-0432."),
    htmlOutput(outputId = "thresholdReference")
  ), # end tabPanel 'extractor'
  tabPanel(title = "Viewer", 
    tags$h1("View Results", align = "center", style = "position: relative; top: 0; width: 100%"),
    actionButton(inputId = "confirmViewerBttn", label = "Continue to Reporter"),hr(),
    bsCollapse(
      open = NULL, 
      multiple = TRUE,
      bsCollapsePanel(title = "1. Introduction", style = "primary",
        "The Bioinformatic Analysis, Statistic, and Image Comparison (BASIN) app extracts object feature data and statistics from fluorescent micrographs. 
        The user uploads images plus a custom analysis table which contains image and experiment information. 
        The user then chooses the desired threshold method for each color frame and runs the analyses for each image.
        BASIN runs the analyses and returns graphs and t-test results which the user may then incorporate in his or her paper.
        The analysis parameters and results may also be published in OntoBIDS."
      ), # end bsCollapsePanel 'Introduction'
      bsCollapsePanel(
        title = "2. Uploaded Images", 
        style = "primary",
        column(width = 8, htmlOutput(outputId = "uploadText")),
        column(width = 2,
          h6("(biocondition 1)", align = "center", style = "color:gray"),
          h5(textOutput(outputId = "img1ConditionC"), align = "center"),
          plotOutput(outputId = "img1.v", height = "100px")
        ),
        column(width = 2,
          h6("(biocondition 2)", align = "center", style = "color:gray"),
          h5(textOutput(outputId = "img2ConditionC"), align = "center"),
          plotOutput(outputId = "img2.v", height = "100px") 
        )
      ), # end bsCollapsePanel 'uploaded images'
      bsCollapsePanel(
        title = "3. Extraction & Pre-processing", 
        style = "primary",
        
          htmlOutput(outputId = "extractorText"),
        
            bsCollapsePanel(
              title = "Object Extraction Sequence",
              style = "warning",
              # Display 2 original images, then associated boxplots, then auto-threshold workflow composite
              fluidRow(
                column(width = 6, plotOutput(outputId = "boxplot1.v")),
                column(width = 6, plotOutput(outputId = "boxplot2.v"))
              ),
              plotOutput(outputId = "img1.sequence.v", width = "100%"),
              plotOutput(outputId = "img2.sequence.v", width = "100%")
            ), # end bsCollapsePanel
        
            bsCollapsePanel(
              title = "Raw Image Data",
              style = "warning",
              fluidRow(
                column(width = 6, plotOutput(outputId = "barplot1")),
                column(width = 6, plotOutput(outputId = "barplot2"))
              ),
              DTOutput("metadataDT.v")
            ) # end bsCollapsePanel
        
      ), # end bsCollapsePanel 'extraction...'
      bsCollapsePanel(
        title = "4. Analysis Results", 
        style = "primary",
        DTOutput(outputId = "tTestDT.v"),
        h5("Net Intensity and Object Count Differences"),
        tableOutput(outputId = "differenceTable.v"),
        fluidRow(
          column(width = 6, plotOutput(outputId = "boxplot3", height = 600)),
          column(width = 6, plotOutput(outputId = "boxplot4", height = 600))
        )
      ),
      bsCollapsePanel(
        title = "5. Interpretations", 
        style = "primary",
        htmlOutput("viewerInterpret")
      ),
      bsCollapsePanel(
        title = "6. References", 
        style = "primary",
        "Package Citations", 
        br(),
        "[1] Yihui Xie, Joe Cheng and Xianying Tan (2019). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version
        0.7. https://CRAN.R-project.org/package=DT",
        br(),
        "[2] Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber (2010): EBImage - an R package for image
        processing with applications to cellular phenotypes. Bioinformatics, 26(7), pp. 979-981, 10.1093/bioinformatics/btq046", 
        br(),
        "[3] H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.", 
        br(),
        "[4] Alboukadel Kassambara (2019). ggpubr: 'ggplot2' Based Publication Ready Plots. R package version 0.2.1. https://CRAN.R-project.org/package=ggpubr", 
        br(),
        "[5] Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1),
        1-29. URL http://www.jstatsoft.org/v40/i01/.", 
        br(),
        "[6] Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2019). shiny: Web Application Framework for R.
        R package version 1.3.2. https://CRAN.R-project.org/package=shiny", 
        br(),
        "[7] Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61. https://CRAN.R-project.org/package=shinyBS", 
        br(),
        "[8] Andras Sali (2017). shinycssloaders: Add CSS Loading Animations to 'shiny' Outputs. R package version 0.2.0.
        https://CRAN.R-project.org/package=shinycssloaders", 
        br(),
        "[9] Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'. R package version
        0.7.1. https://CRAN.R-project.org/package=shinydashboard", 
        br(),
        "[10] Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2. https://CRAN.R-project.org/package=shinythemes",
        br(),
        "[11] Victor Perrier, Fanny Meyer and David Granjon (2019). shinyWidgets: Custom Inputs
        Widgets for Shiny. R package version 0.4.9. https://CRAN.R-project.org/package=shinyWidgets",
        br(),
        "[12] Gagolewski M. and others (2019). R package stringi: Character string processing facilities.
        http://www.gagolewski.com/software/stringi/.",
        br(),
        "[13] Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL
        http://www.jstatsoft.org/v21/i12/.",
        br(),
        "[14] R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
        Vienna, Austria. URL https://www.R-project.org/.",
        br(),
        "[15] Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for 'Grid' Graphics. R package version 2.3.
        https://CRAN.R-project.org/package=gridExtra"
      ) # end bsCollapsePanel
    ) # end bsCollapse
  ), # end tabPanel 'Viewer
  tabPanel(title = "Reporter", 
    tags$h1("Report Results", align = "center", style = "position: relative; top: 0; width: 100%"),
    radioButtons('format', 'Document format', c('HTML', 'Word'), inline = TRUE, selected = 'HTML'),
    textInput("reportAuthor", label = "Enter author name(s) for the analysis report."),
    textInput("reportTitle", label = "Enter a project title for your analysis report."),
    downloadButton(outputId = "downloadFullReport", label = "Generate Report"),
    br(),
    "Note that it may take up to a few minutes to generate the full report, depending on the total number
                of images uploaded."
  ) # end tabPanel 'reporter'
), # end navbarPage
tags$footer("Copyright © 2021 Bioinformatics and Computational Biomedical Engineering (BicBioEng) Lab. All rights reserved.",
            align = "center",
            style = "position: relative; bottom: 0; width: 100%")
) # end fluidPage