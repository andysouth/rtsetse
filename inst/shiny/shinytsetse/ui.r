#shinytse7/ui.r
#andy south 15/09/2014

#seeking stability

#to run type this in R console
#library(shiny)
#runApp('shinytse7')
#for me to deploy online
#devtools::install_github('AndySouth/rtsetse')
#library(shinyapps)
#deployApp('shinytse7')

if (!require("devtools")) install.packages("devtools")
library(devtools)
if (!require("shinyTable")) install_github("shinyTable", "trestletech")

library(shiny)
library(markdown)
library(shinyTable) #for htable editable tables
library(shinythemes) #for new themes

# Define UI 
#shinyUI(fluidPage(
#shinyUI(fluidPage(theme = shinytheme("cerulean"),  #fine, light blue header, buttons white
#shinyUI(fluidPage(theme = shinytheme("cosmo"), #ugly black header, black buttons
#shinyUI(fluidPage(theme = shinytheme("flatly"), #close to before, dk blue header, buttons medium grey
#shinyUI(fluidPage(theme = shinytheme("journal"), #ugly font
#shinyUI(fluidPage(theme = shinytheme("readable"), #ok, white background, blue text
#shinyUI(fluidPage(theme = shinytheme("spacelab"), #nice, greay header, buttons nr black
shinyUI(fluidPage(theme = shinytheme("united"), #nice orange/red header, grey buttons, good for a temporary change !

  #this didn't work to put text above the navbar
  #helpText("Test..."),
  
  #navbarPage sets up navbar, title appears on left
  navbarPage("rtsetse demonstrator", id="selectedTab",
  #trying a theme, must be compatible with Bootstrap 2.3.2 
  #from http://bootswatch.com/2/ saved in www/ folder
  #this theme is flatly - blue navbar & turquoise links !
  #navbarPage("rtsetse demonstrator", theme = "bootstrap.min.css",
             
    # tab "About" ---------------------------
    tabPanel("About", includeMarkdown("about.md")),
    
             
    # tab "seek stable mortality" ---------------------------
    tabPanel("1 seek stable mortality", value="seek",
      
      helpText("Set population parameters on the left and press the seek button to find",
               " values of adult mortality that generate a stable population."),
             
      sidebarLayout(
        
        #title for this page
        #headerPanel("shinytse7 - seeking stability"),
        #if no headerPanel an error is generated
        #headerPanel(""),
        
        # Sidebar for inputs
        sidebarPanel(
      
          #submitButton("Seek Stable Mortality"),
          #replacing submitButton with actionButton
          actionButton('aButtonMortality',"Seek Stable Mortality"),
          
          sliderInput("fMperF", 
                      "1 males per female:", 
                      min = 0.4,
                      max = 0.8, 
                      step = 0.05,
                      value = 0.5),
      
          sliderInput("iPupDurF", 
                      "2 pupal duration F:", 
                      min = 20,
                      max = 48,
                      step = 1,
                      value = 26),
      
          sliderInput("iPupDurM", 
                       "3 pupal duration M:", 
                       min = 22,
                       max = 50,
                       step = 1,
                       value = 28),    
          
          sliderInput("iFirstLarva", 
                      "4 Age that F produces first larva:", 
                      min = 14,
                      max = 18,
                      step=1,
                      value = 16),
          
          sliderInput("iInterLarva", 
                      "5 Days between larvae:", 
                      min = 7,
                      max = 11,
                      step=1,
                      value = 10),        
          
          sliderInput("pMortPupa", 
                      "6 Mortality Pupal per pupal period:", 
                      min = 0,
                      max = 0.5,
                      step=0.05,
                      value = 0.2),        
          
          sliderInput("pMortLarva", 
                      "7 Mortality Larval per larval period:", 
                      min = 0.01,
                      max = 0.4,
                      step=0.01,
                      value = 0.05),
          
          sliderInput("iMortMinAgeStartF", 
                      "8 Mortality minimum start day F:", 
                      min = 2,
                      max = 12,
                      step = 1,
                      value = 10),
          
          sliderInput("fMortMinPropF", 
                      "9 Mortality min., proportion of day1 F:", 
                      min = 0.05,
                      max = 1,
                      step = 0.05,
                      value = 0.2),          

          sliderInput("fMortOldPropF", 
                      "10 Mortality old, proportion of day1 F:", 
                      min = 0.2,
                      max = 1,
                      step = 0.1,
                      value = 0.3),        
          
          sliderInput("iMortMinAgeStartM", 
                      "11 Mortality minimum start day M:", 
                      min = 2,
                      max = 12,
                      step = 1,
                      value = 10),
          
          sliderInput("fMortMinPropM", 
                      "12 Mortality min., proportion of day1 M:", 
                      min = 0.05,
                      max = 1,
                      step = 0.05,
                      value = 0.2),          
          
          sliderInput("fMortOldPropM", 
                      "13 Mortality old, proportion of day1 M:", 
                      min = 0.2,
                      max = 1,
                      step = 0.1,
                      value = 0.3)
          
        ), #end sidebarPanel
        
        
        mainPanel(
          
          tabsetPanel(
      
            tabPanel("Seeking stability",
              helpText("Graph shows the program testing decreasing F mortalities until it finds",
                       " one that balances the population. It then similarly seeks a male mortality that",
                       " generates the M:F ratio requested by the user."),
              plotOutput("plotStableSeek")),     
            tabPanel("Mortality F", 
              helpText("Graph shows pattern of mortality generated by the inputs and used in later runs."),                     
              plotOutput("plotMortalityF")),
            tabPanel("Mortality M", 
              helpText("Graph shows pattern of mortality generated by the inputs and used in later runs."),                     
              plotOutput("plotMortalityM"))
          ) # end tabsetPanel         
        ) # end mainPanel
      ) # end pageWithSidebar
    ), # end tabPanel("seeking stable mortality")
    
    # tab "a-spatial model" ---------------------------
    tabPanel("2 aspatial model", value="aspatial",
             
      helpText("Runs a model for a single population.",
               " Select parameter values on the left, press run, then view different outputs on the right.",
               " Uses mortality parameters calculated on the previous page.",
               " Copy from the Code tab on the right to repeat this run locally in R."
               ),
             
      sidebarLayout(
         
         #title for this page
         #headerPanel("shinytse7 - seeking stability"),
         #if no headerPanel an error is generated
         #headerPanel(""),
         
         sidebarPanel(
           
           #replaced with actionButton
           #submitButton("Run Model"),
           actionButton('aButtonAspatial',"Run Model"),          
           
           sliderInput("days", 
                       "1 Days:", 
                       min = 1,
                       max = 1000, 
                       value = 100),
           
           sliderInput("iCarryCapF", 
                       "2 Carrying Capacity Females:", 
                       min = 100,
                       max = 10000,
                       step = 100,
                       value = 200),   
           
           sliderInput("fStartPopPropCC", 
                       "3 Start popn as a proportion of CC:", 
                       min = 0.1,
                       max = 2,
                       step= 0.1,
                       value = 1),   
           
           helpText("4 Density Dependent proportion of:"),
           
           wellPanel(
           sliderInput("propMortAdultDD", 
                       "a adult mortality:", 
                       min = 0,
                       max = 1,
                       step=0.05,
                       value = 0.25),
               
           sliderInput("propMortLarvaDD", 
                       "b larval mortality:", 
                       min = 0,
                       max = 1,
                       step=0.05,
                       value = 0.25),
           
           sliderInput("propMortPupaDD", 
                       "c pupal mortality:", 
                       min = 0,
                       max = 1,
                       step=0.05,
                       value = 0.25)            
           )#end wellPanel
           
           
         ), #end sidebarPanel   
         mainPanel( 
           tabsetPanel(
             # viewing outputs -----------------
             tabPanel("Popn", plotOutput("plotPop")),
             tabPanel("Females by age", plotOutput("plotAgeStructF")),
             tabPanel("Males by age", plotOutput("plotAgeStructM")),
             tabPanel("Mean age adults", plotOutput("plotMeanAge")),
             tabPanel("Code", verbatimTextOutput("printParamsAspatial"))
           ) # end tabsetPanel         
         ) # end mainPanel
      ) # end pageWithSidebar             
    ), # end tabPanel("a-spatial model") 
    
    # menu tab "spatial model" 
    # adding a menu item, that will create dropdown
    #navbarMenu("3 spatial model",
     
     # tab load grid ---------------------------           
     #tabPanel("3.1 load grid",
     tabPanel("3 load map", value="map",
              
      helpText("Allows loading of a text file containing a grid of vegetation categories",
               " be used in the simulation. You can choose from internally saved files",
               " or space-delimited text files with the codes DTOSBGN and an optional csv file specifying vegetation names.",
               a(href="helpfiles/helpLoadVegetation.html", "Click here for help",target="_blank")
               ),
      
      #now added onto the helpText instead
      #a(href="helpfile.html", "Click here for help"), 
      
      sidebarLayout(        
        #if no headerPanel an error is generated
        #headerPanel(""),
        
        #setting width to make space for table
        #sidebarPanel( width=10,
        sidebarPanel(
        
          selectInput(inputId = 'mapLocation',
                      label = "Where to get map from ?",
                      choices = c("Internal","Local") ),
          
          # Only show this for Local
          conditionalPanel(
            condition = "input.mapLocation == 'Local'",
            #this allows searching for local files
            fileInput('fileMapLocal', 'Choose a grid text file and optional attribute csv', multiple=TRUE) #FALSE)
            ), #end conditionalPanel
          
          # Only show this for internal maps
          conditionalPanel(
            condition = "input.mapLocation == 'Internal'",          
            #this allows searching for files stored in the app
            #starting with veg and ending with txt
            selectInput(inputId = 'fileMapInternal',
                        label = "Choose an internal map",
                        #choices = grep(".txt", list.files('.','^veg'),value=TRUE) )
                        #can I instead get at files from rtsetse ?
                        #should probably do this in server file
                        choices = grep(".txt", list.files(system.file("extdata", package="rtsetse"),'^veg'),value=TRUE) )
          
            ), #end conditionalPanel          
          
          
          helpText("Vegetation names are defaults or come from",
                   "an optional csv file.",
                   "Edit below and press update to change the map legend."),
          
          #EDItable of vegetation attributes         
          #!BEWARE that the num & name of columns gets set by the first 
          #file loaded, if later files have more columns the data are not shown
          htable("editableRasterAtts", colHeaders="provided"), 
          
          
          actionButton('aButtonMap',"update map")  
          
          #just for testing now
          #static table of vegetation attributes         
          #tableOutput("tableRasterAtts")
          

          

        ), # end sidebarPanel
      
      mainPanel(       
        
        tabsetPanel(
          
          tabPanel("loaded map", plotOutput("plotLoadedMap")),
          #tabPanel("plot of a gridascii", plotOutput("plotAsc")),
          #tabPanel("editable", htable("tbl", colHeaders="provided")),
          tabPanel("loaded file table", tableOutput("tableNonEdit"))  
          #tabPanel("About", includeMarkdown("about.md"))
          
        ) # end tabsetPanel         
       ) # end mainPanel              
      ) # end pageWithSidebar        
     ), # end tabPanel("load grid")               
     
     # tab run grid model ---------------------------                      
     tabPanel("4 run grid model", value="grid",
             
     helpText("Runs a gridded model with a starting population defined by the carrying capacities in the loaded grid.",
              " If you tick 'Test spread' popn is started from single central cell on a uniform grid.",
              " Uses parameters from previous pages to run the aspatial model in each cell and move flies between cells.",
              " Select parameter values on the left, press run, then view different outputs on the right.",
              " Use the button on the left to download a run report, or the Code tab on the right to copy",
              " the code to repeat this run locally in R."
              ),
     
     sidebarLayout(
       
       #if no headerPanel an error is generated
       #headerPanel(""),
     
       sidebarPanel(
         
         #splitLayout did kind of work to create 2 columns of inputs, maybe come back to
         #splitLayout(
         
         #submitButton("Run Model"), #replaced with actionButton
         actionButton('aButtonGrid',"Run Model"),   
         
         #next 2 for report download
         downloadButton('downloadReport',label='download run report'),
         #),
         #radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
         
         #checkbox for whether to test spread on uniform grid
         checkboxInput('testSpread', "Test spread on uniform grid", value = FALSE),
         
         #conditionalPanel so that rows & cols only visible for uniform grid
         #previously didn't respond due to submitButton
         #conditionalPanel(
         #   condition = "input.testSpread == true",
           
#            sliderInput("nRow", 
#                        "1 rows:", 
#                        min = 1,
#                        max = 100,
#                        step= 1,
#                        value = 9),
#            
#            sliderInput("nCol", 
#                        "2 columns:", 
#                        min = 1,
#                        max = 100,
#                        step= 1,
#                        value = 9)           

         # ), #end conditionalPanel
         
         
         sliderInput("daysGridModel", 
                     "1 Days:", 
                     min = 1,
                     max = 100, 
                     value = 4),

         sliderInput("pMoveF", 
                      "2 proportion F moving:", 
                      min = 0,
                      max = 1,
                      step = 0.05,
                      value = 0.6),
         
         sliderInput("pMoveM", 
                     "3 proportion M moving:", 
                     min = 0,
                     max = 1,
                     step = 0.05,
                     value = 0.3)
         
       ), #end sidebarPanel
       
       # mainPanel
       mainPanel(
         
         tabsetPanel(
 
           #can use this for temporary disabling grid model   
           #tabPanel("temporary", helpText("temporarily disabled while we work out how best to do this")) 
        
           # viewing outputs -----------------          
           tabPanel("Maps daily", plotOutput("plotMapDays")),
           tabPanel("Maps F", plotOutput("plotMapDaysF")),
           tabPanel("Maps M", plotOutput("plotMapDaysM")),
           tabPanel("Map final", plotOutput("plotMapFinalDay")),           
           tabPanel("Popn whole grid", plotOutput("plotPopGrid")),
           tabPanel("Age structure", plotOutput("plotAgeStructGrid")),
           #tabPanel("Females by age", plotOutput("plotAgeStructF")),
           #tabPanel("Males by age", plotOutput("plotAgeStructM")),
           tabPanel("Mean age adults", plotOutput("plotMeanAgeGrid")),
           tabPanel("test inputs", textOutput("testInputVals")),
           tabPanel("Code", verbatimTextOutput("printParamsGrid")) 
           #tabPanel("test inputs", textOutput("testInputs")),
           #tabPanel("About", includeMarkdown("about.md"))
           
           ) # end tabsetPanel                 
         ) # end mainPanel         
       ) # end sidebarLayout  
    ), # end tabPanel("spatial model") 
    #), # end navbarMenu("3 spatial model",

    # tab "control" ---------------------------
    tabPanel("5 control", value="control",
             
#              helpText("Not yet implemented.",
#                       " Will allow different control measures to be simulated ITC, AB and SAT."
#                       )
             helpText("Applies control by artificial baits to the gridded model specified by the previous tabs.",
                      " (Under development)."
             ),

      sidebarLayout(      
        sidebarPanel(
#            
          actionButton('aButtonControl',"Run Model"), 
#           
          #sliderInput("pControl", 
          numericInput("pControl", 
                      "1 proportion killed by control:", 
                      min = 0,
                      max = 0.5,
                      step = 0.05,
                      value = 0.1),
          
          numericInput("iControlBorder", 
                      "2 border width not controlled:", 
                      min = 1,
                      max = 20, 
                      value = 1)
          
      ), #end sidebarPanel

      # mainPanel
      mainPanel(
        
        tabsetPanel(
                    
#           # viewing outputs -----------------          
            tabPanel("Maps daily", plotOutput("plotMapDaysControl"))         
            #to include a code tab I'll probably need to copy & rename "printParamsGrid" in server
            #tabPanel("Code", verbatimTextOutput("printParamsGrid")) 
            #tabPanel("About", includeMarkdown("about.md"))
    
          ) # end tabsetPanel                 
        ) # end mainPanel         
      ) # end sidebarLayout  
    ) # end tabPanel("control") 
  ) # end navbarPage  
) # end fluidPage
) # end shinyUI
            










