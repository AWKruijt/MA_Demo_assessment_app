### first a bunch of preparatory stuff: 

#scopus search string: (TITLE-ABS-KEY (( "dot probe" OR "probe detection task"
#OR "visual probe" OR "attentional probe task" OR "probe classification task" OR
#"atten* bias modification" ) OR ( "atten* retrain*" AND "probe" )) AND
#TITLE-ABS-KEY ( "bias*" OR "atten*" )) AND LANGUAGE ("English" )

# Scopus offers download of bibliographic information that is sorted in 5
# categories. Export info for the categories 'Sitation information' and 'Abstrac
# & keywords' for all records as a csv file.

# read scopus' csv  # 1181 records in total
scopusMarch18 <- read.csv("S2b scopus March18.csv",check.names=FALSE) 

# some of the scopus variablenames consist of two words with a space inbetween
# (Page count, Author Keywords, etc) this doesn't use to be the case (used to be
# dots) and R also doesn't handle them well, so we'll replace all spaces with
# dots.

names(scopusMarch18) 
names(scopusMarch18)  <- sub(" ", ".", names(scopusMarch18))

# second stage filtering consists of selecting those records for which at least
# one the terms "RCT", "randomized controlled", "randomised controlled", or
# "intervention" plus at least one of the terms anx*", " SAD ", " GAD ", " OCD
# ", " phobi*", " PTSD ", " panic", and at least one of the terms "patient*",
# "diagnos*", "clinic*" were found across each record’s title, abstract, and
# (index and author supplied) keywords

varsToSearch <- paste( scopusMarch18$Title, scopusMarch18$Abstract, scopusMarch18$Index.Keywords, scopusMarch18$Author.Keywords)

recordstoAssess <- scopusMarch18[(
    grepl( paste( c("RCT", "randomized controlled", "randomised controlled", 
                    "intervention", "program"), collapse = "|"), 
      varsToSearch, ignore.case = T ) & 
    grepl( paste( c(" anx*", " SAD ", " GAD ", " OCD ", " phobi*", " PTSD ", 
                  " panic"), collapse = "|"), 
      varsToSearch, ignore.case = T ) & 
    grepl( paste(c( "patient*", "diagnos*", "clinic*", "disorder"), collapse = "|"), 
      varsToSearch, ignore.case = T ) ), ]

# after filtering there are 239 records selected.
nrow(recordstoAssess)
table(recordstoAssess$Year)


library(shiny)

require("plyr")
require("DT")
require("shinyjs")

assessorID <- "demo"

#create a var recordID to use for IDing records (circular explanation much? :p)
recordstoAssess$recordID <- rownames(recordstoAssess)

showHidden = F

# Define the fields we want to save from the form
fields_I <- c("crt_CBM_A", "crt_ThreatBias_A", "crt_Adults_A", "crt_ClinicalAnx_A", "selectStageII", "studyDesign_A")

fields_II <- c("crt_CBM", "crt_ThreatBias", "crt_Adults", "crt_ClinicalAnx", "studyDesign", "primary_diag", "diag_instrument", "moodComorbidity", "BI_task", "stimLatency", "stimType", "n_exp1", "n_exp2", "n_ctrl", "m_BI_pre_exp1", "SD_BI_pre_exp1", "m_BI_pre_exp2", "SD_BI_pre_exp2", "m_BI_pre_ctrl", "SD_BI_pre_ctrl", "AvailableStatifNoGroupVals", "n_total", "m_BI_pre_sample", "SDpooled_BI_pre_sample", "asssessor_Comments", "inclusion_recommended", "consider_Data_Request", "sharedCntrlGrp")
      

# create an assessmentDF to store assessments and scopwip data in (if it doesn't
# yet exist)

 if ( ! exists("assessmentDF")) { 
   assessmentDF <- data.frame(matrix(NA, ncol= ( length(recordstoAssess) + length(fields_I) + length(fields_II) ), nrow = nrow(recordstoAssess)))  # dataframe from a matrix of NAs with the length of the sum of lenghts of recordstoAssess and the to be added input fields
   
   colnames(assessmentDF) = c(colnames(recordstoAssess), fields_I, fields_II) # take colnames from recordstoAssess and input fields
   
   assessmentDF[,1:length(recordstoAssess)]<- recordstoAssess # enter recordstoAssess data into the first 'length(recordstoAssess)' collumns
} 

options(stringsAsFactors = F)

# Save responses
saveData_I <- function(data_I, ID) {
  data_I <- as.data.frame(t(data_I))
   assessment_I <<- data_I
   assessmentDF[ assessmentDF$recordID== ID, (length(recordstoAssess)+1): (length(recordstoAssess) + length(fields_I) )   ] <<- assessment_I 
   write.csv(assessmentDF, paste0("assessmentDF_", assessorID, ".csv" ), row.names = F)
    }

saveData_II <- function(data_II, ID) {
  data_II <- as.data.frame(t(data_II))
    assessment_II <<-  data_II
        assessmentDF[assessmentDF$recordID== ID, ( (length(recordstoAssess) + length(fields_I) +1): length(assessmentDF) ) ] <<- assessment_II
   write.csv(assessmentDF, paste0("assessmentDF_", assessorID, ".csv" ), row.names = F)
    }


addData_row <- function(ID) {
  rowToAdd <<- as.data.frame(assessmentDF[assessmentDF$recordID== ID,])
  rowToAdd$recordID <<- paste0(ID, "_study2")
    assessmentDF <<-  rbind(assessmentDF, rowToAdd)
    assessmentDF[assessmentDF$recordID== ID,]$recordID <<- paste0(ID, "_study1")
    }

#########

# Shiny app with response fields that the user can submit data for
shinyApp(
  
  ui=
  navbarPage("MA input Tool",

  tabPanel("Stage I",
  
  fluidPage(
    fluidRow(
      column(4, h3("Select Record")),
      column(4, h3("Record Info")),
      column(4, h3("Input Assessment")) ),
  
    fluidRow(      
      tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
# this tag organises text wrap on the text in/outputs - so that the textboxes
# also expand to accodomodate the full text in browsers
      column(4, DT::dataTableOutput('recordATY')),
      column(4, 
        fluidRow(column(12, h5("Selected Title"), verbatimTextOutput('selTitle_A'))),
        fluidRow(column(12, h5("Abstract"), verbatimTextOutput('selAbstract_A'))),
        fluidRow(column(12, h5("Index Keywords"), verbatimTextOutput('selIndexKW_A'))),
        fluidRow(column(12, h5("Author Keywords"), verbatimTextOutput('selAuthorKW_A'))) ),
      column(4,
        fluidRow(column(12, h5("study aims to evaluate effects of a bias modification procedure (ABM / CBM / other)?"), selectInput('crt_CBM_A','Options', c(Choose='', c("yes", "no", "possibly", "paper is review/MA")),  selectize=TRUE))),
        fluidRow(column(12, h5("assesses attention allocation bias to threatening information?"), selectInput('crt_ThreatBias_A', 'Options', c(Choose='', c("yes", "no", "possibly")), selectize=TRUE))),
        fluidRow( column(12, h5("participants are adults?"), selectInput('crt_Adults_A', 'Options', c(Choose='', c("yes", "no", "possibly")), selectize=TRUE))),
        fluidRow( column(12, h5("Clinical/diagnosed Anxiety?"), selectInput('crt_ClinicalAnx_A', 'Options', c(Choose='', c("yes", "no", "possibly")), selectize=TRUE))),
        fluidRow(column(12, h5("Design?"), selectInput('studyDesign_A','', c(Choose='', c("2 groups: e.g. avoid negative/attend positive; placebo", "3 groups: e.g. avoid negative; attend positive; placebo", "1 group, avoid negative")), selectize=TRUE))),

        fluidRow(column(12, h5("select for stage II?"), selectInput("selectStageII",'', c(choose = "", c("yes", "no", "?"))))),

        fluidRow( actionButton("submit", "submit", class = "btn-primary")) ))) ),
  

  tabPanel("Stage II",
  shinyjs::useShinyjs(),

  fluidRow( column(3, h3("Select Record")),
            column(3, h3("Inclusion Assessment")),
            column(3, h3("Input Values")),
            column(3, h3("Calculated Values"))),
  
         
  fluidRow( column(3,
              fluidRow(column(12, textOutput('selTitle'))),
                br(),
              fluidRow(column(12, htmlOutput("selDOIURL") ) ),
                br(),       
              DT::dataTableOutput('stIIrecordATY')),
            column(3,
            fluidRow( column(12, h5("study aims to evaluate effects of a bias modification procedure (ABM / CBM / other)?"), selectInput('crt_CBM','Options', c(Choose='', c("yes", "no", "possibly", "paper is review/MA")),  selectize=TRUE))),
            fluidRow( column(12, h5("assesses attention allocation bias to threatening (relative to neutral) information?"), selectInput('crt_ThreatBias', '', c(Choose='', c("yes", "no", "possibly")), selectize=TRUE))),
            fluidRow( column(12, h5("participants are adults?"), selectInput('crt_Adults', '', c(Choose='', c("yes", "no", "possibly")), selectize=TRUE))),
            fluidRow( column(12, h5("Clinical/diagnosed Anxiety?"), selectInput('crt_ClinicalAnx', '', c(Choose='', c("yes", "no", "possibly")),selectize=TRUE))),
            fluidRow( column(12, h5("Primary diagnosis"), selectInput('primary_diag', '', c(Choose='', c("GAD", "OCD", "Panic Disorder", "PTSD", "SP / SAD", "simple phobia")), selectize=TRUE))),
            fluidRow( column(12, h5("Diagnostic instrument"), selectInput('diag_instrument', '', c(Choose='', c("MINI", "SCID", "LSAS", "CIDI", "ADIS", "CAPS", "clinician")), selectize=TRUE))),
            fluidRow( column(12, h5("Mood comorbidity?"), selectInput('moodComorbidity', '', c(Choose='', c("exclusion for comorbid mood disorder", "no exclusion for comorbid mood disorder", "unknown")), selectize=TRUE))),
            fluidRow( column(12, h5("Design?"), selectInput('studyDesign','', c(Choose='', c("2 groups: e.g. avoid negative/attend positive; placebo", "3 groups: e.g. avoid negative; attend positive; placebo", "1 group: avoid negative")), selectize=TRUE))),
            fluidRow( column(12, h5("BI assessment task?"), selectInput('BI_task','', c(Choose='', c("Dot Probe", "Posner/Single Cueing")),  selectize=TRUE))),
            fluidRow( column(12, h5("stimulus latency?"), selectInput('stimLatency','', c(Choose='', c("< 500", "500 - <1000", "??? 1000 ", "other/mixed")),  selectize=TRUE))),
            fluidRow( column(12, h5("Stimulus types?"), selectInput('stimType','', c(Choose='', c("words", "faces", "scenes", "other/mixed")),  selectize=TRUE)))),

        column( 3, 
              shinyjs::useShinyjs(),
              id = "inputCol1",
                     
                fluidRow( column(12, numericInput("n_exp1", "n exp (e.g. avoid neg):", "n exp avoid neg", value = "NA"))),
              conditionalPanel(
               condition = ("input.studyDesign == '3 groups: e.g. avoid negative; attend positive; placebo'"),
            
                fluidRow( column(12, numericInput("n_exp2", "n exp (e.g. attend pos):","n EXP attend pos", value = "NA")))),
                fluidRow( column(12, numericInput("n_ctrl", "n control:", "n control", value = "NA"))),
                h4("Bias at baseline"),
                h5("BI for threat-neutral trials only!" ),
                 br(),
  
                fluidRow( column(12, numericInput("m_BI_pre_exp1", "mean BL BI exp (e.g. avoid neg):", value = "NA"))),
                fluidRow( column(12, numericInput("SD_BI_pre_exp1", "SD BL BI exp (e.g. avoid neg):", value = "NA"))),
            
               conditionalPanel(
               condition = ("input.studyDesign == '3 groups: e.g. avoid negative; attend positive; placebo'"),
               
                fluidRow( column(12, numericInput("m_BI_pre_exp2", "mean BL BI exp (e.g. attend pos):", value = "NA"))),
                fluidRow( column(12,numericInput("SD_BI_pre_exp2", "SD BL BI exp (e.g. attend pos)", value = "NA")))),
                fluidRow( column(12, numericInput("m_BI_pre_ctrl", "mean BL BI control:", value = "NA"))),
                fluidRow( column(12, numericInput("SD_BI_pre_ctrl", "SD BL BI control:", value = "NA"))),
              
                h4("ES from statistics"),
                h5("only if above BI values are not reported but some kind of statistic for threat BI relative to '0' (!) is. I haven't actually implemented this completely - first wanted to see if such cases pop up and if so which type (F/t/other)" ),
                 br(),
                fluidRow( column(12,h5("statistic"), selectInput('AvailableStatifNoGroupVals', ' ', c(Choose='', c("F ANOVA", "F ANCOVA", "t-test", "Chi squared", "log odds")))))),
            
            
            column(3, 
              shinyjs::useShinyjs(),
              id = "inputCol2",  
              h5(textOutput("n_total")),  br(),
              h5(textOutput("m_BI_pre_sample")),  br(),
              h5(textOutput("SDpooled_BI_pre_sample")),  br() ,
              checkboxInput("inclusion_recommended", "Assesor recommmends inclusion in MA", value = FALSE), br(),
              textInput("asssessor_Comments", "enter any additional comments here:", " "), br(),
              actionButton("submit_II", "submit", class = "btn-primary"), br(), br(),
              checkboxInput("consider_Data_Request", "Has unreported BI been assessed at BL that could be requested?", value = FALSE), br(),
              h5("Indicate ohter designs here:"),  
              checkboxInput("sharedCntrlGrp", "one control group with two or more separate disorder patient groups", value = FALSE), br(),
              checkboxInput("addSecondStudyforSamePaper", "check this box if the paper reports on two studies - score the first, a new row for the second study will be added", value = FALSE), br(),


        conditionalPanel("output.showHidden",
               numericInput("n_total", "n_total", value = "NA"),
               numericInput("m_BI_pre_sample", "m_BI_pre_sample", value = "NA"),               
               numericInput("SDpooled_BI_pre_sample", "SDpooled_BI_pre_sample", value = "NA") ))
    ))),
             

server = function(input, output, session) {
    
  
  ## panel I stuff ##
  
      
  output$recordATY = DT::renderDataTable(assessmentDF[,1:3], server = F, selection = 'single', rownames=F)

  # when a record is selected in the table on panel 1 - update fields with pre-existing values in 'assessmentDF':      
 observeEvent (input$recordATY_rows_selected, {
   
   output$selAbstract_A = renderText({ as.character(assessmentDF[input$recordATY_rows_selected,]$Abstract ) })
   output$selTitle_A = renderText({ as.character(assessmentDF[input$recordATY_rows_selected,]$Title ) })
   output$selAuthors_A = renderText({ as.character(assessmentDF[input$recordATY_rows_selected,]$Authors ) })
   output$selIndexKW_A = renderText({ as.character(assessmentDF[input$recordATY_rows_selected,]$Index.Keywords ) })
   output$selAuthorKW_A = renderText({ as.character( assessmentDF[input$recordATY_rows_selected,]$Author.Keywords )}) 

  # for all values (= variable names) in 'fields_I: retrieve previously saved values if these exist'.    
     lapply (fields_I, function(x) updateSelectInput(session, x, selected = (assessmentDF[input$recordATY_rows_selected,x])) )
    })
 
  # track if answers on panel 1 indicate that a record is eligible for stage II assessment:
   # IF eligible for stage II, indicate that in 'selectstageII' and also transfer stage I main criterion values to the correspodning stage II criterion variables.
   observe({  
    if ( (input$crt_CBM_A=='yes' | input$crt_CBM_A=='possibly') & 
         (input$crt_ThreatBias_A=='yes'| input$crt_ThreatBias_A=='possibly') &
         (input$crt_Adults_A=='yes'| input$crt_Adults_A=='possibly') &        
         (input$crt_ClinicalAnx_A=='yes'| input$crt_ClinicalAnx_A=='possibly') ) {
              updateSelectInput(session, "selectStageII", selected = "yes") }
     
   # if any of the four Abstract crits gets a value NO, or is open, change selectStageII to NO 
      else if ( (input$crt_CBM_A =='no') |  
              (input$crt_ThreatBias_A=='no') |
              (input$crt_Adults_A=='no') |            
              (input$crt_ClinicalAnx_A=='no')  ) {
                    updateSelectInput(session, "selectStageII", selected = "no" ) } 
     else { updateSelectInput(session, "selectStageII", selected = "?" ) } }) 


  ## panel II stuff ##

              
  output$stIIrecordATY <- DT::renderDataTable( subset(assessmentDF, assessmentDF$selectStageII == "yes")[, c(20,1,3)], server = F, selection = 'single', rownames= F) 
   
# when a record is selected in the table on panel 2 - update fields with pre-existing values in 'assessmentDF':  
  
    observeEvent (input$stIIrecordATY_rows_selected, { 
      
     output$selTitle = renderText({ as.character( subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$Title )}) 
     output$selDOI = renderText({ as.character( subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$DOI )})        
  #  output$selDOIURL <- renderUI({ HTML(paste( a("clickety", target="_blank", href= paste("http://dx.doi.org/", subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$DOI) ) ) ) })
     
     output$selDOIURL <- renderUI({ HTML(paste( a(paste("link:", subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$DOI), target="_blank", href= paste("http://dx.doi.org/", subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$DOI) ) ) ) })           
     
     
  # for 'selectinput' values, for all values (= variable names) in 'fields_II' retrieve previously saved values. For variables that are not selectinput but numericinput this new input gets 'ignored'.     
     lapply (fields_II, function(x) updateSelectInput(session, x, selected = (subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,x])) )
 
# for 'numericInput' values, for all values (= variable names) in 'fields_II' retrieve previously saved values. For variables that are not numericInput but selectInput this new input gets 'ignored'.      
      lapply (fields_II, function(x) updateNumericInput(session, x, value=  subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,x])) 

# for 'checkbox' values - which are the last 3 variables in 'fields_II' retrieve previously saved values. This one for some reason only works if the retrieved value is first stored in another variable (checkVal)
      
   lapply (fields_II[26:28], function(x) {
      checkVal <- as.logical(subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,x])
      updateCheckboxInput(session, x, value= checkVal) } )


# finally, for the first 6 fields (which are the 5 'abstract criterion fields' + selected for phase II which will be ignored) check if the value retrieved in the first lapply function above is NA, if that is the case use the value obtained for that criterion in the 'abstract phase' (phase I), i.e. for criterion 'crt_CBM' use the value for 'crt_CBM_A'. 
        lapply (fields_II[1:6], function(x) if (is.na (subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,x])) {updateSelectInput(session, x, selected = (subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected, paste0(x, "_A")]))} ) 
        }) 


  # reset the input colums when the study design option changes
   # observeEvent (input$studyDesign, {
  #    reset("inputCol1") 
  #    reset("inputCol2") 
  #   })
  
  # compute totals:
    
 #showhidden: if FALSE the inputs that get the newly computed outputs are hidden in the interface   
   showHidden <- FALSE
   
    observeEvent (input$n_exp1 | input$n_exp2 | input$n_ctrl, {     
      updateNumericInput(session, "n_total", value =  (sum(input$n_exp1, input$n_exp2, input$n_ctrl, na.rm = T) ) ) }) 

    output$n_total <- renderText({ paste("n sample total: ", input$n_total)}) 
     
observeEvent (input$n_exp1 | input$n_exp2 | input$n_ctrl | input$m_BI_pre_exp1 | input$m_BI_pre_exp2 | input$m_BI_pre_ctrl, {     
    if (input$studyDesign == '3 groups: e.g. avoid negative; attend positive; placebo') {
      updateNumericInput (session, "m_BI_pre_sample", value = ( ( (input$m_BI_pre_exp1 * input$n_exp1) + (input$m_BI_pre_exp2 * input$n_exp2) + (input$m_BI_pre_ctrl *input$n_ctrl) ) /  (sum(input$n_exp1, input$n_exp2, input$n_ctrl) ) ) )} 
     if (input$studyDesign != '3 groups: e.g. avoid negative; attend positive; placebo') {  
       updateNumericInput (session, "m_BI_pre_sample", value = ( ( (input$m_BI_pre_exp1 * input$n_exp1) + (input$m_BI_pre_ctrl *input$n_ctrl) ) /  (sum(input$n_exp1, input$n_ctrl)) )) }
     if (input$studyDesign == '1 group: avoid negative') {  
       updateNumericInput (session, "m_BI_pre_sample", value = input$m_BI_pre_exp1 )}
  
output$m_BI_pre_sample <- renderText({ paste("Mean BI pre : ", input$m_BI_pre_sample)}) })

observeEvent (input$n_exp1 | input$n_exp2 | input$n_ctrl | input$SD_BI_pre_exp1 | input$SD_BI_pre_exp2 | input$SD_BI_pre_ctrl, {  
    if (input$studyDesign == '3 groups: e.g. avoid negative; attend positive; placebo') {
      updateNumericInput (session, "SDpooled_BI_pre_sample", value =  ( sqrt( ( (((input$n_exp1 -1) * input$SD_BI_pre_exp1^2) + ((input$n_exp2 -1) * input$SD_BI_pre_exp2^2) + ((input$n_ctrl -1) * input$SD_BI_pre_ctrl^2) ) / ( input$n_exp1 + input$n_exp2 +  input$n_ctrl -3) )  ) )  ) }
    if (input$studyDesign != '3 groups: e.g. avoid negative; attend positive; placebo') { 
      updateNumericInput (session, "SDpooled_BI_pre_sample", value =  (sqrt( ( (((input$n_exp1-1) * input$SD_BI_pre_exp1^2) + ((input$n_ctrl-1) * input$SD_BI_pre_ctrl^2) ) /( input$n_exp1 + input$n_ctrl -2) )  ) ) )} 
     if (input$studyDesign == '1 group: avoid negative') {  
       updateNumericInput (session, "SDpooled_BI_pre_sample", value = input$SD_BI_pre_exp1 )} })
  

output$SDpooled_BI_pre_sample <- renderText({ paste("SD(pooled) BI pre : ", input$SDpooled_BI_pre_sample)})  

       
 ## Overall stuff ##    
    
     
 # Whenever a field is filled, aggregate all form data
    formData_I <- reactive({
      data_I <- sapply( fields_I, function(x) input[[x]])
      data_I
   })
 
# Whenever a field is filled, aggregate all form data
    formData_II <- reactive({
      data_II <- sapply(fields_II, function(x) input[[x]])
      data_II
   })
  
     
  # When the submit button on panel 1 is clicked, save the form data, then re-render the table for stage/page II.
  # open a dialogue warning if no row is selected.
    observeEvent(input$submit, { 
     if (!is.null(input$recordATY_rows_selected))  {
      saveData_I(formData_I(), ID = assessmentDF[input$recordATY_rows_selected,]$recordID)
       
      output$stIIrecordATY <- DT::renderDataTable( subset(assessmentDF, assessmentDF$selectStageII == "yes")[, c(20,1,3)], server = F, selection = 'single', rownames= F) }
      
   if (is.null(input$recordATY_rows_selected)) {
      showModal(modalDialog(
        title = "Message",
        "No rows selected in table!",
        easyClose = TRUE)) } 
      })  
  
  # When the submit button on panel 2 is clicked, save the form data to the record selected in the table, then re-render the table.
  # open a dialogue warning if no row is selected.
    observeEvent(input$submit_II, { 
      if (!is.null(input$stIIrecordATY_rows_selected))  {
      saveData_II(formData_II(), ID = subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$recordID )

        } 
      
      else if (is.null(input$stIIrecordATY_rows_selected)) {
      showModal(modalDialog(title = "Message", "No rows selected in table!", easyClose = TRUE)) }
      
      #if the input indicates that a paper reports on multiple studies and a new row should be added: adjust the assessment dataframe....
      if (input$addSecondStudyforSamePaper == TRUE) {
        addData_row(ID = subset(assessmentDF, assessmentDF$selectStageII == "yes")[input$stIIrecordATY_rows_selected,]$recordID )  }
      
      })
})
 

