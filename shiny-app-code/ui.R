library(shiny)
library(shinybusy)
library(shinyjs)
library(shinyBS)
library(fontawesome)

source("functions.R")

#bgcolor <- "background-color:#D3D3D3 ;"

shinyUI(fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #B7202E;
        font-size: large;
        text-align: left;
      }
    "))
  ),
  
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             
           "))
  ),
  
  titlePanel(
    title = span(img(src = "bmc2-logo.png"), h1("Probability of Events Following PCI")),
    windowTitle = "PCI Prediction App"
  ),
  
  add_loading_state(c(".shiny-plot-output", ".shiny-html-output"), 
                    text = "Please wait while content loads...", 
                    svgColor = "steelblue",
                    timeout = 6000),
  
  hr(),
  
  fluidRow(
    div(id = "form",
        column(2,
               numericInput("age",
                            "Age:",
                            min   = 18,
                            max   = 100,
                            value = 67,
                            step  = 1),
               selectInput("sex",
                           "Sex:",
                           choices = c("Male", "Female"),
                           selected = "Male"),
               numericInput("height",
                            "Height (cm):",
                            min   = 100,
                            max   = 240,
                            value = 170,
                            step  = 1),
               numericInput("weight",
                            "Weight (kg):",
                            min   = 50,
                            max   = 150,
                            value = 89,
                            step  = 1),
               selectInput("vent_support",
                           "Pre-Procedure Ventricular Support:",
                           choices = c("None", 
                                       "Pharmacologic Vasopressor", 
                                       "Mechanical Support",
                                       "Both Mechanical and Vasopressor Support"),
                           selected = "None"),
               selectInput("csha",
                           label = span(tagList("Clinical Frailty Index",
                                                icon("q", "fa-question-circle-o", 
                                                     lib = "font-awesome",
                                                     style="font-size:12px",
                                                     verify_fa = F))),
                           choices = c("1-2: Very Fit or Well",
                                       "3: Managing Well",
                                       "4: Vulnerable",
                                       "5: Mildly Frail",
                                       "6: Moderately Frail",
                                       "7: Severely Frail",
                                       "8-9: Very Sev. Frail or Terminally Ill"),
                           selected = "4: Vulnerable"),
               bsPopover(id = "csha", title = "",
                         content = paste("See the ", a("Canadian Study of Health and Aging", 
                                                       href = "https://www.dal.ca/sites/gmr/our-tools/clinical-frailty-scale.html"),
                                         " for further definitions."
                         ),
                         placement = "top-right", 
                         trigger = "focus", 
                         options = list(container = "body")
               ),
               selectInput("status",
                           "Status:",
                           choices = c("Elective",
                                       "Emergent",
                                       "Urgent",
                                       "Salvage"),
                           selected = "Elective"),
               selectInput("diabetes",
                           "Diabetes:",
                           choices = c("No Diabetes",
                                       "Diabetes: IDDM",
                                       "Diabetes: NIDDM"),
                           selected = "No Diabetes"),
               radioButtons("prior_heart_failure",
                            "Prior Heart Failure?",
                            choices = c("No", "Yes"),
                            selected = "No"),
               conditionalPanel(
                 condition = "input.prior_heart_failure == 'Yes'",
                 selectInput("nyha",
                             "NYHA:",
                             choices = c("Class I",
                                         "Class II",
                                         "Class III",
                                         "Class IV"),
                             selected = "Class I"),
                 selectInput("hf_type",
                             "Heart Failure Type:",
                             choices = c("Diastolic Heart Failure",
                                         "Systolic Heart Failure",
                                         "Heart Failure Type Not Documented"),
                             selected = "Diastolic heart failure")
               ),
               numericInput("systolic",
                            "Systolic Blood Pressure (mmHg):",
                            min   = 80,
                            max   = 180,
                            value = 150,
                            step  = 1), 
               radioButtons("hdl",
                            "Is HDL Cholesterol Known?",
                            choices = c("No", "Yes"),
                            selected = "No"),  
               conditionalPanel(
                 condition = "input.hdl == 'Yes'",
                 numericInput("hdl_val",
                              "HDL Cholesterol (mg/dL):",
                              min   = 30,
                              max   = 100,
                              value = 40,
                              step  = 1),
               ),
               radioButtons("tot_chol",
                            "Is Total Cholesterol Known?",
                            choices = c("No", "Yes"),
                            selected = "No"),  
               conditionalPanel(
                 condition = "input.tot_chol == 'Yes'",
                 numericInput("total_val",
                              "Total Cholesterol (mg/dL):",
                              min   = 100,
                              max   = 250,
                              value = 160,
                              step  = 1),
               ),
               radioButtons("hgb",
                            "Is Hemoglobin Known?",
                            choices = c("No", "Yes"),
                            selected = "No"),  
               conditionalPanel(
                 condition = "input.hgb == 'Yes'",
                 numericInput("hgb_val",
                              "Hemoglobin (g/dL):",
                              min   = 10,
                              max   = 20,
                              value = 14,
                              step  = 1),
               ),
               radioButtons("creat",
                            "Is Creatinine Known?",
                            choices = c("No", "Yes"),
                            selected = "No"),  
               conditionalPanel(
                 condition = "input.creat == 'Yes'",
                 numericInput("creat_val",
                              "Creatinine (g/dL):",
                              min   = 0.20,
                              max   = 1.5,
                              value = 1.0,
                              step  = 0.05)
               ),
               radioButtons("leftheartcath",
                            "Diagnostic Left Heart Cath?",
                            choices = c("No", "Yes"),
                            selected = "No")
        ),
        column(2,
               radioButtons("lvef",
                            "Is Pre-PCI Left Ventricular Ejection Fraction (LVEF) Known?",
                            choices = c("No", "Yes"),
                            selected = "No"),  
               conditionalPanel(
                 condition = "input.lvef == 'Yes'",
                 numericInput("lvef_val",
                              "LVEF (%):",
                              min   = 5,
                              max   = 100,
                              value = 55,
                              step  = 1),
               ),  
               radioButtons("cardiac_arrest_any",
                            "Any Cardiac Arrest at This or Outside Facility?",
                            choices = c("No", "Yes"),
                            selected = "No"),
               radioButtons("cvinstability",
                            label = span(tagList("Cardiovascular Instability?",
                                                 icon("q", "fa-question-circle-o", 
                                                      lib = "font-awesome",
                                                      style="font-size:12px",
                                                      verify_fa = F))),
                            choices = c("No", "Yes"),
                            selected = "No"),
               bsPopover(id = "cvinstability", title = "Any of the following types:",
                         content = paste(
                           "1. Persistent Ischemic Symptoms", br(), 
                           "2. Ventricular Arrythmias", br(),
                           "3. Acute Heart Failure Symptoms", br(), 
                           "4. Hemodynamic Instability", br(),
                           "5. Cardiogenic Shock", br(),
                           "6. Refractory Cardiogenic Shock"
                         ),
                         placement = "top-right", 
                         trigger = "click", 
                         options = list(container = "body")
               ),
               radioButtons("prior_cvd",
                            "Prior CVD?",
                            choices = c("No", "Yes"),
                            selected = "No"),
               selectInput("tobacco",
                           "Tobacco Use",
                           choices = c("Never Used Tobacco",
                                       "Former Use",
                                       "Current Some Day Use",
                                       "Current Every Day Use"),
                           selected = "Diastolic heart failure"),
               selectInput("pci_indication",
                           "PCI Indication",
                           choices = c("Coronary Artery Disease (without ischemic symptoms)",
                                       "New Onset Angina",
                                       "NSTE - ACS",
                                       "Primary PCI for Acute STEMI",
                                       "Stable angina",
                                       "STEMI",
                                       "Other PCI Indication"),
                           selected = "Coronary Artery Disease (without ischemic symptoms)"),
               selectInput("stress_test",
                           "Stress Testing",
                           choices = c("No Stress Testing",
                                       "Negative",
                                       "Positive - Low Risk of Ischemia",
                                       "Positive - Intermediate Risk of Ischemia",
                                       "Positive - High Risk of Ischemia",
                                       "Positive - Risk Unknown",
                                       "Indeterminate/Unavailable"),
                           selected = "No Stress Testing"),
               checkboxGroupInput("cath_indications",
                                  "Indications for Cath Lab Visit (Check All that Apply)",
                                  choices = c(
                                    "ACS <= 24 Hrs Prior to Cath Lab Visit",
                                    "ACS > 24 Hrs Prior to Cath Lab Visit",
                                    "Pre-Operative Evaluation",
                                    "Syncope",
                                    "Cardiomyopathy",
                                    "Stable Known Coronary Artery Disease",
                                    "Cardiac Arrest with Successful Resuscitation",
                                    "Cardiac Arrhythmia",
                                    "Stable Suspected Coronary Artery Desease",
                                    "Heart Valve Disorder",
                                    "Other"
                                  ))
        )),
    column(8, align = "center",
           tabsetPanel(
             type = "tabs",
             tabPanel(
               "Summary",
               br(),
               div(
                 htmlOutput("htmllist"),
                 style="text-align: left;"),
               br(),
               downloadButton("report", "Generate report"),
               linebreaks(2),
               actionButton("resetAll_0", "Reset Values to Defaults")
             ),
             tabPanel(
               "Mortality",
               linebreaks(1),
               h3(textOutput("mort_text")),
               plotOutput("mort_graph"),
               h3(textOutput("mort_rev_text")),
               br(),
               actionButton("resetAll_6", "Reset Values to Defaults")
             ),
             tabPanel(
               "Transfusion",
               linebreaks(1),
               h3(textOutput("trnsf_text")),
               plotOutput("trnsf_graph"),
               h3(textOutput("trnsf_rev_text")),
               br(),
               actionButton("resetAll_1", "Reset Values to Defaults")
             ),
             tabPanel(
               "Stroke",
               linebreaks(1),
               h3(textOutput("stroke_text")),
               plotOutput("stroke_graph"),
               h3(textOutput("stroke_rev_text")),
               br(),
               actionButton("resetAll_2", "Reset Values to Defaults")
             ),
             tabPanel(
               "Acute Kidney Injury",
               linebreaks(1),
               h3(textOutput("aki_text")),
               plotOutput("aki_graph"),
               h3(textOutput("aki_rev_text")),
               br(),
               p("*Predictions are based only on patients not on dialysis at the time of PCI."),
               actionButton("resetAll_3", "Reset Values to Defaults")
             ),
             tabPanel(
               "New Dialysis",
               linebreaks(1),
               h3(textOutput("nrd_text")),
               plotOutput("nrd_graph"),
               h3(textOutput("nrd_rev_text")),
               br(),
               p("*Predictions are based only on patients not on dialysis at the time of PCI."),
               actionButton("resetAll_4", "Reset Values to Defaults")
             ),
             tabPanel(
               "Major Bleeding",
               linebreaks(1),
               h3(textOutput("majbleed_text")),
               plotOutput("majbleed_graph"),
               h3(textOutput("majbleed_rev_text")),
               br(),
               actionButton("resetAll_5", "Reset Values to Defaults")
             )
           )
    )
  )
)
)


