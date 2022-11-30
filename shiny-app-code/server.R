library(shiny)
library(tidyverse)
library(xgboost)
library(shinyBS)

load("models/models.rda")  # rda file with models not available in this repo.
source("functions.R")


shinyServer(function(input, output, session) {
  
  observeEvent(input$resetAll_0, {
    reset("form")
  })
  observeEvent(input$resetAll_1, {
    reset("form")
  })
  observeEvent(input$resetAll_2, {
    reset("form")
  })
  observeEvent(input$resetAll_3, {
    reset("form")
  })
  observeEvent(input$resetAll_4, {
    reset("form")
  })
  observeEvent(input$resetAll_5, {
    reset("form")
  })
  observeEvent(input$resetAll_6, {
    reset("form")
  })
  
  indications <- reactive(if_else(is.null(input$cath_indications), 
                                  "empty", 
                                  paste(input$cath_indications, sep = "", collapse = ""))
  )
  
  data_tbl <- reactive(
    
    data.frame(
      age = as.numeric(input$age),
      height = as.numeric(input$height),
      weight = as.numeric(input$weight),
      preproccreat = if_else(
        input$creat == "No",
        1,
        as.numeric(input$creat_val)
      ),
      lipidshdl = if_else(
        input$hdl == "No",
        41,
        as.numeric(input$hdl_val)
      ),
      lipidshdl_NA = if_else(
        input$hdl == "No",
        1L,
        0L
      ),
      lipidstc = if_else(
        input$tot_chol == "No",
        160,
        as.numeric(input$total_val)
      ),
      lipidstc_NA = if_else(
        input$tot_chol == "No",
        1L,
        0L
      ),
      procsystolicbp = as.numeric(input$systolic),
      hgb_NA = if_else(
        input$hgb == "No",
        1L,
        0L
      ),
      hgb = if_else(
        input$hgb == "No",
        14,
        as.numeric(input$hgb_val)
      ),
      cvinstability = case_when(
        input$cvinstability == "Yes" ~ 1L,
        input$cvinstability == "No"  ~ 0L
      ),
      sex = as.factor(input$sex),
      ventricular_support = case_when(
        input$vent_support == "None" ~ 0L,
        input$vent_support == "Pharmacologic Vasopressor" ~ 2L,
        input$vent_support == "Mechanical Support" ~ 1L,
        input$vent_support == "Both Mechanical and Vasopressor Support" ~ 3L) %>% 
        factor(levels = 0:3, labels = c("No Ventricular Support or Unknown",
                                        "Mechanical Support",
                                        "Vasopressor Support",
                                        "Both Mechanical and Vasopressor Support")
        ),
      cshascale = as.factor(input$csha),
      status = case_when(
        input$status == "Elective" ~ 0L,
        input$status == "Emergent" ~ 1L,
        input$status == "Urgent"   ~ 2L,
        input$status == "Salvage"  ~ 3L,
        TRUE                       ~ NA_integer_) %>%  
        factor(levels = 0:3, labels = c("Elective",
                                        "Emergent",
                                        "Urgent",
                                        "Salvage")),
      diab_tx = case_when(
        input$diabetes == "No Diabetes" ~ 0L,
        input$diabetes == "Diabetes: IDDM" ~ 1L,
        input$diabetes == "Diabetes: NIDDM" ~ 2L,
        TRUE ~ NA_integer_) %>% 
        factor(levels = 0:2, labels = c("No Diabetes", "Diabetes - IDDM", "Diabetes - NIDDM")),
      priornyha = if_else(input$prior_heart_failure == "No",
                          "No Hx Heart Failure",
                          input$nyha)  %>% factor(levels = c("No Hx Heart Failure",
                                                             "Class I",
                                                             "Class II",
                                                             "Class III",
                                                             "Class IV")),
      
      hftype = case_when(
        input$prior_heart_failure == "No" ~ "No Hx Heart Failure",
        input$hf_type == "Diastolic Heart Failure" ~ "Diastolic heart failure",
        input$hf_type == "Systolic Heart Failure" ~ "Systolic Heart Failure",
        input$hf_type == "Heart Failure Type Not Documented" ~ "Heart Failure Type Not Documented"
      ) %>% factor(levels = c("No Hx Heart Failure",
                              "Diastolic heart failure",
                              "Systolic Heart Failure",
                              "Heart Failure Type Not Documented")),
      tobaccouse = case_when(
        input$tobacco == "Never Used Tobacco" ~ "Never Used Tobacco",
        input$tobacco == "Former Use" ~ "Former User",
        input$tobacco == "Current Some Day Use" ~ "Current Some Day User",
        input$tobacco == "Current Every Day Use" ~ "Current Every Day User"
      ) %>% factor(levels = c("Never Used Tobacco",
                              "Former User",
                              "Current Some Day User",
                              "Current Every Day User")),
      cardiac_arrest_any = as.factor(input$cardiac_arrest_any),
      leftheartcath = case_when(
        input$leftheartcath == "Yes" ~ 1L,
        input$leftheartcath == "No"  ~ 0L
      ),
      prior_cvd = case_when(
        input$prior_cvd == "Yes" ~ 1L,
        input$prior_cvd == "No"  ~ 0L
      ), 
      pci_indication = as.factor(input$pci_indication),
      cath_lab_indication_1 = case_when(
        str_detect(indications(), "Syncope") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_2 = case_when(
        str_detect(indications(), "Cardiomyopathy") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_5 = case_when(
        str_detect(indications(), "Other") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_6 = case_when(
        str_detect(indications(), "Stable Known Coronary Artery Disease") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_7 = case_when(
        str_detect(indications(), "Cardiac Arrest with Successful Resuscitation") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_8 = case_when(
        str_detect(indications(), "Cardiac Arrhythmia") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_10 = case_when(
        str_detect(indications(), "Stable Suspected Coronary Artery Desease") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_11 = case_when(
        str_detect(indications(), "Heart Valve Disorder") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_13 = case_when(
        str_detect(indications(), "ACS <= 24 Hrs Prior to Cath Lab Visit") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_14 = case_when(
        str_detect(indications(), "ACS > 24 Hrs Prior to Cath Lab Visit") ~ TRUE, 
        TRUE ~ FALSE),
      cath_lab_indication_15 = case_when(
        str_detect(indications(), "Pre-Operative Evaluation") ~ TRUE, 
        TRUE ~ FALSE),
      stress_test_result = input$stress_test  %>% factor(
        levels = c("No Stress Testing",
                   "Negative",
                   "Positive - Low Risk of Ischemia",
                   "Positive - Intermediate Risk of Ischemia",
                   "Positive - High Risk of Ischemia",
                   "Positive - Risk Unknown",
                   "Indeterminate/Unavailable")
      ),
      pre_lvef = case_when(
        input$lvef == "No" ~ 55,
        TRUE ~ as.numeric(input$lvef_val)
      ),
      pre_lvef_NA = case_when(
        input$lvef == "No" ~ 1L,
        TRUE ~ 0L,
      ) %>% as.numeric(),
      temp = 1L
    ) %>% 
      bind_rows(
        fix_levels
      ) %>% 
      filter(temp == 1L)
  )
  
  mort_preds <- reactive(
    predictions(mortality, data_tbl())
  )
  aki_preds <- reactive(
    predictions(aki, data_tbl())
  )
  nrd_preds <- reactive(
    predictions(nrd, data_tbl())
  )
  maj_bleed_preds <- reactive(
    predictions(maj_bleed, data_tbl())
  )
  trnsf_preds <- reactive(
    predictions(transfusion, data_tbl())
  )
  stroke_preds <- reactive(
    predictions(stroke, data_tbl())
  )
  
  output$mort_graph <- renderPlot({
    validate(
      need(input$age, "Please enter an age."),
      need(input$height, "Please enter a height."),
      need(input$weight, "Please enter a weight."),
      need(input$systolic, "Please enter a value for systolic blood pressure."),
      need(input$creat_val, "Please enter a value for creatinine, or select not known."),
      need(input$hdl_val, "Please enter a value for HDL cholesterol, or select not known."),
      need(input$total_val, "Please enter a value for total cholesterol, or select not known."),
      need(input$hgb_val, "Please enter a value for hemoglobin, or select not known."),
      need(input$lvef_val, "Please enter a value for LVEF, or select not known."),
      errorClass = "validate"
    )
    gen_graph(mort_preds())
  })
  output$mort_text <- output$mort_desc <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_text(mort_preds(), "not survive")
  })
  output$mort_rev_text <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_rev_text(mort_preds(), "die")
  })
  output$aki_graph <- renderPlot({
    validate(
      need(input$age, "Please enter an age."),
      need(input$height, "Please enter a height."),
      need(input$weight, "Please enter a weight."),
      need(input$systolic, "Please enter a value for systolic blood pressure."),
      need(input$creat_val, "Please enter a value for creatinine, or select not known."),
      need(input$hdl_val, "Please enter a value for HDL cholesterol, or select not known."),
      need(input$total_val, "Please enter a value for total cholesterol, or select not known."),
      need(input$hgb_val, "Please enter a value for hemoglobin, or select not known."),
      need(input$lvef_val, "Please enter a value for LVEF, or select not known."),
      errorClass = "validate"
    )
    gen_graph(aki_preds())
  })
  output$aki_text <- output$aki_desc <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_text(aki_preds(), "experience acute kidney injury*")
  })
  output$aki_rev_text <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_rev_text(aki_preds(), "experience acute kidney injury")
  })
  output$nrd_graph <- renderPlot({
    validate(
      need(input$age, "Please enter an age."),
      need(input$height, "Please enter a height."),
      need(input$weight, "Please enter a weight."),
      need(input$systolic, "Please enter a value for systolic blood pressure."),
      need(input$creat_val, "Please enter a value for creatinine, or select not known."),
      need(input$hdl_val, "Please enter a value for HDL cholesterol, or select not known."),
      need(input$total_val, "Please enter a value for total cholesterol, or select not known."),
      need(input$hgb_val, "Please enter a value for hemoglobin, or select not known."),
      need(input$lvef_val, "Please enter a value for LVEF, or select not known."),
      errorClass = "validate"
    )
    gen_graph(nrd_preds())
  })
  output$nrd_text <- output$nrd_desc <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_text(nrd_preds(), "require dialysis*")
  })
  output$nrd_rev_text <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_rev_text(nrd_preds(), "require dialysis")
  })
  output$majbleed_graph <- renderPlot({
    validate(
      need(input$age, "Please enter an age."),
      need(input$height, "Please enter a height."),
      need(input$weight, "Please enter a weight."),
      need(input$systolic, "Please enter a value for systolic blood pressure."),
      need(input$creat_val, "Please enter a value for creatinine, or select not known."),
      need(input$hdl_val, "Please enter a value for HDL cholesterol, or select not known."),
      need(input$total_val, "Please enter a value for total cholesterol, or select not known."),
      need(input$hgb_val, "Please enter a value for hemoglobin, or select not known."),
      need(input$lvef_val, "Please enter a value for LVEF, or select not known."),
      errorClass = "validate"
    )
    gen_graph(maj_bleed_preds())
  })
  output$majbleed_text <- output$majbleed_desc <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_text(maj_bleed_preds(), "develop major bleeding")
  })
  output$majbleed_rev_text <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_rev_text(maj_bleed_preds(), "develop major bleeding")
  })
  output$trnsf_graph <- renderPlot({
    validate(
      need(input$age, "Please enter an age."),
      need(input$height, "Please enter a height."),
      need(input$weight, "Please enter a weight."),
      need(input$systolic, "Please enter a value for systolic blood pressure."),
      need(input$creat_val, "Please enter a value for creatinine, or select not known."),
      need(input$hdl_val, "Please enter a value for HDL cholesterol, or select not known."),
      need(input$total_val, "Please enter a value for total cholesterol, or select not known."),
      need(input$hgb_val, "Please enter a value for hemoglobin, or select not known."),
      need(input$lvef_val, "Please enter a value for LVEF, or select not known."),
      errorClass = "validate"
    )
    gen_graph(trnsf_preds())
  })
  output$trnsf_text <- output$trnsf_desc <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_text(trnsf_preds(), "need a transfusion")
  })
  output$trnsf_rev_text <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_rev_text(trnsf_preds(), "need a transfusion")
  })
  output$stroke_graph <- renderPlot({
    validate(
      need(input$age, "Please enter an age."),
      need(input$height, "Please enter a height."),
      need(input$weight, "Please enter a weight."),
      need(input$systolic, "Please enter a value for systolic blood pressure."),
      need(input$creat_val, "Please enter a value for creatinine, or select not known."),
      need(input$hdl_val, "Please enter a value for HDL cholesterol, or select not known."),
      need(input$total_val, "Please enter a value for total cholesterol, or select not known."),
      need(input$hgb_val, "Please enter a value for hemoglobin, or select not known."),
      need(input$lvef_val, "Please enter a value for LVEF, or select not known."),
      errorClass = "validate"
    )
    gen_graph(stroke_preds())
  })
  output$stroke_text <- output$stroke_desc <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_text(stroke_preds(), "experience a stroke")
  })
  output$stroke_rev_text <- renderText({
    validate(
      need(input$age, ""),
      need(input$height, ""),
      need(input$weight, ""),
      need(input$systolic, ""),
      need(input$creat_val, ""),
      need(input$hdl_val, ""),
      need(input$total_val, ""),
      need(input$hgb_val, ""),
      need(input$lvef_val, "")
    )
    gen_rev_text(stroke_preds(), "experience a stroke")
  })
  
  output$report <- downloadHandler(
    
    filename = "risk-summary.pdf",
    
    content = function(file){
      
      tempReport <- file.path(tempdir(), "generate-report.Rmd")
      file.copy("generate-report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        rpt_mort_text     = gen_probs_text(mort_preds(), "Mortality"),
        rpt_trnsf_text    = gen_probs_text(trnsf_preds(), "Transfusion"),
        rpt_stroke_text   = gen_probs_text(stroke_preds(), "Stroke"),
        rpt_aki_text      = gen_probs_text(aki_preds(), "Acute Kidney Injury*"),
        rpt_nrd_text      = gen_probs_text(nrd_preds(), "New Dialysis*"),
        rpt_majbleed_text = gen_probs_text(maj_bleed_preds(), "Major Bleeding"),
        
        rpt_mort_sum_text     = gen_text(mort_preds(), "die"),
        rpt_trnsf_sum_text    = gen_text(trnsf_preds(), "need a transfusion"),
        rpt_stroke_sum_text   = gen_text(stroke_preds(), "experience a stroke"),
        rpt_aki_sum_text      = gen_text(aki_preds(), "experience acute kidney injury"),
        rpt_nrd_sum_text      = gen_text(nrd_preds(), "require dialysis"),
        rpt_majbleed_sum_text = gen_text(maj_bleed_preds(), "develop major bleeding"),
        
        rpt_mort_rev_text     = gen_rev_text(mort_preds(), "die"),
        rpt_trnsf_rev_text    = gen_rev_text(trnsf_preds(), "need a transfusion"),
        rpt_stroke_rev_text   = gen_rev_text(stroke_preds(), "experience a stroke"),
        rpt_aki_rev_text      = gen_rev_text(aki_preds(), "experience acute kidney injury"),
        rpt_nrd_rev_text      = gen_rev_text(nrd_preds(), "require dialysis"),
        rpt_majbleed_rev_text = gen_rev_text(maj_bleed_preds(), "develop major bleeding"),
        
        rpt_trnsf         = predictions(transfusion, data_tbl()),
        rpt_stroke        = predictions(stroke, data_tbl()),
        rpt_aki           = predictions(aki, data_tbl()),
        rpt_nrd           = predictions(nrd, data_tbl()),
        rpt_majbleed      = predictions(maj_bleed, data_tbl()),
        rpt_mort          = predictions(mortality, data_tbl())
      )
      
      id <- showNotification("Preparing report for download...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = T)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    } 
  ) 
  
  
  output$htmllist <- reactive({
    paste("<h4>Rate of In-Hospital Adverse Events::</h4>",
          "<ul>",
          "<li>", gen_probs_text(mort_preds(), "Mortality"),"</li>",
          "<li>", gen_probs_text(trnsf_preds(), "Transfusion"),"</li>",
          "<li>", gen_probs_text(stroke_preds(), "Stroke"),"</li>",
          "<li>", gen_probs_text(aki_preds(), "Acute Kidney Injury*"),"</li>",
          "<li>", gen_probs_text(nrd_preds(), "New Dialysis*"),"</li>",
          "<li>", gen_probs_text(maj_bleed_preds(), "Major Bleeding"),"</li>",
          "</ul>",
          p("*Predictions are based only on patients not on dialysis at the time of PCI."))
  })
  
  
  
})
