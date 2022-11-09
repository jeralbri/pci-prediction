library(glue)
library(caret)
library(personograph)

predictions <- function(mod_obj, new_data){
  
  predict(mod_obj, newdata = new_data, type = "prob") %>% 
    pull(2)
  
}

gen_text <- function(x, outcome){
  
  if(x < 0.0004999){
    glue("Less than one in 1,000 patients may { outcome } following PCI while in the hospital.")
  } else {
    glue("{round(x*1000)} in 1,000 patients may { outcome } following PCI while in the hospital.")
  } 
  
}


gen_graph <- function(x){
  
  probs <- list(`Experience Event` = x,
                `Do Not Experience Event` = 1-x)
  
  suppressWarnings(
    personograph(probs, n.icons=1000, draw.legend = F, plot.width=1.0, dimensions=c(20,50), 
                 colors = list(`Experience Event` = "#B7202E",
                               `Do Not Experience Event` = "#BBBDC0"))
  )
}


linebreaks <- function(n){

    HTML(strrep(br(), n))

}

