library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HSP + MDM2 + tp53"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("cohort", "Cancer:",
                   c("TCGA Acute Myeloid Leukemia", "TCGA Bladder Cancer", "TCGA Breast Cancer", 
                     "TCGA Colon Cancer", "TCGA Endometrioid Cancer", "TCGA Formalin Fixed Paraffin-Embedded Pilot Phase II", 
                     "TCGA Glioblastoma", "TCGA Head and Neck Cancer", "TCGA Kidney Clear Cell Carcinoma", 
                     "TCGA Lung Adenocarcinoma", "TCGA Lung Squamous Cell Carcinoma", 
                     "TCGA Ovarian Cancer", "TCGA Rectal Cancer"),
                   "TCGA Breast Cancer"),
       selectInput("hspgene", "HSP 40:",
                   c("DNAJB1", "DNAJB2", "DNAJB4", "DNAJB5", "DNAJB6", "DNAJB9", "DNAJB11", "DNAJB12", "DNAJB13", "DNAJB14"),
                   "DNAJB1"),
       selectInput("TP", "TP:",
                   c("TP63", "TP73"),
                   "TP63")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot3"),
      plotOutput("distPlot2"),
      plotOutput("distPlot")
    )
  )
))
