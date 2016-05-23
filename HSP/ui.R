library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HSP + MDM2 + tp53"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3,
         selectInput("cohort", "Cancer:",
                     c("PANCAN12","TCGA Acute Myeloid Leukemia", "TCGA Bladder Cancer", "TCGA Breast Cancer", 
                       "TCGA Colon Cancer", "TCGA Endometrioid Cancer", "TCGA Formalin Fixed Paraffin-Embedded Pilot Phase II", 
                       "TCGA Glioblastoma", "TCGA Head and Neck Cancer", "TCGA Kidney Clear Cell Carcinoma", 
                       "TCGA Lung Adenocarcinoma", "TCGA Lung Squamous Cell Carcinoma", 
                       "TCGA Ovarian Cancer", "TCGA Rectal Cancer"),
                     "TCGA Breast Cancer"),
         selectInput("hspgene", "HSP 40 / 70 / 90 / TP:",
                     c("MDM2", "TP53", "TP63", "TP73", "DNAJB1", "DNAJB2", "DNAJB4", "DNAJB5", "DNAJB6", "DNAJB9", "DNAJB11", "DNAJB12", "DNAJB13", "DNAJB14",
                       "HSPA1A", "HSPA1B", "HSPA1L", "HSPA2", "HSPA5", "HSPA6", "HSPA7", "HSPA8", "HSPA9", "HSPA12A", "HSPA12B", "HSPA13", "HSPA14", "HSP90AA1", "HSP90AB1", "HSP90B1", "TRAP1"),
                     "TP63"),
         checkboxInput("groups42", "4 groups / 2 groups", TRUE),
         checkboxInput("median", "Median / 0 split", TRUE),
         plotOutput("distPlot", width = "100%", height = "350px")
    ),
    column(4,
           plotOutput("distPlot2", width = "100%", height = "400px"),
           plotOutput("distPlot5", width = "100%", height = "400px")
    ),
    column(4,
           plotOutput("distPlot3", width = "100%", height = "400px"),
           plotOutput("distPlot4", width = "100%", height = "400px")
    )
  )
))
