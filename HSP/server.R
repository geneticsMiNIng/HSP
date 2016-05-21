library(shiny)
library(RTCGA.PANCAN12)
library(ggplot2)
library(survminer)
library(survival)

load("clinical_expression_mut.rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    cohort <- "TCGA Breast Cancer"
    hspgene = "DNAJB2"

    df <- na.omit(clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,])

    hspgene = input$hspgene
    ggplot(df, aes_string(hspgene)) +
      geom_histogram() + geom_vline(xintercept = 0, color="red", size=2)
  })
  
  output$distPlot2 <- renderPlot({
    cohort <- "TCGA Breast Cancer"
    hspgene = "DNAJB2"
    
    df <- clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,]
    
    df <- df[df$X18475 == "1", ]
    
    hspgene = input$hspgene
    df$bin <- cut(df[,hspgene], breaks = c(-100,0,100), labels = paste(hspgene,c("low", "high"), "mut53"))

    df <- na.omit(df)
    model <- survfit(Surv(X_TIME_TO_EVENT,X_EVENT) ~ bin, data=df)
    pp <- pchisq(survdiff(Surv(X_TIME_TO_EVENT,X_EVENT) ~ bin, data=df)$chisq, 1, lower.tail = F)

    ggsurvplot(model)$plot + ggtitle(paste0("p-value: ", signif(pp,2)))

  })
  
  output$distPlot3 <- renderPlot({
    df <- clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,]
    
    df$MDM2_TP53 = ifelse(df$X18475 == "1" & df$MDM2 > 0, 
                          "MDM2_high_TP53_mut",
                          "others")
    
    hspgene = input$hspgene
    df$bin <- cut(df[,hspgene], breaks = c(-100,0,100), labels = paste(hspgene,c("low", "high"), "mut53"))

    df <- na.omit(df)
    model <- survfit(Surv(X_TIME_TO_EVENT,X_EVENT) ~ MDM2_TP53 + bin, data=df)
    pp <- pchisq(survdiff(Surv(X_TIME_TO_EVENT,X_EVENT) ~ MDM2_TP53 + bin, data=df)$chisq, 1, lower.tail = F)
    
    ggsurvplot(model)$plot + ggtitle(paste0("p-value: ", signif(pp,2))) +
      theme(legend.position="right")
    
  })
  
  
  
})




# 
# clinical <- clinical.cb[,c("X_PATIENT", "X_TIME_TO_EVENT", "X_EVENT", "X_cohort")]
# clinical$X_PATIENT <- substr((clinical$X_PATIENT), 1, 12)
# clinical$X_PATIENT <- gsub(clinical$X_PATIENT, pattern="-", replacement=".")
# 
# # expression
# expression <- rbind(expression.cb1, expression.cb2)
# rownames(expression) <- expression[,1]
# expression <- expression[,-1]
# expression <- t(expression)
# expression <- as.data.frame(expression)
# expression$X_PATIENT <- substr(rownames(expression), 1, 12)
# expression$X_PATIENT <- gsub(expression$X_PATIENT, pattern="-", replacement=".")
# 
# # mutation
# clinical_expression <- merge(clinical, expression[,c("X_PATIENT", "DNAJB1", "DNAJB2", "DNAJB4", "DNAJB5", "DNAJB6", "DNAJB9", "DNAJB11", "DNAJB12", "DNAJB13", "DNAJB14", "MDM2", "TP73", "TP63")],  by="X_PATIENT", all.x=TRUE)
# 
# TP53 <- mutation.cb[grep(mutation.cb[,1], pattern="TP53$", value = FALSE),-1]
# TP53v <- data.frame(X_PATIENT = substr(names(TP53), 1, 12),TP53=t(TP53))
# 
# clinical_expression_mut <- merge(clinical_expression, TP53v, by="X_PATIENT", all.x=TRUE)
# 
