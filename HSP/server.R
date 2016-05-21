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
    hspgene = input$hspgene
    
    df <- na.omit(clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,
                                          c(hspgene), drop=FALSE])

    pl <- ggplot(df, aes_string(hspgene)) +
      geom_histogram()+
      ggtitle(paste(hspgene, "in\n", cohort,"\n", nrow(df), "cases"))
    if (input$median) {
      pl <- pl + geom_vline(xintercept = 0, color="red", size=2) 
    } else { 
      pl <- pl + geom_vline(xintercept = median(df[,hspgene], na.rm = TRUE), color="red", size=2) 
    }
    pl
  })
  
  output$distPlot2 <- renderPlot({
    cohort <- "TCGA Breast Cancer"
    hspgene = "DNAJB2"
    
    df <- clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,]
    df <- df[df$X18475 == "1", ]
    
    hspgene = input$hspgene
    if (input$median) {
      df$bin <- cut(df[,hspgene], breaks = c(-100,0,100), labels = paste(hspgene,c("low", "high"), "/ mut53"))
    } else { 
      df$bin <- cut(df[,hspgene], breaks = c(-100,median(df[,hspgene], na.rm = TRUE),100), labels = paste(hspgene,c("low", "high"), "/ mut53"))
    }

    df <- na.omit(df)
    model <- survfit(Surv(X_TIME_TO_EVENT,X_EVENT) ~ bin, data=df)
    pp <- pchisq(survdiff(Surv(X_TIME_TO_EVENT,X_EVENT) ~ bin, data=df)$chisq, 1, lower.tail = F)

    ggsurvplot(model)$plot + 
      ggtitle(paste0("High/Low ",hspgene, "\nOnly mut tp53\np-value: ", signif(pp,2))) +
      theme(legend.position=c(0.2,0.1))
  })
  
  # tylko HIGH
  output$distPlot3 <- renderPlot({
    df <- clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,]
    
    hspgene = input$hspgene
    if (input$median) {
      df <- df[which(df[,hspgene] > 0),]
      df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,0,100), labels = paste("MDM2",c("low", "high")))
    } else { 
      df <- df[which(df[,hspgene] > median(df[,hspgene], na.rm = TRUE)),]
      df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,median(df[,"MDM2"], na.rm = TRUE),100), labels = paste("MDM2",c("low", "high")))
    }
    
    df$TP53 = ifelse(df$X18475 == "1", "TP53 mut", "TP53 wild")
    
    df <- na.omit(df[,c("X_TIME_TO_EVENT", "X_EVENT", "TP53", "MDM2b")])
    model <- survfit(Surv(X_TIME_TO_EVENT,X_EVENT) ~ TP53 + MDM2b, data=df)
    pp <- pchisq(survdiff(Surv(X_TIME_TO_EVENT,X_EVENT) ~ TP53 + MDM2b, data=df)$chisq, 3, lower.tail = F)
    
    ggsurvplot(model)$plot + ggtitle(paste0("p-value: ", signif(pp,2))) +
      ggtitle(paste0("Only HIGH ",hspgene, "\np-value: ", signif(pp,2))) +
      theme(legend.position=c(0.3,0.15))
  })
  
  
  # tylko LOW
  output$distPlot4 <- renderPlot({
    df <- clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,]
    
    hspgene = input$hspgene
    if (input$median) {
      df <- df[which(df[,hspgene] <= 0),]
      df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,0,100), labels = paste("MDM2",c("low", "high")))
    } else { 
      df <- df[which(df[,hspgene] <= median(df[,hspgene], na.rm = TRUE)),]
      df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,median(df[,"MDM2"], na.rm = TRUE),100), labels = paste("MDM2",c("low", "high")))
    }
    
    df$TP53 = ifelse(df$X18475 == "1", "TP53 mut", "TP53 wild")
    
    df <- na.omit(df[,c("X_TIME_TO_EVENT", "X_EVENT", "TP53", "MDM2b")])
    model <- survfit(Surv(X_TIME_TO_EVENT,X_EVENT) ~ TP53 + MDM2b, data=df)
    pp <- pchisq(survdiff(Surv(X_TIME_TO_EVENT,X_EVENT) ~ TP53 + MDM2b, data=df)$chisq, 3, lower.tail = F)
    
    ggsurvplot(model)$plot + ggtitle(paste0("p-value: ", signif(pp,2))) +
      ggtitle(paste0("Only LOW ",hspgene, "\np-value: ", signif(pp,2))) +
      theme(legend.position=c(0.3,0.15))
  })
  
  # tylko 
  output$distPlot5 <- renderPlot({
    df <- clinical_expression_mut[clinical_expression_mut$X_cohort == input$cohort,]
    
    hspgene = input$hspgene
    if (input$median) {
      df$gene <- cut(df[,hspgene], breaks = c(-100,0,100), labels = paste(hspgene,c("low", "high")))
      df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,0,100), labels = paste("MDM2",c("low", "high")))
    } else { 
      df$gene <- cut(df[,hspgene], breaks = c(-100,median(df[,hspgene], na.rm = TRUE),100), labels = paste(hspgene,c("low", "high")))
      df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,median(df[,"MDM2"], na.rm = TRUE),100), labels = paste("MDM2",c("low", "high")))
    }
    
    df$TP53 = ifelse(df$X18475 == "1", "TP53 mut", "TP53 wild")
    
    df <- na.omit(df[,c("X_TIME_TO_EVENT", "X_EVENT", "TP53", "MDM2b", "gene")])
    df$MDM2_TP53 <- factor(paste(df$MDM2b, df$TP53))
    tmp <- data.frame(table(df$MDM2_TP53, df$gene))
    
    ggplot(tmp, aes(Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity") +
      coord_flip() + theme(legend.position="bottom") + xlab("") + ylab("")
  })
  
})





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
# clinical_expression <- merge(clinical, expression[,c("X_PATIENT", "TP63", "TP73", "DNAJB1", "DNAJB2", "DNAJB4", "DNAJB5", "DNAJB6", "DNAJB9", "DNAJB11", "DNAJB12", "DNAJB13", "DNAJB14", "MDM2", "TP73", "TP63")],  by="X_PATIENT", all.x=TRUE)
# 
# TP53 <- mutation.cb[grep(mutation.cb[,1], pattern="TP53$", value = FALSE),-1]
# TP53v <- data.frame(X_PATIENT = substr(names(TP53), 1, 12),TP53=t(TP53))
# 
# clinical_expression_mut <- merge(clinical_expression, TP53v, by="X_PATIENT", all.x=TRUE)
# 
# save(clinical_expression_mut, file="clinical_expression_mut.rda")
