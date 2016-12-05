library(shiny)
library(ggplot2)
library(survminer)
library(survival)

load("dfll.rda")
dfll$pat <- substr(dfll$pat, 1, 12)
load("clinical_expression_mut.rda")
clinical_expression_mut2 <- merge(clinical_expression_mut, dfll, by.x="X_PATIENT", by.y="pat")

#table(clinical_expression_mut2$X_cohort, clinical_expression_mut2$tp53)
clinical_expression_mut2$tp53 <- ifelse(clinical_expression_mut2$tp53 == -1, "LOH", "-")
#df <- (clinical_expression_mut[, , drop=FALSE])
df <- (clinical_expression_mut2[, , drop=FALSE])

clinical_expression_mut <- clinical_expression_mut[!grepl(clinical_expression_mut$X_cohort, pattern = "Formalin"), ]

clinical_expression_mut$MDM2
clinical_expression_mut$DNAJB1

ggplot(clinical_expression_mut, aes(MDM2, DNAJB1)) +
  geom_point() + facet_wrap(~X_cohort)


df <- (clinical_expression_mut2[clinical_expression_mut2$X_cohort == "TCGA Breast Cancer", , drop=FALSE])

hspgene = "MDM2"

df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,0,100), labels = paste("MDM2",c("low", "high")), right=FALSE)
df$hsp <- cut(df[,hspgene], breaks = c(-100,0,100), labels = paste(hspgene,c("low", "high")), right=FALSE)

#df$MDM2b <- cut(df[,"MDM2"], breaks = c(-100,median(df[,"MDM2"], na.rm = TRUE),100), labels = paste("MDM2",c("low", "high")), right=FALSE)
#df$hsp <- cut(df[,hspgene], breaks = c(-100,median(df[,hspgene], na.rm = TRUE),100), labels = paste(hspgene,c("low", "high")), right=FALSE)

df$TP53 = ifelse(df$X18475 == "1", "TP53 mut", "TP53 wild")

df <- na.omit(df[,c("X_TIME_TO_EVENT", "X_EVENT", "TP53", "MDM2b", "hsp", "tp53")])
df$g <- factor(paste(df$MDM2b, df$TP53, df$hsp))

table(df$g, df$tp53)


df2 <- df[df$tp53 == "LOH", ]
df2$g2 = factor(df2$g == "MDM2 high TP53 mut MDM2 high")
model <- survfit(Surv(X_TIME_TO_EVENT,X_EVENT) ~ g2, data=df2)
pp <- pchisq(survdiff(Surv(X_TIME_TO_EVENT,X_EVENT) ~ g2, data=df2)$chisq, nlevels(df2$g2)-1, lower.tail = F)

ggsurvplot(model, xlim=c(0,3000), main=paste0("Low/High ",hspgene, "\n  LOH p:", signif(pp,2)), # , "\ncases: ", nrow(df)
           legend="none",
           risk.table = TRUE, risk.table.y.text.col = TRUE)$plot
