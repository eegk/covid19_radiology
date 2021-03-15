### load libraries
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
libs<-c( "gridExtra","ggplot2","ggpubr","readxl","reshape2","tidyverse","lubridate","magrittr","broom" ,
         "stringr","psych","RColorBrewer","glmnet","ROCR","pheatmap","questionr","ggbeeswarm","caret")
lapply(libs, require, character.only = TRUE) ; rm(libs)
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### set directory
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
setwd("/Users/gonzae34/Documents/projects_gnjatic/covid19/project_diane/")
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### formal sample processing
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### test_tmp <- read_excel("diane_analysis/final_data_GC_DD_V5.xlsx", sheet = "DATA")
data <- read_excel("diane_analysis/final_data_GC_DD_V5.xlsx", sheet = "DATA")
### remove acc_Ct
data$Acc_CT <- NULL
### modify smoking factor
data <- data %>% replace_na(list(`Smoking factor` = "Unknown")) %>% mutate(`Smoking factor` = factor(`Smoking factor`, levels=c("Yes","Quit","Never","Unknown")))
### remove this two
ix <- which(colnames(data) %in% c("Patient_number","MRN"))
data <- data[,-ix]
### factor variables
ix <- which(colnames(data) %in% c("Other symptoms","Other chest abnormalities","Cancer diagnosis description","Smoking factor","Deceased date","Last follow-up",
                                  "Date of discharge","Intubation date","Intubation factor","Severity degree factor (MSH classification)","Cytokine assessment date",
                                  "CT scan date","Date of hospital admission","Onset of symptoms","Hospital factor","WHO","Ethnicity factor","Race factor","Sex factor"))
### change
data <- data %>% mutate_at(ix, as.character)
data <- data %>% mutate_at(ix, as.factor)
### numeric variables
ix <- which(!(colnames(data) %in% c("Other symptoms","Other chest abnormalities","Cancer diagnosis description","Smoking factor","Deceased date","Last follow-up",
                                  "Date of discharge","Intubation date","Intubation factor","Severity degree factor (MSH classification)","Cytokine assessment date",
                                  "CT scan date","Date of hospital admission","Onset of symptoms","Hospital factor","WHO","Ethnicity factor","Race factor","Sex factor")))
### change
data <- data %>% mutate_at(ix, as.character)
data <- data %>% mutate_at(ix, as.numeric)

### to df
data <- as.data.frame(data)
colnames(data)[colnames(data) %in% "Deceased"] <- "Deceased before discharge"
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### variables not to math
ix <- which(colnames(data) %in% c("Other symptoms","Other chest abnormalities","Cancer diagnosis description","Smoking factor","Deceased date","Last follow-up",
                                  "Date of discharge","Intubation date","Intubation factor","Severity degree factor (MSH classification)","Cytokine assessment date",
                                  "CT scan date","Date of hospital admission","Onset of symptoms","Hospital factor","WHO","Ethnicity factor","Race factor","Sex factor"))
### store all data
data_all <- data
### filter 
data <- data[,-ix]
### Test for differences between deceased and not
test_list <- list()
for (i in colnames(data)) {
  if (is.numeric(data[[i]])){ test_list[[i]] <- wilcox.test(data[[i]] ~ data$`Deceased before discharge`)
  } else if (is.factor(data[[i]])) { test_list[[i]] <- fisher.test(data[[i]], data$`Deceased before discharge`) }
} 
result <- bind_rows(lapply(test_list,glance),.id="Variable") 
result$adj.p.value <- p.adjust(result$p.value,method = "fdr")
result <- data.frame(result) ; rm(test_list)
result <- result[ order(result$adj.p.value, decreasing = FALSE), ]

result$ranking <- 1:nrow(result)
result$nLogFDR <- -log10(result$adj.p.value)

library(ggrepel)

pdf(file="univariate_tests.pdf",width = 16,height =  12)
ggplot(data=result) + aes(x=ranking,y=nLogFDR) + geom_point() + theme_bw() + 
  geom_text_repel(data=result[1:25,],aes(label=Variable),force = 2,max.iter = 500,nudge_x = seq(25,100,3.1), nudge_y=seq(28,4) ) +
  labs(x="Ranking",y="-Log10(FDR)") +
  theme(text = element_text(size=rel(4.8)),
        legend.text=element_text(size=rel(4.8)),
        plot.title=element_text(size=rel(4.8)) ) 
dev.off()

write.csv(file="statistics_variables.csv",result)

### Edgar's code:
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### PREPARE THE DATASET
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
### Separate outcomes
truth_survival <- as.numeric(as.character(data$`Deceased before discharge`))
truth_severity <- data_all$WHO

### separate numeric values
### data_numeric <- data[ ,sapply(data, function(x) is.numeric(x)) ] 
data_numeric <- data

### remove variables with NAs or NaNs
tmp_var <- list()
for ( i in 1:ncol(data_numeric) ) { if ( length(which(is.na(data_numeric[,i]))) > 0 ) { tmp_var[[i]] <- "FALSE" } else { tmp_var[[i]] <- "TRUE" } }
tmp_var <- as.logical(unlist(tmp_var)) 

#colnames(data_numeric)[tmp_var]
#data_numeric[,which(tmp_var)]

### remove hospital length as predictor
data_numeric <- data_numeric[,tmp_var] ; rm(tmp_var) 
#data_numeric <- data_numeric[,-which(colnames(data_numeric) %in% "Hospital_lenght_(d)")]
data_numeric <- as.matrix(data_numeric) ; rm(i)

#i <- which(colnames(data_numeric) %in% "ETHNICITY")
#data_numeric[,i] <- paste("p",as.character(data_numeric[,i]),sep="_")
#i <- which(colnames(data_numeric) %in% "RACE")
#data_numeric[,i] <- paste("p",as.character(data_numeric[,i]),sep="_")
#which(colnames(data_numeric) %in% c("ETHNICITY","RACE"))

### plots to compare some of the variables
my_tmp_plots <- list()

### variables to plot
list_of_names <- c("IL-6","IL-8","CT qualitative score","Age","GGO volume (ml)","Consolidation volume (ml)","Total volume (ml)","GGO/Aerated lung")

### Make plots for inspection
for( i in 1:length(list_of_names)) {
ix <- which(colnames(data_numeric) %in% list_of_names[i] )
my_tmp_plots[[i]] <- ggplot() + aes( x=as.factor(truth_survival),y=data_numeric[,ix], ) + geom_boxplot() + theme_bw() }

### Create new column for each cytokine to create Low v High group
data$IL6_group <- if_else(data$`IL-6` <= 70,"0","1")
data <- data %>% mutate(IL6_group = as.numeric(IL6_group))

data$IL8_group <- if_else(data$`IL-8` <= 50,"0","1")
data <- data %>% mutate(IL8_group = as.numeric(IL8_group))

data$IL1B_group <- if_else(data$`IL-1B` <= 0.5,"0","1")
data <- data %>% mutate(IL1B_group = as.numeric(IL1B_group))

data$TNF_ALPHA_group <- if_else(data$`TNF alpha` <= 35,"0","1")
data <- data %>% mutate(TNF_ALPHA_group = as.numeric(TNF_ALPHA_group))


### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Spearman Correlation
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
data_numeric <- data_numeric[,-grep(" \\(d\\)",colnames(data_numeric))]

### spearman rank-based measure of association
rm(my_storage) ; my_storage <- list() ; count=1
for (xxi in 1:ncol(data_numeric) ){
  for( xxy in 1:ncol(data_numeric) ){
    my_test <- cor.test(data_numeric[,xxi],data_numeric[,xxy],method="spearman")    
    my_storage[[count]] <- data.frame( p.value = my_test$p.value , S = my_test$statistic , Rho = my_test$estimate ,
                                       a = colnames(data_numeric)[xxi] , b = colnames(data_numeric)[xxy] )
    count=count+1  } }
###
my_storage <- do.call(rbind,my_storage)
my_test <- my_storage[,c("Rho","a","b")]
corr_data_rho_df <- as.data.frame( pivot_wider(data=my_test, names_from = a,values_from = Rho) )
rownames(corr_data_rho_df)<-corr_data_rho_df$b ; corr_data_rho_df <- corr_data_rho_df[,-1]
my_storage$nLog.p.val <- -log10(my_storage$p.value)
corr_data_long <- my_storage
rm(my_test,my_storage)
###
out <- pheatmap(corr_data_rho_df,
         color = colorRampPalette(c("steelblue","white","firebrick"))(255),
         angle_col = 90)
dev.off()
dev.off()
###
corr_data_long$a <- factor(corr_data_long$a, levels=out$tree_row$labels[out$tree_row$order])
corr_data_long$b <- factor(corr_data_long$b, levels=out$tree_col$labels[out$tree_col$order])
corr_data_long$sig <- corr_data_long$p.value < 0.05
corr_data_long$nLog.p.val[corr_data_long$nLog.p.val > 5] <- 5
###
pdf(file="spearman_correlation_matrix_numeric_variables.pdf",width = 17, height = 13)
ggplot(data=corr_data_long) + aes(x=a, y=b, size=nLog.p.val, color=Rho, shape=sig) + 
  theme_classic2() +
  scale_colour_gradient2(low="steelblue", mid="white", high="firebrick", space="Lab", na.value="grey50", guide="colourbar", aesthetics="colour",limits=c(-1,1)) +
  #geom_point( shape=16, colour = "black", aes(size = 1*max(nLog.p.val),color=Rho ) ) +
  #geom_point( shape=16, colour = "white", aes(size = 0.8*(nLog.p.val),color=Rho ) ) +
  geom_point( data=corr_data_long[which(corr_data_long$sig %in% "TRUE") , ], aes(x=a, y=b, size=0.81*nLog.p.val, color=Rho), shape=16) + 
  geom_point( data=corr_data_long[which(corr_data_long$sig %in% "FALSE") , ], aes(x=a, y=b, size=0.81*nLog.p.val), color="black", shape=4) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x="", y="", title = "") +
  theme(text = element_text(size=rel(4.8)), legend.text=element_text(size=rel(4.8)), plot.title=element_text(size=rel(4.8)) )
dev.off()
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### odds ratio from package "questionr"
### fisher.test ; Performs Fisher's exact test for testing the null of independence
### A logical indicating whether to compute p-values by Monte Carlo simulation, in larger than 2 by 2 tables.
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ix <- which(colnames(data_numeric) %in% "GGO/Aerated lung")
fisher.test(data_numeric[,ix],data_numeric[,ix],conf.level = 0.95,simulate.p.value=TRUE)


data_quantiles <- list()
for (ix in 1:ncol(data_numeric)){
  data_quantiles[[ix]] <- as.character(.bincode(data_numeric[,ix], breaks=quantile(data_numeric[,ix], probs=seq(0,1, by=0.25), na.rm=TRUE), include.lowest=TRUE))  
}

data_quantiles <- do.call(cbind,data_quantiles)
colnames(data_quantiles) <- colnames(data_numeric)

data_quantiles <- as.data.frame(data_quantiles)
data_quantiles <- data_quantiles %>% mutate_at(1:ncol(data_quantiles),as.character)
data_quantiles <- data_quantiles %>% mutate_at(1:ncol(data_quantiles),as.factor)

to_keep <- list()
for (ix in 1:ncol(data_quantiles)){ to_keep[[ix]] <- length(levels(data_quantiles[,ix])) > 1 }
ix <- which(unlist(to_keep))

data_quantiles <- data_quantiles[,ix]

###
rm(my_storage) ; my_storage <- list() ; count=1
for (xxi in 1:ncol(data_quantiles) ){
  for( xxy in 1:ncol(data_quantiles) ){
    my_test <- fisher.test(data_quantiles[,xxi],data_quantiles[,xxy],conf.level = 0.95,simulate.p.value=TRUE)    
    my_storage[[count]] <- data.frame( p.value = my_test$p.value , a = colnames(data_quantiles)[xxi] , b = colnames(data_quantiles)[xxy] )
    count=count+1  } }
###
my_storage <- do.call(rbind,my_storage)
fisher_data_long <- my_storage
fisher_data_df <- as.data.frame( pivot_wider(data=my_storage, names_from = a,values_from = p.value) )
rownames(fisher_data_df)<-fisher_data_df$b ; fisher_data_df <- fisher_data_df[,-1]
fisher_data_long$nLog.p.val <- -log10(fisher_data_long$p.value)
###
rm(my_test,my_storage)
###
out <- pheatmap(fisher_data_df,
                color = colorRampPalette(c("steelblue","white","firebrick"))(255),
                angle_col = 90)
dev.off()
dev.off()
###
fisher_data_long$a <- factor(fisher_data_long$a, levels=out$tree_row$labels[out$tree_row$order])
fisher_data_long$b <- factor(fisher_data_long$b, levels=out$tree_col$labels[out$tree_col$order])
fisher_data_long$sig <- fisher_data_long$p.value < 0.05
###
pdf(file="fisher_independence_test_matrix_numeric_variables.pdf",width = 17, height = 13)
ggplot(data=fisher_data_long) + aes(x=a,y=b,size=nLog.p.val,color=nLog.p.val,shape=sig) + theme_classic2() +
  scale_colour_viridis_c()+
  geom_point(shape=16, colour = "black", aes(size = 1*max(nLog.p.val))) +
  geom_point(shape=16, colour = "white", aes(size = 0.8*max(nLog.p.val))) +
  geom_point(data=fisher_data_long[which(fisher_data_long$sig %in% "TRUE"),],aes(x=a, y=b, size=0.81*nLog.p.val, color=nLog.p.val),shape=16) + 
  geom_point(data=fisher_data_long[which(fisher_data_long$sig %in% "FALSE"),],aes(x=a, y=b, size=0.81*nLog.p.val, color=nLog.p.val),shape=4) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x="", y="", title = "") +
  theme(text = element_text(size=rel(4.8)), legend.text=element_text(size=rel(4.8)), plot.title=element_text(size=rel(4.8)) )
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### results numeric
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
test_list <- list()
for (i in colnames(data_numeric)) { test_list[[i]] <- wilcox.test(data_numeric[which(data$Deceased_before_discharge %in% "0"),i], data_numeric[which(data$Deceased_before_discharge %in% "1"),i]) } 
results_numeric <- bind_rows(lapply(test_list,glance),.id="Variable") 
results_numeric <- as.data.frame(results_numeric)
results_numeric <- results_numeric[order(results_numeric$p.value,decreasing = FALSE),]
sort()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### List with scenarios needs to be writen only 1 time and then used further.
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#c("IL-6","IL-8","CT qualitative score","Age","GGO volume (ml)","Consolidation volume (ml)","Total volume (ml)","GGO/Aerated lung")

### Scenarios for testing
scenario1 = c("IL6_group","IL8_group","TNF_ALPHA_group","Age") #  cytokines
scenario2 = c("CT qualitative score","Age") # CT semi quantitative
scenario3 = c("GGO volume (ml)","Aerated lung volume (ml)","Total volume (ml)","Age")
### Complete list of variables per scenario tested
my_scenarios <- list(
  scenario1 = scenario1, # Cytokines
  scenario2 = scenario2, # CT semi quantitative
  scenario3 = scenario3, # CT quant
  scenario4 = c(scenario1,scenario2,scenario3), # both
  #scenario5 = colnames(data_numeric), # all variables
  scenario6 = c("GGO/Aerated lung","Age","TNF_ALPHA_group","IL6_group","IL8_group") #, ### Best model
  # scenario6a = c("GGO_AeratedLung_Ratio","Age","TNF_ALPHA_group","IL6_group"), # very similar to il8
  # scenario6b = c("GGO_AeratedLung_Ratio","Age","TNF_ALPHA_group","IL8_group")  # slightly better than il6
)
### as data frame
data_numeric <- as.data.frame(data_numeric)
#Create new column for each cytokine to create Low v High group
data_numeric$IL6_group <- data$IL6_group 
data_numeric$IL8_group <- data$IL8_group 
#data_numeric$IL1B_group <- data$IL1B_group 
data_numeric$TNF_ALPHA_group <- data$TNF_ALPHA_group 
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Figures for markers in scenarios
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data_numeric_tmp <- data_numeric
###
data_numeric_tmp$survival <- as.character(data$`Deceased before discharge`)
data_numeric_tmp$survival[data_numeric_tmp$survival %in% "1"] <- "Dead"
data_numeric_tmp$survival[data_numeric_tmp$survival %in% "0"] <- "Alive"
###
#table(data_numeric_tmp$IL6_group,data_numeric_tmp$survival)
#table(data_numeric_tmp$IL8_group,data_numeric_tmp$survival)
#table(data_numeric_tmp$TNF_ALPHA_group,data_numeric_tmp$survival)
###
cute_plots <- list()

library(ggbeeswarm)

my_comparisons <- list(c("Dead","Alive"))

cute_plots[[1]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="Age", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='Age',color="Status",fill="Status") + theme(text=element_text(size=21))

data_numeric_tmp$log10_il6 <- log10(data_numeric_tmp$`IL-6`)

cute_plots[[2]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="log10_il6", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='Log10(IL6)',color="Status",fill="Status") + theme(text=element_text(size=21))

data_numeric_tmp$log10_il8 <- log10(data_numeric_tmp$`IL-8`)

cute_plots[[3]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="log10_il8", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='Log10(IL8)',color="Status",fill="Status") + theme(text=element_text(size=21))

data_numeric_tmp$log10_tnfa <- log10(data_numeric_tmp$`TNF alpha`)

cute_plots[[4]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="log10_tnfa", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='Log10(TNF alpha)',color="Status",fill="Status") + theme(text=element_text(size=21))

cute_plots[[5]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="CT qualitative score", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='CT qualitative score',color="Status",fill="Status") + theme(text=element_text(size=21))

cute_plots[[6]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="GGO volume (ml)", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='GGO volume (ml)',color="Status",fill="Status") + theme(text=element_text(size=21))

cute_plots[[7]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="Aerated lung volume (ml)", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='Aerated lung volume (ml)',color="Status",fill="Status") + theme(text=element_text(size=21))

cute_plots[[8]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="Total volume (ml)", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='Total Volume mL',color="Status",fill="Status") + theme(text=element_text(size=21))

cute_plots[[9]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="GGO/Aerated lung", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='GGO/Aerated lung',color="Status",fill="Status") + theme(text=element_text(size=21))

data_numeric_tmp$BMI <- data$BMI
data_numeric_tmp$BMI[71] <- 21.85

cute_plots[[10]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="BMI", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='BMI',color="Status",fill="Status") + theme(text=element_text(size=21))

data_all$`O2 saturation venous`
data_numeric_tmp$o2sat <-data_all$`O2 saturation`

cute_plots[[11]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="o2sat", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='O2 saturation',color="Status",fill="Status") + theme(text=element_text(size=21))

data_numeric_tmp$max_who <- data$`Maximun severity degree (WHO scale)`

cute_plots[[12]] <- ggviolin(data=data_numeric_tmp, x="survival" , y="max_who", color="survival",fill="survival",alpha=0.5) +
  theme_classic() +  geom_quasirandom() + geom_boxplot(width=0.1) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE ) +
  theme(legend.position = "none") + 
  labs(x ='', y='', title='WHO score',color="Status",fill="Status") + theme(text=element_text(size=21))


pdf(file="figures_variables_used_in_models.pdf",width = 17,height =  15)
ggarrange( cute_plots[[1]],cute_plots[[2]],cute_plots[[3]],
           cute_plots[[4]],cute_plots[[5]],cute_plots[[6]],
           cute_plots[[7]],cute_plots[[8]],cute_plots[[9]],
           cute_plots[[10]],cute_plots[[11]],cute_plots[[12]], ncol=4, nrow=3, align = 'hv')
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### repeating the models n number of times
### validation 10x
### set.seed(42)
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### make empty dataframe to store results
results_survival_cv <- data.frame()
### make empty lists to store results
my_model.fit <- list()
my_coeficients <- list()
#my_ggplots <- list()
my_rocs <- list()
score_data <- list()
#co_list <- list()
subset_prob <- c(0.1,0.2,0.3,0.4,0.5)
###

### FOR loop to perform analysis and cross-fold validation
for ( repeats in 1:100 ) { #1:100
  #repeats=1
  ### test different subsets of variable sizes, 
  for ( r2 in 1:length(subset_prob) ) {
    ### partitions the data into testing and training
    #r2=1
    ix <- createDataPartition(truth_survival, p=subset_prob[r2])
    ix <- as.numeric(ix$Resample1)
    truth_survival_train <- truth_survival[-ix]
    truth_survival_test <- truth_survival[ix]
    truth_survival_test
    ### BUG: Sometimes produces only 1 class for test. Needs to be binary. Thus
    if ( length(levels(as.factor(truth_survival_test))) > 1 ) {
    ###
    for ( xyz in 1:length(my_scenarios) ){
      #xyz=1
      for (iii in 0:10) {
        ### Selects the scenario
        icol <- which(colnames(data_numeric) %in% my_scenarios[[xyz]] )
        ### name of a model
        name <- paste(names(my_scenarios)[xyz], iii/10, "repeat",repeats,"p",subset_prob[r2], sep="_")
        ### makes a model
        my_model.fit[[name]] <- cv.glmnet(x=as.matrix(data_numeric[-ix,icol]), y=truth_survival_train, family="binomial", alpha=iii/10 )
        ###
        ### prediction scores into dataframe
        score_data[[name]] <- data.frame(link=predict(my_model.fit[[name]],s=my_model.fit[[name]]$lambda.min,newx=as.matrix(data_numeric[ix,icol]), type="link")[,1],  
                                         response=predict(my_model.fit[[name]],s=my_model.fit[[name]]$lambda.min,newx=as.matrix(data_numeric[ix,icol]), type="response")[,1], 
                                         class=predict(my_model.fit[[name]],s=my_model.fit[[name]]$lambda.min,newx=as.matrix(data_numeric[ix,icol]), type="class")[,1],
                                         survival=as.factor(truth_survival_test) )
        ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ### Alternative Figure to compare prediction scores
        ### my_ggplots[[name]] <- score_data[[name]] %>% ggplot(aes(x=link, y=response, col=survival)) + scale_color_manual(values=c("black", "red")) + geom_point() + geom_rug() + theme_bw() + ggtitle("Scoring")
        ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ### prediction for ROC
        pred <- prediction( predictions=as.numeric(as.character(score_data[[name]]$response)), labels=as.numeric(as.character(score_data[[name]]$survival)) )
        # TPR/FPR Predictions
        perf <- performance(pred,"tpr","fpr")
        my_rocs[[name]] <- perf
        ### temporal data frame to pre-store results in the loop and add them to dataframe later.
        temp <- data.frame(alpha= iii/10, auc=performance(pred, measure = "auc")@y.values[[1]], model=name, prob=subset_prob[r2], scenario=names(my_scenarios)[xyz])
        ### concatenate with results
        results_survival_cv <- rbind(results_survival_cv, temp)
        ### store coefficients
        my_coeficients[[name]] <- cbind( coef(my_model.fit[[name]], s = "lambda.min") , coef(my_model.fit[[name]], s = "lambda.1se") )
        ### store names of lambda
        colnames(my_coeficients[[name]]) <- c("lambda.min","lambda.1se")
      } } } } }
###

my_rocs_survival <- my_rocs
my_coef_survival <- my_coeficients
my_score_survival <- score_data
my_models_survival <- my_model.fit

# results_cv <- results_cv[order(results_cv$auc,decreasing = TRUE),]
# as.data.frame(results_cv %>% group_by(scenario) %>% top_n(1,wt=auc))

### AUC plot
#plot(perf,colorize=FALSE, col="black") # Plot ROC curve
#lines(c(0,1),c(0,1),col = "gray", lty = 4 )

#pred <- prediction(score_data$response, score_data$survival)
#perf <- performance(pred, measure="acc", x.measure="cutoff")
# Get the cutoff for the best accuracy
#bestAccInd <- which.max(perf@"y.values"[[1]])
#bestMsg <- paste("best accuracy=", perf@"y.values"[[1]][bestAccInd], 
#                 " at cutoff=", round(perf@"x.values"[[1]][bestAccInd], 4))
#plot(perf, sub=bestMsg)

#ggplot(data=results_survival_cv) + aes(x=scenario,y=auc,color=scenario) + facet_wrap(~prob) + geom_boxplot() + theme_bw()
#ggplot(data=results) + aes(x=scenario,y=auc,color=scenario) + geom_boxplot() + theme_bw()

### write.csv(file="results_cv_90_vs_10_survival_models.csv",results_cv)

#summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'scenario1'])
#summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'scenario2']) # %in% looks for an exact string
#summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'scenario3'])
#summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'scenario4'])
#summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'scenario5'])
#summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'scenario6'])

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### pHeatmap For Scenario 5
### Can be used for Variable Selection by inspecting the regression coefficients.
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Use grep to match pattern "scenario5 and extract all coefficients for these models
# grep("scenario5",names(my_coeficients)) ### grep looks for a pattern
#tmp <- do.call(cbind, my_coeficients[grep("scenario5",names( my_coeficients))])
### Because the names were not stored BUT we know that we storing the coefficients for 2 models, ...
### we use 1 and 2 to select only the first set of coeccifients corresponding to lammbda min.
###
### #which(rep(c("1","2"),ncol(tmp)/2) %in% "1") # expression to select only the first out of 2 coeficients.
### subselect tmp for the coefficients in labmba min
###
#tmp_subset_lamba_min <- tmp[,which(rep(c("1","2"),ncol(tmp)/2) %in% "1")]
### name the models correctly. These were not named in the selection of coeeficients.
#colnames(tmp_subset_lamba_min) <- grep("scenario5",names( my_coeficients),value = TRUE)
### make the sparse matrix a dataframe. converts the "." to cero. 
#tmp_subset_lamba_min_df <- as.data.frame(as.matrix(tmp_subset_lamba_min))

### extracts the annotation for results from scenario 5.
#annotation_col <- results_cv[results_cv$scenario %in% 'scenario5',c(1,2)]
### Names
#rownames(annotation_col) <- results_cv[results_cv$scenario %in% 'scenario5',3] #name rows 
### review if the files are correctly selected and named
#rownames(annotation_col) %in% colnames(tmp_subset_lamba_min_df) #checking in rows in A are in columns in B
### reviews order
#identical(rownames(annotation_col), colnames(tmp_subset_lamba_min_df)) #checking for order
### orders based on AUC
#annotation_col <- annotation_col %>% arrange(desc(auc)) #order annotation col on auc
### vector with the correct order based on AUC
#ix <- match(rownames(annotation_col),colnames(tmp_subset_lamba_min_df)) #order B by A

### colors for plot
#myColor <- colorRampPalette(c("steelblue","white","firebrick"))(255)

### breaks for colors
#myBreaks <- c(seq(min(tmp_subset_lamba_min_df), 0, length.out=ceiling(255/2) + 1), 
#              seq(max(tmp_subset_lamba_min_df)/255, max(tmp_subset_lamba_min_df), length.out=floor(255/2)))

### simple plot sorted by coefs and not AUC
### pheatmap(tmp_subset_lamba_min_df,scale = 'row', annotation_col = annotation_col)
# Plot the heatmap
#scenario5_headmap <- pheatmap(tmp_subset_lamba_min_df[,ix],
#                              cluster_cols = FALSE, 
#                              show_colnames = FALSE,
#                              scale = 'none',
#                              annotation_col = annotation_col, 
#                              color = colorRampPalette(c("steelblue","white","firebrick"))(255),
#                              breaks = myBreaks)
###

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### FOR SEVERITY
### Testing with all the data.
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

truth_severity_adjusted <- as.character(truth_severity)
truth_severity_adjusted[truth_severity_adjusted %in% c("Mild","Moderate")] <- "0"
truth_severity_adjusted[truth_severity_adjusted %in% c("Severe")] <- "1"
truth_severity_adjusted[truth_severity_adjusted %in% c("Unable to score")] <- NA

data_numeric_SEV <- data_numeric
ix <- which(!is.na(truth_severity_adjusted))

truth_severity_adjusted <- truth_severity_adjusted[ix]
data_numeric_SEV <- data_numeric_SEV[ix,]


### make empty dataframe to store results
results_severity_cv <- data.frame()
### make empty lists to store results
my_model.fit <- list()
my_coeficients <- list()
#my_ggplots <- list()
my_rocs <- list()
score_data <- list()
#co_list <- list()
subset_prob <- c(0.1,0.2,0.3,0.4,0.5)

### FOR loop to perform analysis over all the data
for ( repeats in 1:100 ) {
  
  for ( r2 in 1:length(subset_prob) ) {
  
    ix <- createDataPartition(truth_severity_adjusted, p=subset_prob[r2])
    ix <- as.numeric(ix$Resample1)
    truth_severity_train <- truth_severity_adjusted[-ix]
    truth_severity_test <- truth_severity_adjusted[ix]
    #truth_severity_test
    
    for ( xyz in 1:length(my_scenarios) ){
      
      for (iii in 0:10) {
        ### Selects the scenario
        icol <- which(colnames(data_numeric_SEV) %in% my_scenarios[[xyz]] )
        ### name of a model
        name <- paste(names(my_scenarios)[xyz], iii/10, "repeat",repeats,"p",subset_prob[r2], sep="_")
        ### makes a model
        my_model.fit[[name]] <- cv.glmnet(x=as.matrix(data_numeric_SEV[-ix,icol]), y=truth_severity_train, family="binomial", alpha=iii/10 )
        ###
        ### prediction scores into dataframe
        score_data[[name]] <- data.frame(link=predict(my_model.fit[[name]],s=my_model.fit[[name]]$lambda.min,newx=as.matrix(data_numeric_SEV[ix,icol]), type="link")[,1],  
                                         response=predict(my_model.fit[[name]],s=my_model.fit[[name]]$lambda.min,newx=as.matrix(data_numeric_SEV[ix,icol]), type="response")[,1], 
                                         class=predict(my_model.fit[[name]],s=my_model.fit[[name]]$lambda.min,newx=as.matrix(data_numeric_SEV[ix,icol]), type="class")[,1],
                                         severity=as.factor(truth_severity_test) )
        ### prediction for ROC
        pred <- prediction( predictions=as.numeric(as.character(score_data[[name]]$response)), labels=as.numeric(as.character(score_data[[name]]$severity)) )
        # TPR/FPR Predictions
        perf <- performance(pred,"tpr","fpr")
        my_rocs[[name]] <- perf
        ### temporal data frame
        temp <- data.frame(alpha= iii/10, auc=performance(pred, measure = "auc")@y.values[[1]], model=name, prob=subset_prob[r2], scenario=names(my_scenarios)[xyz])
        ### concatenate with results
        results_severity_cv <- rbind(results_severity_cv, temp)
        ### store coefficients
        my_coeficients[[name]] <- cbind( coef(my_model.fit[[name]], s = "lambda.min") , coef(my_model.fit[[name]], s = "lambda.1se")  ) 
        colnames(my_coeficients[[name]]) <- c("lambda.min","lambda.1se")
      } } } }
###

my_rocs_severity <- my_rocs
my_coef_severity <- my_coeficients
my_score_severity <- score_data
my_models_severity <- my_model.fit

rm(my_rocs,my_coeficients,score_data,my_model.fit,xxi,xxy,xyz,r2,iii,ixx,count,name,tmp,out,perf,pred)
###
### write.csv(file="results_all_data_severity_models.csv",results_severity)
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# load("covid19_glmnet_models_diane.RData")
#
# There was a big problem with how the calculations per model were done for ROCR package. The function accepts a list of numbers but
# the input was actually factors, producing wrong results. Beucase of this and because pROC also has a function to calculate CI I re-calculate 
# all the AUC values and CI intervals for further testing.
#
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

results_survival_cv$scenario[which(results_survival_cv$scenario %in% 'scenario1')] <- "Cytokine"
results_survival_cv$scenario[which(results_survival_cv$scenario %in% 'scenario2')] <- "CT-Qualitative"
results_survival_cv$scenario[which(results_survival_cv$scenario %in% 'scenario3')] <- "CT-Quantitative"
results_survival_cv$scenario[which(results_survival_cv$scenario %in% 'scenario4')] <- "Combined"
results_survival_cv$scenario[which(results_survival_cv$scenario %in% 'scenario6')] <- "Optimized"
results_survival_cv$scenario[which(results_survival_cv$scenario %in% 'CT-Semiquantitative')] <- "CT-Qualitative"

results_severity_cv$scenario[which(results_severity_cv$scenario %in% 'scenario1')] <- "Cytokine"
results_severity_cv$scenario[which(results_severity_cv$scenario %in% 'scenario2')] <- "CT-Qualitative"
results_severity_cv$scenario[which(results_severity_cv$scenario %in% 'scenario3')] <- "CT-Quantitative"
results_severity_cv$scenario[which(results_severity_cv$scenario %in% 'scenario4')] <- "Combined"
results_severity_cv$scenario[which(results_severity_cv$scenario %in% 'scenario6')] <- "Optimized"

summary(results_severity_cv$auc[results_severity_cv$scenario %in% 'Cytokine'])
summary(results_severity_cv$auc[results_severity_cv$scenario %in% 'CT-Qualitative']) 
summary(results_severity_cv$auc[results_severity_cv$scenario %in% 'CT-Quantitative'])
summary(results_severity_cv$auc[results_severity_cv$scenario %in% 'Combined'])
summary(results_severity_cv$auc[results_severity_cv$scenario %in% 'Optimized'])

summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'Cytokine'])
summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'CT-Qualitative']) 
summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'CT-Quantitative'])
summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'Combined'])
summary(results_survival_cv$auc[results_survival_cv$scenario %in% 'Optimized'])


### AUC plot
#pred <- prediction(my_score_survival[[20]]$response, my_score_survival[[20]]$survival )
#perf <- performance(pred,"tpr","fpr")
#plot(perf, colorize=FALSE, col="black") # Plot ROC curve
#lines(c(0,1),c(0,1),col = "gray", lty = 4 )
#perf@x.values
#perf@y.values
rm(acc,bestAccInd,bestMsg,cutoff,icol,ind,list_of_names,repeats,subset_prob,temp,perf,pred,acc.perf)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### CI 4 delong + CORRECTIONS IN CALCULATIONS
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(pROC)

#ci.auc(as.numeric(as.character(my_score_survival[[12]]$survival)),
#       as.numeric(as.character(my_score_survival[[12]]$class)), 
#       conf.level = 0.95,
#       method="d") 

### deLong.test(x, labels, labpos, ref=NULL, conf.level=0.95)

#pred <- prediction( as.numeric(as.character(my_score_survival[[12]]$class)),as.numeric(as.character(my_score_survival[[12]]$survival)))
#TPR/FPR Predictions
#perf <- performance(pred,"tpr","fpr")
#performance(pred, measure = "auc")@y.values[[1]]
### Mann–Whitney U-statistic.
#results_survival_cv[1:12,]

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### survival
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cis_df <- list()
my_procs_survival <- list()

for( i in 1:length(my_score_survival)) {

  if ( length(levels(as.factor(as.character(my_score_survival[[i]]$class)))) > 1 ) {

    rr <- pROC::roc(predictor=as.numeric(as.character(my_score_survival[[i]]$class)), 
                    response=as.numeric(as.character(my_score_survival[[i]]$survival)), ci=TRUE)
    
    name <- names(my_score_survival)[i]
    my_procs_survival[name] <- rr
    
    v <- var(rr) ; 
    b <- rr$auc - .5 ; se <- sqrt(v)

    cis_df[[i]] <- data.frame( proc_AUC = as.numeric(rr$auc),
                               proc_CI_min = rr$ci[1],
                               proc_CI_max = rr$ci[3],
                               proc_p.value = se,
                               i=i)

  } }

cis_df <- do.call(rbind,cis_df)
# dim(cis_df)
# head(cis_df)
###
results_survival_cv$i <- 1:nrow(results_survival_cv)
###
adjusted_results_survival_cv <- results_survival_cv[which(results_survival_cv$i %in% cis_df$i),]
###
adjusted_results_survival_cv <- merge(adjusted_results_survival_cv,cis_df,by="i")
###

###
#ixx <- which(adjusted_results_survival_cv$scenario %in% c('Cytokine',"CT-Qualitative","CT-Quantitative","Combined","Optimized"))
#adjusted_results_survival_cv <- adjusted_results_survival_cv[ixx,]
###

### to fix before final version
#for_figure <- adjusted_results_survival_cv[adjusted_results_survival_cv$scenario %in% "CT-Qualitative" & adjusted_results_survival_cv$prob %in% 0.8,]
#for_figure$prob <- 0.9
#adjusted_results_survival_cv <- rbind(adjusted_results_survival_cv,for_figure)
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### severity
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cis_df <- list()
my_procs_severity <- list()

for( i in 1:length(my_score_severity)) {
  
  if ( length(levels(as.factor(as.character(my_score_severity[[i]]$class)))) > 1 ) {
    
    rr <- pROC::roc(predictor=as.numeric(as.character(my_score_severity[[i]]$severity)), 
                    response=as.numeric(as.character(my_score_severity[[i]]$class)), ci=TRUE)
    
    name <- names(my_score_severity)[i]
    my_procs_severity[name] <- rr
    
    v <- var(rr) ; 
    b <- rr$auc - .5 ; se <- sqrt(v)
    
    cis_df[[i]] <- data.frame( proc_AUC = as.numeric(rr$auc),
                               proc_CI_min = rr$ci[1],
                               proc_CI_max = rr$ci[3],
                               proc_p.value = se,
                               i=i)
###    
  } }
###
cis_df <- do.call(rbind,cis_df)
###
results_severity_cv$i <- 1:nrow(results_severity_cv)
###
adjusted_results_severity_cv <- results_severity_cv[which(results_severity_cv$i %in% cis_df$i),]
###
adjusted_results_severity_cv <- merge(adjusted_results_severity_cv,cis_df,by="i")
###
ixx <- which(adjusted_results_severity_cv$scenario %in% c('Cytokine',"CT-Qualitative","CT-Quantitative","Combined","Optimized"))
adjusted_results_severity_cv <- adjusted_results_severity_cv[ixx,]
adjusted_results_severity_cv$scenario <- factor(adjusted_results_severity_cv$scenario, levels = c('Cytokine',"CT-Qualitative","CT-Quantitative","Combined","Optimized") )
###
ixx <- which(adjusted_results_survival_cv$scenario %in% c('Cytokine',"CT-Qualitative","CT-Quantitative","Combined","Optimized"))
adjusted_results_survival_cv <- adjusted_results_survival_cv[ixx,]
adjusted_results_survival_cv$scenario <- factor(adjusted_results_survival_cv$scenario, levels = c('Cytokine',"CT-Qualitative","CT-Quantitative","Combined","Optimized") )
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#save.image("covid19_glmnet_models_diane_final.RData")
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_survival.pdf",width = 8, height = 4)
### ixx<-adjusted_results_survival_cv$proc_p.value<0.05
ggplot(data=adjusted_results_survival_cv) + aes(x=scenario,y=auc,fill=scenario) + 
  facet_wrap(~prob,ncol=5) + 
  geom_boxplot(color="black") +
  #geom_violin(color="black") + 
  theme_linedraw() +
  rotate_x_text(angle=45) +
  labs(x ='', y='AUC', title='Survival Status')
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_severity.pdf",width = 8, height = 4)
### ixx <- which(results_severity_cv$scenario %in% c('Cytokine',"CT-Semiquantitative","CT-Quantitative","Combined","Optimized"))
ggplot(data=adjusted_results_severity_cv) + aes(x=scenario,y=auc,fill=scenario) + 
  facet_wrap(~prob,ncol=5) + 
  geom_boxplot(color="black") + 
  theme_linedraw() +
  rotate_x_text(angle=45) +
  labs(x ='', y='AUC', title='Severity Status')
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_severity_vio.pdf",width = 18, height = 7)
### ixx <- which(results_severity_cv$scenario %in% c('Cytokine',"CT-Semiquantitative","CT-Quantitative","Combined","Optimized"))
ggplot(data=adjusted_results_severity_cv) + aes(x=scenario,y=auc,fill=scenario) + 
  facet_wrap(~prob,ncol=5) + 
  geom_violin(color="black",alpha=0.5) + 
  theme_linedraw() +
  geom_quasirandom(size=0.1) +
  geom_boxplot(width=0.1,color="black",fill="white") +
  rotate_x_text(angle=45) + theme(legend.position = "none") + 
  labs(x ='', y='AUC', title='Severity Status') + theme(text=element_text(size=21))
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_survival_vio.pdf",width = 18, height = 7)
### ixx<-adjusted_results_survival_cv$proc_p.value<0.05
ggplot(data=adjusted_results_survival_cv[,]) + aes(x=scenario,y=auc,fill=scenario) + 
  facet_wrap(~prob,ncol=5) + 
  geom_violin(color="black",alpha=0.5) + 
  theme_linedraw() +
  geom_quasirandom(size=0.1) +
  geom_boxplot(width=0.1,color="black",fill="white") +
  rotate_x_text(angle=45) + theme(legend.position = "none") + 
  labs(x ='', y='AUC', title='Survival Status') + theme(text=element_text(size=21))
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_alpha_effect_survival.pdf",width = 8, height = 4)
ggplot(data=adjusted_results_survival_cv) + aes(y=as.factor(alpha),x=auc) + geom_boxplot() +
  theme_linedraw() + facet_wrap(~scenario,ncol=5) + labs(x ='AUC', y='Alpha', title='Survival Status')
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_alpha_effect_severity.pdf",width = 8, height = 4)
ggplot(data=adjusted_results_severity_cv) + aes(y=as.factor(alpha),x=auc) + geom_boxplot() +
  theme_linedraw() + facet_wrap(~scenario,ncol=5) + labs(x ='AUC', y='Alpha', title='Severity Status')
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++ with wilcox
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my_comparisons <- list(
  c('Cytokine',"CT-Qualitative"),
  c('Cytokine',"CT-Quantitative"),
  c('Cytokine',"Combined"),
  c('Cytokine',"Optimized"),
  
  c('CT-Qualitative',"CT-Quantitative"),
  c('CT-Qualitative',"Combined"),
  c('CT-Qualitative',"Optimized"),
  
  c('CT-Quantitative',"Combined"),
  c('CT-Quantitative',"Optimized"),
  
  c('Combined',"Optimized")
)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ggplot(adjusted_results_survival_cv) + aes(x=scenario,y=auc) + geom_boxplot() #+ facet_wrap(~prob)
#ggplot(adjusted_results_survival_cv) + aes(x=scenario,y=proc_AUC) + geom_boxplot() #+ facet_wrap(~prob)
#ggplot(adjusted_results_survival_cv) + aes(x=auc,y=proc_AUC) + geom_point() #+ facet_wrap(~prob)

pdf(file="auc_prediction_cv_results_survival_w_wilcox.pdf",width = 8, height = 6)
ixx <- which(!adjusted_results_survival_cv$auc <= 0.5)
ggboxplot(data=adjusted_results_survival_cv[ixx,], x="scenario",y="auc",fill="scenario") + 
  facet_wrap(~prob,ncol=5) + 
  geom_boxplot(color="black",aes(fill=scenario)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE) +
  #geom_violin(color="black") + 
  theme_linedraw() +
  rotate_x_text(angle=45) +
  labs(x ='', y='AUC', title='Survival Status')
dev.off()

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_severity_w_wilcox.pdf",width = 8, height = 6)
### ixx <- which(results_severity_cv$scenario %in% c('Cytokine',"CT-Semiquantitative","CT-Quantitative","Combined","Optimized"))
ixx <- which(!adjusted_results_severity_cv$auc <= 0.5)
ggboxplot(data=adjusted_results_severity_cv[ixx,], x="scenario",y="auc",fill="scenario") + 
  facet_wrap(~prob,ncol=5) + 
  geom_boxplot(color="black",aes(fill=scenario)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE) +
  theme_linedraw() +
  rotate_x_text(angle=45) +
  labs(x ='', y='AUC', title='Severity Status')
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

head(adjusted_results_survival_cv,n=25)

ix <- which(adjusted_results_survival_cv$scenario %in% "Cytokine" & adjusted_results_survival_cv$prob %in% "0.3" & adjusted_results_survival_cv$alpha == 0.0 )
ggplot(data=adjusted_results_survival_cv[ix,]) + aes(x=proc_AUC) + geom_density()

ix <- which(adjusted_results_survival_cv$scenario %in% "Cytokine" & adjusted_results_survival_cv$prob %in% "0.3" & adjusted_results_survival_cv$alpha == 0.1 )
ggplot(data=adjusted_results_survival_cv[ix,]) + aes(x=proc_AUC) + geom_density()

ix <- which(adjusted_results_survival_cv$scenario %in% "Cytokine" & adjusted_results_survival_cv$prob %in% "0.3" & adjusted_results_survival_cv$alpha == 0.6 )
ggplot(data=adjusted_results_survival_cv[ix,]) + aes(x=proc_AUC) + geom_density()

ix <- which(adjusted_results_survival_cv$scenario %in% "Cytokine" & adjusted_results_survival_cv$prob %in% "0.3" & adjusted_results_survival_cv$alpha == 0.8 )
ggplot(data=adjusted_results_survival_cv[ix,]) + aes(x=proc_AUC) + geom_density()

ix <- which(adjusted_results_survival_cv$scenario %in% "Cytokine" & adjusted_results_survival_cv$prob %in% "0.3" & adjusted_results_survival_cv$alpha == 1.0 )
ggplot(data=adjusted_results_survival_cv[ix,]) + aes(x=proc_AUC) + geom_density()

head(adjusted_results_survival_cv)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### de long test
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dim(adjusted_results_survival_cv)
head(adjusted_results_survival_cv)

test <- adjusted_results_survival_cv[which(!is.na(adjusted_results_survival_cv$proc_CI_min)), ]
test <- test[order(test$proc_AUC,decreasing = TRUE),]

# CT-Qualitative, CT-Quantitative, Combined, Optimized 
mean(test$proc_AUC[ test$scenario %in% "Cytokine" ])
mean(test$proc_AUC[ test$scenario %in% "CT-Qualitative" ])
mean(test$proc_AUC[ test$scenario %in% "CT-Quantitative" ])
mean(test$proc_AUC[ test$scenario %in% "Combined" ])
mean(test$proc_AUC[ test$scenario %in% "Optimized" ])

which(test$prob == 0.9 & test$scenario %in% "Optimized" & test$proc_AUC > mean(test$proc_AUC[ test$scenario %in% "Optimized" ]) - 0.02 & test$proc_AUC < mean(test$proc_AUC[ test$scenario %in% "Optimized" ]) + 0.02 )[1]
which(test$prob == 0.9 & test$scenario %in% "Cytokine" & test$proc_AUC > mean(test$proc_AUC[ test$scenario %in% "Cytokine" ]) - 0.02 & test$proc_AUC < mean(test$proc_AUC[ test$scenario %in% "Cytokine" ]) + 0.02 )[1]

test$i[2878]
test$i[4436]

### opt vs cyto
rr1 <- pROC::roc(predictor=as.numeric(as.character(my_score_survival[[56]]$survival)), 
                 response=as.numeric(as.character(my_score_survival[[56]]$class)), ci=TRUE)
###
rr2 <- pROC::roc(predictor=as.numeric(as.character(my_score_survival[[4885]]$survival)), 
                 response=as.numeric(as.character(my_score_survival[[4885]]$class)), ci=TRUE)
###
roc.test(rr1,rr2,method='delong')
###
roc.test(rr1,rr2,method='bootstrap')
###

#write.csv(file="results_all_data_survival_models.csv",adjusted_results_survival_cv)
#write.csv(file="results_all_data_severity_models.csv",adjusted_results_severity_cv)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Hazard ratio models for survival
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(survminer)
library(survival)

data_legacy <- as.data.frame(read_excel("diane_analysis/final_data_GC_DD_V5.xlsx", sheet = "DATA"))

cor.test(data_legacy$Age,data$Age)

data_all$Deceased_before_discharge <-  data_all$`Deceased before discharge`
### as dates
data_all$LAST_FOLLOWUP_as.Date <- as.Date(data_all$`Last follow-up`)
### as dates
data_all$Date_of_admission_ED_as.Date <- as.Date(data_all$`Date of hospital admission`)
### difference: default=days
data_all$time_to_last_follow_up <- as.numeric(difftime(data_all$LAST_FOLLOWUP_as.Date,data_all$Date_of_admission_ED_as.Date)) #units='weeks'
### needs to be numeric
data_all$`Deceased before discharge` <- as.numeric(as.character(data_all$`Deceased before discharge`))
### CENSOR
data_all$censored_dead <- Surv(data_all$time_to_last_follow_up,data_all$`Deceased before discharge`)


### simple coxph model
res.cox <- coxph(Surv(time_to_last_follow_up,`Deceased before discharge`) ~ `Sex factor`, data =  data_all)
summary(res.cox)

data_all$Sex_factor <- data_all$`Sex factor`
### surv fit test
res.cox <- survfit( Surv(data_all$time_to_last_follow_up, data_all$Deceased_before_discharge) ~ Sex_factor, data =  data_all )
#ggsurvplot(res.cox, data = data)

### custom plot
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            legend.labs =  c("Male", "Female"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data_all ) + labs(title = "Gender")

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### univariate analysis
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Scenarios for testing
#scenario1 = c("IL6_group","TNF_ALPHA_group") ### Cytokines
#scenario2 = c("Total_severity_score_Scale_0_20","Age") ### CT qualitative
#scenario3 = c("GGO_ml","InterstitiumBlood_ml","Totalvolume_ml") ### CT quantitative
#scenario6 = c("GGO_AeratedLung_Ratio","Age","TNF_ALPHA_group","IL6_group","IL8_group") ### Best model
data_all$IL6_group <- data$IL6_group
data_all$IL8_group <- data$IL8_group
data_all$TNF_ALPHA_group <- data$TNF_ALPHA_group
data_all$CT_qualitative <- data$`CT qualitative score`
data_all$GGO.aerated.lung <- data$`GGO/Aerated lung`

data_all$o2.saturation <- data_all$`O2 saturation`
data_all$who.score <- data_all$`Maximun severity degree (WHO scale)`

### variables to test
covariates <- c("IL6_group","IL8_group","TNF_ALPHA_group","Age","Sex_factor","BMI","CT_qualitative","GGO.aerated.lung","o2.saturation","who.score")
### formulas
univ_formulas <- sapply(covariates, function(x) as.formula(paste('Surv(time_to_last_follow_up, Deceased_before_discharge)~', x)))
### models
univ_models <- lapply( univ_formulas, function(x){coxph(x, data = data_all)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2); #coeficient beta
                         HR <-signif(x$coef[2], digits=2); #exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
### join & dataframe
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
univariate_results_hr <- res

write.csv(file="univariate.coxph.models.csv",univariate_results_hr)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### multivariate
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### "IL6_group","IL8_group","TNF_ALPHA_group","Age","Sex_factor","BMI","CT_qualitative","GGO.aerated.lung"
res.cox <- coxph(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ IL6_group + IL8_group + TNF_ALPHA_group + Age + Sex_factor + BMI + CT_qualitative + GGO.aerated.lung + o2.saturation + who.score , data = data_all)
summary(res.cox)

library(tab)
write.csv(file="multivariate.coxph.models.csv",tabcoxph(res.cox))

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### figures
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### IL6
res.cox <- survfit(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ IL6_group, data = data)
### custom plot
pdf(file="coxph_fit_il6.pdf",width = 6,height =  5)
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            #legend.labs =  c("0", "1"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data ) + labs(title = "IL6")
dev.off()

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### IL8
res.cox <- survfit(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ IL8_group, data = data)
### custom plot
pdf(file="coxph_fit_il8.pdf",width = 6,height =  5)
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            #legend.labs =  c("0", "1"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data ) + labs(title = "IL8")
dev.off()

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### TNF alpha
res.cox <- survfit(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ TNF_ALPHA_group, data = data)
### custom plot
pdf(file="coxph_fit_tnfa.pdf",width = 6,height =  5)
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            #legend.labs =  c("0", "1"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data ) + labs(title = "TNF alpha")
dev.off()

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### GGO_AeratedLung_Ratio
### quantiles
data$GGO_AeratedLung_Ratio_quantiles <- as.character(.bincode(data$GGO_AeratedLung_Ratio, breaks=quantile(data$GGO_AeratedLung_Ratio, probs=seq(0,1, by=0.25), na.rm=TRUE), include.lowest=TRUE))
### model
res.cox <- survfit(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ GGO_AeratedLung_Ratio_quantiles, data = data)
### custom plot
pdf(file="coxph_fit_ratio.pdf",width = 8,height =  7)
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            #legend.labs =  c("0", "1"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data ) + labs(title = "GGO AeratedLung Ratio")
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Severity
### quantiles
data$Total_severity_score_Scale_0_20_quantiles <- as.character(.bincode(data$Total_severity_score_Scale_0_20, breaks=quantile(data$Total_severity_score_Scale_0_20, probs=seq(0,1, by=0.25), na.rm=TRUE), include.lowest=TRUE))
### cox model
res.cox <- survfit(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ Total_severity_score_Scale_0_20_quantiles, data = data)
### custom plot
pdf(file="coxph_fit_severity.pdf",width = 8,height =  7)
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            #legend.labs =  c("0", "1"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data ) + labs(title = "Total Severity Score Scale (0:20)")
dev.off()

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Age
### quantiles
data$age_quantiles <- as.character(.bincode(data$Age, breaks=quantile(data$Age, probs=seq(0,1, by=0.25), na.rm=TRUE), include.lowest=TRUE))
### model
res.cox <- survfit(Surv(time_to_last_follow_up, Deceased_before_discharge) ~ age_quantiles, data = data)
### custom plot
pdf(file="coxph_fit_age.pdf",width = 8,height =  7)
ggsurvplot( res.cox, 
            size = 1, 
            conf.int = TRUE, pval = TRUE,
            risk.table = TRUE, 
            risk.table.col = "strata",
            #legend.labs =  c("0", "1"), 
            risk.table.height = 0.25, 
            ggtheme = theme_bw(), data = data ) + labs(title = "Age")
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary(adjusted_results_survival_cv$proc_AUC[adjusted_results_survival_cv$scenario %in% "Optimized"])
summary(adjusted_results_survival_cv$proc_AUC[adjusted_results_survival_cv$scenario %in% "Combined"])

ix <- which(adjusted_results_survival_cv$prob %in% "0.3")
figure_data_adjusted_results_survival_cv <- adjusted_results_survival_cv[ix,]

ix <- which(figure_data_adjusted_results_survival_cv$proc_AUC >= 0.5)
figure_data_adjusted_results_survival_cv <- figure_data_adjusted_results_survival_cv[ix,]

dim(figure_data_adjusted_results_survival_cv)
table(figure_data_adjusted_results_survival_cv$scenario)

my_comparisons <- list( c('Cytokine',"CT-Qualitative"), c('Cytokine',"CT-Quantitative"), c('Cytokine',"Combined"), c('Cytokine',"Optimized"),
                        c('CT-Qualitative',"CT-Quantitative"), c('CT-Qualitative',"Combined"), c('CT-Qualitative',"Optimized"),
                        c('CT-Quantitative',"Combined"), c('CT-Quantitative',"Optimized"), c('Combined',"Optimized"))

ggboxplot(data=figure_data_adjusted_results_survival_cv, x="scenario",y="proc_AUC",fill="scenario") + 
  facet_wrap(~prob,ncol=5) + 
  geom_boxplot(color="black", aes(fill=scenario)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE) +
  theme_linedraw() +
  #rotate_x_text(angle=45) + 
  coord_flip() +
  labs(x ='', y='AUC', title='Severity Status') + theme(text=element_text(size=21))

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(rms)
# nomogram()
#"GGO/Aerated lung","Age","TNF_ALPHA_group","IL6_group","IL8_group"

data$GGO.aerated.lung <- data$`GGO/Aerated lung`
data$y <- data$`Deceased before discharge`

ddist <- datadist(data)
options(datadist='ddist')

mod.bi <- lrm(y ~ GGO.aerated.lung + Age + TNF_ALPHA_group + IL6_group + IL8_group ,data)

nom.bi <- nomogram(mod.bi,
                   lp.at=seq(-3,4,by=0.5),
                   fun=function(x)1/(1+exp(-x)),
                   fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                   funlabel="Risk of Death",
                   conf.int=c(0.1,0.5),
                   abbrev=TRUE,
                   minlength=1,lp=F)

pdf(file="risk.nonogram.pdf",width = 10,height = 10)
plot(nom.bi,lplabel="Linear Predictor",
     #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
     col.conf=c('red','green'),
     conf.space=c(0.1,0.5),
     label.every=1,
     col.grid = gray(c(0.8, 0.95)))
dev.off()

#legend.nomabbrev(nom.bi, which='shock', x=.5, y=.5)
dev.off()


### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# confusionMatrix(predict(model, newdata=test_data), test_data$labels)
plot(my_rocs_survival$scenario1_0_repeat_1_p_0.1)


tmp_survival <- adjusted_results_survival_cv
tmp_survival$auc <- round(tmp_survival$auc,2)

ix <- which(!tmp_survival$auc<0.5)
tmp_survival <- tmp_survival[ix,]


my_scene <- levels(as.factor(tmp_survival$scenario))
results_for_roc <- list()
count=1

for ( iii in 1:length(my_scene)) {

vmy_value <- round(mean(tmp_survival$auc[which(tmp_survival$scenario %in% my_scene[iii] )]),2)
ix <- which(tmp_survival$scenario %in% my_scene[iii] & tmp_survival$auc == vmy_value)
ix <- tmp_survival$i[ix]

  for (xii in ix) {
  results_for_roc[[count]] <- data.frame( FPR = unlist(my_rocs_survival[[xii]]@x.values) ,
                                        TPR = unlist(my_rocs_survival[[xii]]@y.values) ,
                                        model = xii ,
                                        scenario =  my_scene[iii] )
  count=count+1
} }

results_for_roc <- do.call(rbind,results_for_roc)

head(results_for_roc)

gg_color_hue <- function(n) { hues = seq(15, 375, length = n + 1) ; hcl(h = hues, l = 65, c = 100)[1:n] }

gg_color_hue(5)[1]

ix1 <- which(results_for_roc$scenario %in% my_scene[1]) 
ix2 <- which(results_for_roc$scenario %in% my_scene[2])
ix3 <- which(results_for_roc$scenario %in% my_scene[3])
ix4 <- which(results_for_roc$scenario %in% my_scene[4])
ix5 <- which(results_for_roc$scenario %in% my_scene[5])


pdf(file="average_ROC_curves_w_error_bars_labeled.pdf",width = 8,height = 6)
ggplot(results_for_roc[,]) + aes(x=FPR,y=TPR) + 
  
  geom_line(data=results_for_roc[ix1,] ,color=gg_color_hue(5)[1],alpha=0.07) + 
  stat_smooth(data=results_for_roc[ix1,], method = 'loess', color = gg_color_hue(5)[1],fullrange = TRUE,alpha=0) +
  
  geom_line(data=results_for_roc[ix2,] ,color=gg_color_hue(5)[2],alpha=0.07) + 
  stat_smooth(data=results_for_roc[ix2,], method = 'loess', color = gg_color_hue(5)[2],fullrange = TRUE,alpha=0) +
  
  geom_line(data=results_for_roc[ix3,] ,color=gg_color_hue(5)[3],alpha=0.07) + 
  stat_smooth(data=results_for_roc[ix3,], method = 'loess', color = gg_color_hue(5)[3],fullrange = TRUE,alpha=0) +
  
  geom_line(data=results_for_roc[ix4,] ,color=gg_color_hue(5)[4],alpha=0.07) + 
  stat_smooth(data=results_for_roc[ix4,], method = 'loess', color = gg_color_hue(5)[4],fullrange = TRUE,alpha=0) +
  
  geom_line(data=results_for_roc[ix5,] ,color=gg_color_hue(5)[5],alpha=0.07) + 
  stat_smooth(data=results_for_roc[ix5,], method = 'loess', color = gg_color_hue(5)[5],fullrange = TRUE,alpha=0) +
  
  geom_label( label="Cytokines",  x=0.75, y=0.1, label.padding = unit(0.45, "lines"), label.size = 0.30, color = "black", fill="#F8766D") +
  geom_label( label="CT-Qualitative",  x=0.75, y=0.2, label.padding = unit(0.45, "lines"), label.size = 0.30, color = "black", fill="#A3A500") +
  geom_label( label="CT-Quantitative",  x=0.75, y=0.3, label.padding = unit(0.45, "lines"), label.size = 0.30, color = "black", fill="#00BF7D") +
  geom_label( label="Combined",  x=0.75, y=0.4, label.padding = unit(0.45, "lines"), label.size = 0.30, color = "black", fill="#00B0F6") +
  geom_label( label="Optimized",  x=0.75, y=0.5, label.padding = unit(0.45, "lines"), label.size = 0.30, color = "black", fill="#E76BF3") +
  
  theme_classic2() 
dev.off()
  
#annotate(geom="text", x=0.75, y=0.1, label="Cytokines", color="#F8766D",fill="#F8766D",size=4) +
#annotate(geom="text", x=0.75, y=0.2, label="CT-Qualitative", colour="#F8766D",size=4) +
#annotate(geom="text", x=0.75, y=0.3, label="CT-Quantitative", colour=gg_color_hue(5)[1],size=4) +
#annotate(geom="text", x=0.75, y=0.4, label="Combined",colour =gg_color_hue(5)[1],size=4) +
#annotate(geom="text", x=0.75, y=0.5, label="Optimized",colour= gg_color_hue(5)[1],size=4) +

#new_fpr <- pivot_wider(data=results_for_roc[ix,], names_from = model, values_from = FPR) %>% unnest()
#new_fpr <- as.data.frame(new_fpr)
#ix <- which(results_for_roc$scenario %in% my_scene[2])
#ggplot(results_for_roc[ix,]) + aes(x=FPR,y=TPR) + geom_line(aes(color=as.factor(model)),show.legend = FALSE) 
#head(adjusted_results_survival_cv)

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
my_comparisons <- list(  c('Cytokine',"CT-Qualitative"), c('Cytokine',"CT-Quantitative"), c('Cytokine',"Combined"), c('Cytokine',"Optimized"),
                         c('CT-Qualitative',"CT-Quantitative"), c('CT-Qualitative',"Combined"), c('CT-Qualitative',"Optimized"), c('CT-Quantitative',"Combined"),
                         c('CT-Quantitative',"Optimized"), c('Combined',"Optimized") )
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_survival_w_wilcox_MAIN.pdf",width = 16, height = 5)
ixx <- which(!adjusted_results_survival_cv$auc <= 0.5)
ggboxplot(data=adjusted_results_survival_cv[ixx,], x="scenario",y="auc",fill="scenario") + 
  geom_boxplot(color="black",aes(fill=scenario)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE) +
  theme_linedraw() +
  #rotate_x_text(angle=45) + 
  coord_flip() +
  labs(x ='', y='AUC', title='Survival Status', fill="Scenario") + theme(text = element_text(size=rel(4.8)),
                                                        legend.text=element_text(size=rel(4.4)),
                                                        plot.title=element_text(size=rel(4.8)) )  + theme(legend.position="bottom")
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf(file="auc_prediction_cv_results_severity_w_wilcox_MAIN.pdf",width = 16, height = 5)
ixx <- which(!adjusted_results_severity_cv$auc <= 0.5)
ggboxplot(data=adjusted_results_severity_cv[ixx,], x="scenario",y="auc",fill="scenario") + 
  geom_boxplot(color="black",aes(fill=scenario)) +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", paired=FALSE, label="p.signif", hide.ns = FALSE) +
  theme_linedraw() +
  #rotate_x_text(angle=45) + 
  coord_flip() +
  labs(x ='', y='AUC', title='Severity Status', fill="Scenario") + theme(text = element_text(size=rel(4.8)),
                                                        legend.text=element_text(size=rel(4.4)),
                                                        plot.title=element_text(size=rel(4.8)) ) +theme(legend.position="bottom") 
dev.off()
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("covid19_glmnet_models_diane_final.RData")
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ix <- which( colnames(data_numeric) %in% c("Severity degree at admission (MSH classification)",
                              "Deceased before discharge",
                              "Deceased after complete follow-up","Rehospitalization","ICU admission during hospital stay") )

data_numeric <- data_numeric[,-ix]

mydata <- prcomp( data_numeric , scale=TRUE ) 

Scree.plot <- fviz_eig(mydata, addlabels = T, barfill = "lightsteelblue3", barcolor = "lightsteelblue3") + theme_bw() + ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=rel(4.8)))
eigen <- get_eigenvalue(mydata)
pca1 <- format(round(eigen$variance.percent[1], 2), nsmall = 2)
pca2 <- format(round(eigen$variance.percent[2], 2), nsmall = 2)
Scree.plot

pca_mts<-as.data.frame(mydata$x)
pca_mts$who <- data$`Maximun severity degree (WHO scale)`
pca_mts$RIP <- data$`Deceased before discharge`
pca_mts$ids <- rownames(data_numeric)

ggarrange(
ggplot(pca_mts, aes(PC1, PC2,color=who )) + 
  geom_point() + 
  theme_bw() + labs(x=paste("PCA1:",pca1,"%",sep=""),y=paste("PCA2:",pca2,"%",sep=""),color="WHO\nScore")
,
ggplot(pca_mts, aes(PC1, PC2,color=as.character(RIP) )) + 
  geom_point() + 
  theme_bw() + labs(x=paste("PCA1:",pca1,"%",sep=""),y=paste("PCA2:",pca2,"%",sep=""),color="Deceased")
, ncol=1,nrow=2
)








### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### save.image("covid19_glmnet_models_diane.RData")
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### The end
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++