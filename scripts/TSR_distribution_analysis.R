## Plotting the distribution of tags per TSR and STRIPE-seq

#set working directory
setwd("/home/rraborn/scratch/DaphPopsTSRs")

#loading libraries
library(ggplot2)
library(reshape2)
library(dplyr)

#Let's import the datasets:

POV84_3thresh <- read.table(file="TSRchitect_TSSthresh3_files/POV84_TSRset.txt",header=TRUE)
TEX36_3thresh <- read.table(file="TSRchitect_TSSthresh3_files/TEX36_TSRset.txt",header=TRUE)
POV12_3thresh <- read.table(file="TSRchitect_TSSthresh3_files/POV12_TSRset.txt",header=TRUE)
LPB_3thresh <- read.table(file="TSRchitect_TSSthresh3_files/LPB_TSRset.txt", header=TRUE)
LPB_2thresh <- read.table(file="TSRchitect_TSSthresh2_files/LPB_TSRset.txt", header=TRUE)
NFL_3thresh <- read.table(file="TSRchitect_TSSthresh3_files/NFL_TSRset.txt", header=TRUE)
BRV_2thresh <- read.table(file="TSRchitect_TSSthresh2_files/BRV_TSRset.txt", header=TRUE)

dim(POV84_3thresh)
dim(TEX36_3thresh)
dim(POV12_3thresh)
dim(LPB_3thresh)
dim(LPB_2thresh)
dim(NFL_3thresh)
dim(BRV_2thresh)

POV84_3thresh$Pop <- "POV84"
TEX36_3thresh$Pop <- "TEX36"
POV12_3thresh$Pop <- "POV12"
LPB_3thresh$Pop <- "LPB"
LPB_2thresh$Pop <- "LPB-2"
NFL_3thresh$Pop <- "NFL"
BRV_2thresh$Pop <- "BRV"

#Combining the three datasets:
combined_table <- rbind(POV12_3thresh, POV84_3thresh, TEX36_3thresh, LPB_3thresh, LPB_2thresh, NFL_3thresh, BRV_2thresh)

#Plotting the reverse cumulative distribution of the TSRs
ggplot(combined_table, aes(as.numeric(nTAGs), colour=Pop)) + geom_step(aes(y = 1 - ..y..), stat='ecdf') + scale_x_log10() + scale_y_log10() + labs(y="Fraction of Total TSRs", x = "number of tags/TSR (nTAGs)")
png(file="Dpulex_TSRs_nTAGs_RevCumulPlot.png")

quantile.pov84 <- quantile(as.numeric(POV84_3thresh$nTAGs), probs = seq(0,1,0.1))
quantile.pov12 <- quantile(as.numeric(POV12_3thresh$nTAGs), probs=seq(0,1,0.1))
quantile.tex36 <- quantile(as.numeric(TEX36_3thresh$nTAGs), probs=seq(0,1,0.1))
quantile.lpb_2 <- quantile(as.numeric(LPB_2thresh$nTAGs), probs=seq(0,1,0.1))

#let's explore the quantile results for each
quantile.pov84
#the 90% quantile begins at 30 tags
quantile.pov12
#the 90% quantile begins at 57 tags 
quantile.tex36
#the 90% quantile begins at 52 tags
quantile.lpb_2
#the 90% quantile begins at 10 tags

POV84_top10pc <- POV84_3thresh[POV84_3thresh$nTAGs>=30,]
POV12_top10pc <- POV12_3thresh[POV84_3thresh$nTAGs>=57,]
TEX36_top10pc <- TEX36_3thresh[TEX36_3thresh$nTAGs>=52,]
LPB_2_top10pc <- LPB_2thresh[LPB_2thresh$nTAGs>=10,]

POV84_top_genes <- na.omit(POV84_top10pc$featureID)
POV12_top_genes <- na.omit(POV12_top10pc$featureID)
TEX36_top_genes <- na.omit(TEX36_top10pc$featureID)
LPB_2_top_genes <- na.omit(LPB_2_top10pc$featureID)

first.m <- match(POV84_top_genes, TEX36_top_genes)
common.genes.1 <- TEX36_top_genes[na.omit(first.m)]

TEX36_top_ind <- match(common.genes.1, TEX36_3thresh$featureID)
TEX36_top_genes_df <- TEX36_3thresh[TEX36_top_ind,]

POV84_top_ind <- match(common.genes.1, POV84_3thresh$featureID)
POV84_top_genes_df <- POV84_3thresh[POV84_top_ind,]
POV84_w_geneIDs <- POV84_3thresh %>% filter(!is.na(featureID))
TEX36_w_geneIDs <- TEX36_3thresh %>% filter(!is.na(featureID))

test.compare <- POV84_w_geneIDs %>% full_join(TEX36_w_geneIDs, by="featureID")  #using dplyr 

compare_table <- rbind(POV84_top_genes_df, TEX36_top_genes_df)
joined_table <- POV84_top_genes_df %>% full_join(TEX36_top_genes_df, by="featureID")  #using dplyr 
joined_table %>% group_by(Pop.x, Pop.y) %>% dplyr::summarise(Mean = mean(nTAGs.x, na.rm = T), n=n(), sd = sd(nTAGs.x, na.rm = T), se = sd/sqrt(n))

cor.test(joined_table$nTSSs.x, joined_table$nTSSs.y, method = "pearson", conf.level = 0.95)




