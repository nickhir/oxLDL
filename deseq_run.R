library(DESeq2)
library("BiocParallel")
register(MulticoreParam(20))
library(magrittr)
library(PCAtools)
library(tidyverse)
library(umap)
library(patchwork)
library(httpgd)

library(here)
source(here("plot_theme.R"))
source(here("helper_functions.R"))


dds <- readRDS(here("QC", "results", "dds_final.rds"))
mdata <- colData(dds) %>% data.frame()
svseq <- readRDS(here("QC", "results", "sva_5_final.rds"))
SVs <- svseq$sv
colnames(SVs) <- paste0("SV", 1:5)
mdata <- cbind(colData(dds) %>% data.frame() %>% select(-contains("SV")), SVs)
mdata$line <- factor(mdata$line)
dds <- DESeqDataSetFromMatrix(counts(dds),
    colData = mdata,
    design = ~ 0 + SV1 + SV2 + SV3 + SV4 + SV5 + line + group
)



# runs very long... 
# using 18 cores this literally took ~6h
deseq_res <- DESeq(dds, parallel = TRUE)
saveRDS(deseq_res, here("differential_gene_expression", "results","init_results.rds"))
