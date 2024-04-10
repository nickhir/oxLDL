library(DESeq2)
library(PCAtools)
library(httpgd)
library(tidyverse)
library(here)
source(here("plot_theme.R"))
source(here("helper_functions.R"))

# this function regresses SVs out of genomic data
cleaningP = function(y, mod, svaobj, P = ncol(mod)) {
    X = cbind(mod, svaobj$sv)
    Hat = solve(t(X) %*% X) %*% t(X)
    beta = (Hat %*% t(y))
    cleany = y - t(as.matrix(X[, -c(1:P)]) %*% beta[-c(1:P), ])
    return(cleany)
}





 solve(t(X) %*% X) %>% dim()
test





dds <- readRDS(here("QC", "results", "dds_libsize_outliers_removed.rds"))
mdata <- colData(dds) %>% data.frame()
mod <- model.matrix(~sampletype, colData(dds))
svaobj = readRDS(here("QC", "results", str_interp("sva_12SVs.rds")))
norm_data <- assay(vst(dds, blind=TRUE))

# regress out SVs
cleanp = cleaningP(norm_data, mod, svaobj)

# only keep top 40%
# Calculate standard deviation for each feature
std_dev <- apply(cleanp, 1, sd)

# Determine the threshold for selecting the top 40% of features
threshold <- quantile(std_dev, 0.6)

# Select the indices of the features with standard deviation above the threshold
selected_indices <- which(std_dev >= threshold)

# Subset the original data to keep only the selected features
selected_data <- cleanp[selected_indices, ]

pca = prcomp(t(cleanp))
pcaVars=signif(((pca$sdev)^2)/(sum((pca$sdev)^2)),3)*100
signed = ifelse(max(pca$x[, 2] > 70), 1, -1) # same sign across plots

plot(pca$x[, 1], pca$x[, 2],
        pch = 19, col = mdata$state,
        main = "Figure 1A - No SVA", cex = 1.1,
        ylim = c(-80, 100), cex.axis = 1.5, cex.lab = 1.5, 
        xlab = paste("PC1:", pcaVars[1], "% of Variance Explained"), ylab = paste("PC2:", pcaVars[2], "% of Variance Explained")
)

