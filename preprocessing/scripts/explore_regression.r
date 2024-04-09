# this function regresses SVs out of genomic data
cleaningP = function(y, mod, svaobj, P = ncol(mod)) {
        X = cbind(mod, svaobj$sv)
        Hat = solve(t(X) %*% X) %*% t(X)
        beta = (Hat %*% t(y))
        cleany = y - t(as.matrix(X[, -c(1:P)]) %*% beta[-c(1:P), ])
        return(cleany)
}


mod <- model.matrix(~sampletype, colData(dds))

# sva with default # of SVs
svaobj = readRDS(here("QC", "results", str_interp("sva_12SVs.rds")))
p <- assay(vsd_data)

# regress out SVs
cleanp = cleaningP(p, mod, svaobj)

# only keep top 40%
# Calculate standard deviation for each feature
std_dev <- apply(cleanp, 1, sd)

# Determine the threshold for selecting the top 40% of features
threshold <- quantile(std_dev, 0.6)

# Select the indices of the features with standard deviation above the threshold
selected_indices <- which(std_dev >= threshold)

# Subset the original data to keep only the selected features
selected_data <- cleanp[selected_indices, ]

pca = prcomp(t(selected_data))
pcaVars=signif(((pca$sdev)^2)/(sum((pca$sdev)^2)),3)*100
signed = ifelse(max(pca$x[, 2] > 70), 1, -1) # same sign across plots

plot(pca$x[, 1], pca$x[, 2],
        pch = 19,
        main = "Figure 1A - No SVA", cex = 1.1,
        ylim = c(-80, 100), cex.axis = 1.5, cex.lab = 1.5, 
        xlab = paste("PC1:", pcaVars[1], "% of Variance Explained"), ylab = paste("PC2:", pcaVars[2], "% of Variance Explained")
)

