get_assoc_pval_categorical <- function(x, y) {
    if (n_distinct(x) == 2) {
        p_val <- broom::tidy(wilcox.test(y ~ x))[, "p.value", drop=TRUE]
    } else {
        p_val <- broom::tidy(kruskal.test(y ~ x))[, "p.value", drop=TRUE]
    }
    return(p_val)
}
          

get_assoc_pval_continous <- function(x, y) {
    p_val <- broom::tidy(lm(y ~ x))[2, "p.value", drop=TRUE] # pick second row, the first will be the intercept.
    return(p_val)
}

#' Calculate covariate correlations with components
#'
#' This function calculates the association between specified covariates and components in the input dataframe.
#'
#' @param df Input dataframe containing covariates and components
#' @param covars Character vector specifying the covariates to analyze
#' @param components Character vector specifying the components to analyze (default: c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"))
#' @return A list containing:
#' \item{adjusted}{Matrix of adjusted p-values for the associations between covariates and components}
#' \item{raw}{Data frame of raw p-values for each covariate-component pair}
#' \item{heatmap}{Heatmap visualizing the adjusted p-values}
#' @examples
#' df <- data.frame(covar1 = rnorm(100), covar2 = rnorm(100), PC1 = rnorm(100), PC2 = rnorm(100))
#' covar_correlation(df, c("covar1", "covar2"), c("PC1", "PC2"))
covar_correlation <- function(df, covars, components = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")) {
    require(ComplexHeatmap)
    require(circlize)
    # check if specified covars exist in input dataframe
    if (!all(covars %in% colnames(df))) {
        missing_covars <- covars[!covars %in% colnames(df)]
        stop(str_interp("The indicated covariates do not occur in the dataframe: ${missing_covars}"))
    }

    # check if all components are in input dataframe
    if (!all(components %in% colnames(df))) {
        missing_components <- components[!components %in% colnames(df)]
        stop(str_interp("The indicated components do not occur in the dataframe: ${missing_components}"))
    }

    # go through covars and calculate association with all specified components
    covar_p_vals <- lapply(setNames(covars, covars), function(covar) {
        print(str_interp("Processing covar ${covar}"))
        tmp_cov <- df[, covar]
        if (is.factor(tmp_cov)) {
            print("Assuming variable is a factor")
        } else {
            print("Assuming variable is continuous")
        }
        if (n_distinct(tmp_cov) == 1) {
            stop(str_interp("${covar} only has a single level."))
        }
        # go through the different components
        component_p_vals <- lapply(setNames(components, components), function(component) {
            tmp_component <- df[, component]
            # if variable is continous, use correlation as an estimate of association.
            # if variable is categorical (as indicated by factor), use either a wilcoxon test (if 2 categories) or a kruskal.test
            if (is.factor(tmp_cov)) {
                p <- get_assoc_pval_categorical(x = tmp_cov, y = tmp_component)
            } else {
                p <- get_assoc_pval_continous(x = tmp_cov, y = tmp_component)
            }
            return(p)
        })
    })
    # do global p_value adjustment
    adjusted_pvals <- p.adjust(unlist(covar_p_vals), method = "fdr") # Adjust using FDR method
    adjusted_matrix <- matrix(adjusted_pvals,
        byrow = T,
        nrow = length(covars), ncol = length(components)
    ) %>%
        data.frame()
    rownames(adjusted_matrix) <- covars
    colnames(adjusted_matrix) <- components

    heatmap <- Heatmap(adjusted_matrix %>% as.matrix(),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_names_side = "left",
        column_names_gp = grid::gpar(fontsize = 12, fontface = "bold"),
        row_names_gp = grid::gpar(fontsize = 12, fontface = "bold"),
        border = "black",
        layer_fun = function(j, i, x, y, width, height, fill) {
            # since grid.text can also be vectorized
            grid.text(round(pindex(adjusted_matrix %>% as.matrix(), i, j), 2), x, y,
                gp = gpar(fontsize = 11, col = "black")
            )
        },
        col = colorRamp2(
            c(0, 0.05, 1),
            c("red", "grey", "blue")
        ),
        heatmap_legend_param = list(
            title = "adj. p",
            at = c(0, 0.05, 1)
        )
    )

    return(list(
        adjusted = adjusted_matrix,
        raw = bind_rows(covar_p_vals) %>% data.frame() %>% magrittr::set_rownames(covars),
        heatmap = heatmap
    ))
}




plot_continuous_covar <- function(df, covar, PC, x_pos = "left",
                                  y_pos = "bottom") {
    p <- ggscatter(df, x = PC, y = covar, add = "reg.line") +
        stat_cor(label.x.npc = x_pos, label.y.npc = y_pos) +
        xlab(paste(PC, ":", round(pca_data$variance[PC], 2), "% variance")) +
        theme_Publication()
    return(p)
}

#' Plot categorical covariate with principal components
#'
#' This function generates boxplots to visualize the relationship between a categorical covariate and specified principal components in the input dataframe.
#'
#' @param df Input dataframe containing the covariate and principal components
#' @param covar Character string specifying the categorical covariate to plot
#' @param PC Character vector specifying the principal components to analyze
#' @return A list of ggplot2 objects representing boxplots for each principal component with the categorical covariate
#' @examples
#' df <- data.frame(covar = factor(rep(c("A", "B"), 50)), PC1_explained_var = runif(100), PC2_explained_var = runif(100))
#' plot_categorical_covar(df, "covar", c("PC1", "PC2"))
plot_categorical_covar <- function(df, covar, PC, xlab = waiver(), ylab = waiver(), bonferroni_correction=FALSE) {
    plots <- lapply(PC, function(tmp_pcs) {
        # get the explained variation of the PC. for that we have to get the correct column
        if (n_distinct(df[, covar]) == 2) {
            p <- ggplot(df, aes_string(x = covar, y = tmp_pcs, fill = covar)) +
                geom_boxplot(width = 0.6) +
                ggsignif::geom_signif(comparisons = list(levels(df[, covar]))) +
                theme_Publication() %+%
                theme(legend.position = "none") +
                ylab(ylab) +
                xlab(xlab) +
                ylim(min(df[, tmp_pcs]) - diff(range(df[, tmp_pcs])) * 0.15, max(df[, tmp_pcs]) + diff(range(df[, tmp_pcs])) * 0.1)
        } else {
            # calc p value with the kruskal test
            p.val <- broom::tidy(kruskal.test(get(tmp_pcs) ~ get(covar), data = df))$p.value
            if(bonferroni_correction){
                p.val = round(p.val * length(PC),4)
            } else {
                p.val = round(p.val, 4)
            }
            p <- ggplot() +
                geom_boxplot(data = df, aes_string(x = covar, y = tmp_pcs, fill = covar), width = 0.6) +
                geom_label(aes(
                    x = median(1:n_distinct(df[, covar])),
                    y = max(df[, tmp_pcs]) + diff(range(df[, tmp_pcs])) * 0.1,
                    label = paste0("p value = ", p.val)
                )) +
                theme_Publication() %+%
                theme(legend.position = "none") +
                ylab(ylab) +
                xlab(xlab) 
        }
        return(p)
    })
    return(plots)
}




#' Density PCA Plot Function
#'
#' This function creates a PCA plot with density plots on the top and right side.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x_variable The name of the variable to be plotted on the x-axis.
#' @param y_variable The name of the variable to be plotted on the y-axis.
#' @param color_by The name of the variable by which the points should be colored.
#' @param xlab The label for the x-axis. Defaults to a waiver.
#' @param ylab The label for the y-axis. Defaults to a waiver.
#' @param shape_by The name of the variable by which the points should be shaped. Defaults to NA.
#' @param colors A vector of colors to be used in the plot. If NA, distinct colors will be generated. Defaults to NA.
#' @param linewidth The width of the lines in the density plots. Defaults to 0.8.
#' @param pt_size The size of the points in the PCA plot. Defaults to 3.
#' @param show_density Logical indicating whether or not to show the density plots. Defaults to TRUE.
#' @param density_plot_ratio The ratio of the size of the density plots to the size of the PCA plot. Defaults to 0.3.
#' @param alpha The transparency of the density plots. Defaults to 0.3.
#'
#' @return A ggplot object containing the PCA plot and density plots.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = rnorm(100), y = rnorm(100), condition = sample(c("A","B"), 100, replace=T))
#' density_pca(data, "x", "y", "condition")
#' }
density_pca <- function(
    df, x_variable, y_variable, color_by, xlab = waiver(), ylab = waiver(), shape_by = NA, colors = NA,
    linewidth = 0.8, pt_size = 3, show_density = TRUE, density_plot_ratio = 0.3, alpha = 0.3, 
    add_ellipse=FALSE) {

    # determine the colors
    if (any(is.na(colors))) {
        colors = chameleon::distinct_colors(n_distinct(df[, color_by]))$name
    }

    # initialize ggplot object
    pmain <- ggplot(df %>% sample_frac(1), aes(!!sym(x_variable), !!sym(y_variable),
        color = !!sym(color_by),
    ))
    
    # draw the ellipse first so that it is underneath the points
    if (add_ellipse) {
        pmain <- pmain + stat_ellipse(
            geom = "polygon", aes(color = !!sym(color_by), fill = !!sym(color_by)), alpha = alpha-0.1,
            level = 0.95
        )
    }

    pmain <- pmain + geom_point(size = pt_size, alpha = 0.9) +
        xlab(xlab) +
        ylab(ylab) +
        theme_Publication() +
        scale_color_manual(values = colors) +
        scale_fill_manual(values = colors)
    

    
    
    if (!is.na(shape_by)) {
        pmain <- pmain + aes(shape = !!sym(shape_by))
    }
    
    if (!show_density) {
        return(pmain)
    }

    xlim.update <- layer_scales(pmain)$x$get_limits()
    ylim.update <- layer_scales(pmain)$y$get_limits()

    ptop <- ggplot(data = df, aes(x = !!sym(x_variable), fill = !!sym(color_by), y=..scaled..)) +
        geom_density(linewidth = linewidth, alpha = alpha) +
        theme_void() %+%
        theme(legend.position = "none") +
        scale_x_continuous(limits = xlim.update) +
        scale_fill_manual(values = colors)

    pright <- ggplot(data = df, aes(x = !!sym(y_variable), fill = !!sym(color_by), y=..scaled..)) +
        geom_density(linewidth = linewidth, alpha = alpha) +
        coord_flip() +
        theme_void() %+%
        theme(legend.position = "none") +
        scale_fill_manual(values = colors)



    layout <- "
    A#
    BC
    "
    final_plot <- wrap_plots(ptop, pmain, pright) +
        plot_layout(design = layout, widths = c(1, density_plot_ratio), heights = c(density_plot_ratio, 1))
    return(final_plot)
}
