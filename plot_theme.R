theme_Publication <- function(base_size = 14) {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size = base_size)
    + theme(
            plot.title = element_text(
                face = "bold",
                size = rel(1.2), hjust = 0.5
            ),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(0.3, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 12),
            plot.margin = unit(c(3, 3, 3, 3), "mm"),
            strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
            strip.text = element_text(face = "bold")
        ))
}


theme_Publication_side <- function(base_size = 14) {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size = base_size)
    + theme(
            plot.title = element_text(
                face = "bold",
                size = rel(1.2), hjust = 0.5
            ),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(3, 3, 3, 3), "mm"),
            strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
            strip.text = element_text(face = "bold")
        ))
}


small_axis <- function(label = NULL, fontsize = 7, arrow_length = 25, fix_coord = FALSE, arrow_offset = 6,
                       arrow_spec = grid::arrow(ends = "both", type = "closed", angle = 20, length = unit(arrow_length / 7, units)),
                       units = "mm", ...) {
    coord <- if (fix_coord) {
        coord_fixed(clip = "off", ...)
    } else {
        NULL
    }
    if (is.null(label)) {
        label <- "UMAP"
    }
    lines <- annotation_custom(grid::polylineGrob(
        x = unit(c(arrow_offset, arrow_offset, arrow_length), units),
        y = unit(c(arrow_length, arrow_offset, arrow_offset), units),
        gp = grid::gpar(fill = "black"),
        arrow = arrow_spec
    ))
    text <- if (!is.null(label)) {
        list(
            annotation_custom(grid::textGrob(
                label = paste0(label, "1"), gp = grid::gpar(fontsize = fontsize, fontface = "bold"),
                x = unit(arrow_offset, units), y = unit(arrow_offset, units),
                hjust = 0, vjust = 1.4
            )),
            annotation_custom(grid::textGrob(
                label = paste0(label, "2"), rot = 90, gp = grid::gpar(fontsize = fontsize, fontface = "bold"),
                x = unit(arrow_offset, units), y = unit(arrow_offset, units),
                hjust = 0, vjust = -0.45
            ))
        )
    } else {
        NULL
    }

    theme_plot <- theme_void() %+%
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

    list(coord, lines, text, theme_plot)
}
