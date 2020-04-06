library(ggplot2)
library(RColorBrewer)
library(sf)
library(ggrepel)
library(rnaturalearth)
library(extrafont)    # Dahrendorf font (siehe https://stackoverflow.com/q/30058107)

WZB_BLUE <- '#0380B5'
WZB_VIOL <- '#9E3173'
WZB_GREEN <- '#619933'
WZB_PLUS <- '#272F35'

DEFAULT_SUBTITLE <- 'Difference between current popularity score at local time and usual popularity at the same place.'

# common shortcut objects / functions

wzb_theme <- theme_minimal() + theme(
    text = element_text(family = "Dahrendorf Light"),
    axis.text = element_text(size = 11)
)

rotate_x_axis_labels <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

zero_intercept <- geom_hline(yintercept = 0, linetype = 3)

add_labels <- function(title, collection_time = NULL, subtitle = DEFAULT_SUBTITLE, xaxislab = NULL, yaxislab = NULL) {
    if (!is.null(collection_time)) {
        caption <- paste('source: data retrieved from Google Places API between',
              collection_time[1], 'UTC and', collection_time[2], 'UTC')
    } else {
        caption <- NULL
    }
    
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xaxislab, y = yaxislab)
}


# specific plotting functions

plot_means_errorbars <- function(meansdata, x, y, ymin, ymax, title, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    ggplot(meansdata, aes(x = !!x, y = !!y)) +
        geom_point() +
        geom_linerange(aes(ymin = !!ymin, ymax = !!ymax)) +
        zero_intercept +
        scale_x_discrete(limits = rev(sort(unique(pull(meansdata, !!x))))) +
        coord_flip() +
        add_labels(title, collection_time) +
        wzb_theme
}


plot_ts_means_ribbon <- function(meansdata, x, y, ymin, ymax, title, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    
    if ('POSIXct' %in% class(pull(meansdata, !!x))) {
        xscale <- scale_x_datetime(date_breaks = '1 day', date_minor_breaks = '6 hours', date_labels = '%b %d')
    } else {
        xscale <- scale_x_date(date_breaks = '1 day', date_labels = '%b %d')
    }
    
    ggplot(meansdata, aes(x = !!x, y = !!y)) +
        geom_ribbon(aes(ymin = !!ymin, ymax = !!ymax), alpha = 0.25) +
        geom_line() +
        geom_point() +
        zero_intercept +
        xscale +
        add_labels(title, collection_time) +
        wzb_theme
}


plot_ts_cat_means_ribbon <- function(meansdata, x, y, ymin, ymax, color, title, collection_time, ribbon = TRUE) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    color <- enquo(color)
    
    if ('POSIXct' %in% class(pull(meansdata, !!x))) {
        xscale <- scale_x_datetime(date_breaks = '1 day', date_minor_breaks = '6 hours', date_labels = '%b %d')
    } else {
        xscale <- scale_x_date(date_breaks = '1 day', date_labels = '%b %d')
    }
    
    p <- ggplot(meansdata, aes(x = !!x, y = !!y, color = !!color))
    
    if (ribbon) {
        p <- p + geom_ribbon(aes(ymin = !!ymin, ymax = !!ymax, fill = !!color), color = NA, alpha = 0.25)
    }
    
    p + geom_line() +
        geom_point() +
        zero_intercept +
        xscale +
        scale_color_brewer(palette = "Dark2", guide = guide_legend(NULL)) +
        scale_fill_brewer(palette = "Dark2", guide = FALSE) +
        add_labels(title, collection_time) +
        wzb_theme
}


plot_daily_means_ribbon <- function(meansdata, x, y, ymin, ymax, color, title, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    color <- enquo(color)
    
    xrange = range(pull(meansdata, !!x))
    
    cmap <- c(WZB_BLUE, WZB_VIOL)
    names(cmap) <- c(FALSE, TRUE)
    
    ggplot(meansdata, aes(x = !!x, y = !!y, color = !!color)) +
        geom_ribbon(aes(ymin = !!ymin, ymax = !!ymax, fill = !!color), color = NA, alpha = 0.25) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = seq(xrange[1], xrange[2], 2)) +
        scale_color_manual(labels = c('working day', 'weekend'), values = cmap, guide = guide_legend(NULL)) +
        scale_fill_manual(values = cmap, guide = FALSE) +
        zero_intercept +
        add_labels(title, collection_time) +
        wzb_theme
}


plot_daily_cat_means_ribbon <- function(meansdata, x, y, ymin, ymax, color, title, collection_time, ribbon = TRUE) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    color <- enquo(color)
    
    xrange = range(pull(meansdata, !!x))
    
    p <- ggplot(meansdata, aes(x = !!x, y = !!y, color = !!color))
    
    if (ribbon) {
        p <- p + geom_ribbon(aes(ymin = !!ymin, ymax = !!ymax, fill = !!color), color = NA, alpha = 0.25)
    }
    
    p + geom_line() +
        geom_point() +
        scale_x_continuous(breaks = seq(xrange[1], xrange[2], 2)) +
        scale_color_brewer(palette = "Dark2", guide = guide_legend(NULL)) +
        scale_fill_brewer(palette = "Dark2", guide = FALSE) +
        zero_intercept +
        add_labels(title, collection_time) +
        facet_wrap(~ local_weekend, ncol = 1,
                   labeller = as_labeller(c(`FALSE` = 'working day', `TRUE` = 'weekend'))) +
        wzb_theme
}


plot_country_daily_means <- function(meansdata, x, y, ymin, ymax, color, title, collection_time, ribbon = TRUE) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    color <- enquo(color)
    
    p <- ggplot(meansdata, aes(x = !!x, y = !!y, color = !!color))
    
    if (ribbon) {
        p <- p + geom_ribbon(aes(ymin = !!ymin, ymax = !!ymax, fill = !!color), color = NA, alpha = 0.25)
    }
    
    p + geom_line() +
        geom_point() +
        scale_color_brewer(palette = "Dark2", guide = guide_legend(NULL)) +
        scale_fill_brewer(palette = "Dark2", guide = FALSE) +
        zero_intercept +
        add_labels(title, collection_time) +
        wzb_theme
}


plot_country_categories <- function(meansdata, x, y, ymin, ymax, title, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    ggplot(meansdata, aes(x = reorder(country, desc(!!x)), y = !!y)) +
        geom_point() +
        geom_linerange(aes(ymin = !!ymin, ymax = !!ymax)) +
        zero_intercept +
        coord_flip() + 
        facet_wrap(~ category, ncol = 3, scales = 'free_y') + 
        add_labels(title, collection_time) +
        wzb_theme
}


plot_cities_map <- function(meansdata, title, collection_time, draw_labels = TRUE) {
    mapdata <- ne_countries(type = 'map_units', returnclass = 'sf')
    cities_means_plotdata <- mutate(meansdata,
                                    label = paste(city, round(mean_mean_pop_diff)),
                                    scaled_mean = ifelse(mean_mean_pop_diff < 0,
                                                         mean_mean_pop_diff / abs(min(mean_mean_pop_diff)),
                                                         mean_mean_pop_diff / abs(max(mean_mean_pop_diff))))
    
    p <- ggplot(cities_means_plotdata) +
        geom_sf(data = mapdata) +
        geom_point(aes(x = lng, y = lat), color = 'white', size = 3) +
        geom_point(aes(x = lng, y = lat, color = scaled_mean), size = 2)
    
    if (draw_labels) {
        p <- p + geom_label_repel(aes(x = lng, y = lat, label = label, fill = scaled_mean), size = 2.5)
    }
    
    p + coord_sf(xlim = c(-120, 180), ylim = c(-55, 70), datum = NA) +
        scale_color_distiller(palette = 'RdBu', direction = 1,
                              guide = FALSE, aesthetics = c("colour", "fill")) +
        labs(x = NULL, y = NULL) +
        add_labels(title, collection_time) +
        wzb_theme
}
