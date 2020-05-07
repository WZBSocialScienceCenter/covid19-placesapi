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
SUBTITLE_RATIOS <- 'Geometric mean of ratio between current popularity and usual popularity per place.'

# common shortcut objects / functions

wzb_theme <- theme_minimal() + theme(
    text = element_text(family = "Dahrendorf Light"),
    axis.text = element_text(size = 11)
)

rotate_x_axis_labels <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

zero_intercept <- geom_hline(yintercept = 0, linetype = 3)
one_intercept <- geom_hline(yintercept = 1, linetype = 3)

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

plot_categ_means_ci <- function(meansdata, x, y, ymin, ymax, title, subtitle, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    
    ggplot(meansdata, aes(x = !!x, y = !!y)) +
        geom_point() +
        geom_linerange(aes(ymin = !!ymin, ymax = !!ymax)) +
        one_intercept +
        ylim(0, max(pull(meansdata, !!ymax))) +
        scale_x_discrete(limits = rev(sort(unique(pull(meansdata, !!x))))) +
        coord_flip() +
        add_labels(title, collection_time, subtitle) +
        wzb_theme
}

plot_categ_means_country_ci <- function(meansdata, x, y, ymin, ymax, facet, title, subtitle, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    facet <- enquo(facet)
    
    ggplot(meansdata, aes(x = reorder(country, desc(!!x)), y = !!y)) +
        geom_point() +
        geom_linerange(aes(ymin = !!ymin, ymax = !!ymax)) +
        one_intercept +
        ylim(0, max(pull(meansdata, !!ymax))) +
        #scale_x_discrete(limits = rev(sort(unique(pull(meansdata, !!x))))) +
        coord_flip() +
        facet_wrap(vars(!!facet), ncol = 2, scales = 'free_y') +
        add_labels(title, collection_time, subtitle) +
        wzb_theme
}

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

plot_means_hbars <- function(meansdata, x, y, title, collection_time) {
    x <- enquo(x)
    y <- enquo(y)
    ggplot(meansdata, aes(x = !!x, y = !!y)) +
        geom_col() +
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


plot_daily_cat_means_ribbon <- function(meansdata, x, y, ymin, ymax, color, facet, title, subtitle, collection_time, ribbon = TRUE) {
    x <- enquo(x)
    y <- enquo(y)
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    color <- enquo(color)
    facet <- enquo(facet)
    
    xrange = range(pull(meansdata, !!x))
    
    p <- ggplot(meansdata, aes(x = !!x, y = !!y, color = !!color))
    
    if (ribbon) {
        p <- p + geom_ribbon(aes(ymin = !!ymin, ymax = !!ymax, fill = !!color), color = NA, alpha = 0.25)
    }
    
    cmap <- c(WZB_BLUE, WZB_VIOL)
    names(cmap) <- c(FALSE, TRUE)
    
    p + geom_line() +
        geom_point() +
        scale_x_continuous(breaks = seq(xrange[1], xrange[2], 2)) +
        scale_color_manual(labels = c('working day', 'weekend'), values = cmap, guide = guide_legend(NULL)) +
        scale_fill_manual(values = cmap, guide = FALSE) +
        one_intercept +
        add_labels(title, collection_time, subtitle) +
        facet_wrap(vars(!!facet), ncol = 2, scales = 'free_y') +
        wzb_theme +
        theme(legend.position = 'bottom')
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
    
    minmax <- range(meansdata$mean_mean_pop_diff)
    guide_labels <- round(c(minmax[1], minmax[1] / 2, 0, minmax[2] / 2, minmax[2]))
    
    p + coord_sf(xlim = c(-120, 180), ylim = c(-55, 70), datum = NA) +
        scale_color_distiller(palette = 'RdBu', direction = 1,
                              breaks = seq(-1, 1, by = 0.5),
                              labels = guide_labels,
                              guide = guide_legend(title = 'Mean popularity\ndifference'),
                              aesthetics = c("colour", "fill")) +
        labs(x = NULL, y = NULL) +
        add_labels(title, collection_time) +
        wzb_theme
}

plot_choropleth <- function(cntry_means_plotdata, disp_win_bottom_left, disp_win_top_right, target_crs,
                            title, subtitle, collection_time) {
    disp_win_wgs84 <- st_sfc(disp_win_bottom_left, disp_win_top_right, crs = 4326)
    disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)
    disp_win_coord <- st_coordinates(disp_win_trans)
    
    
    ggplot(cntry_means_plotdata) +
        geom_sf(aes(geometry = geometry, fill = mean_bins)) +
        scale_fill_brewer(palette = 'OrRd', na.value = "grey90",
                          guide = NULL, direction = -1) +
        geom_label_repel(aes(x = X, y = Y, label = label), size = 2.5) +
        coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
                 datum = NA, expand = FALSE) +
        add_labels(title, collection_time, subtitle) +
        wzb_theme + theme(axis.title = element_blank())
}
