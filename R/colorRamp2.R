

# == title
# Generate color mapping functions
#
# == param
# -breaks A vector of numeric break values.
# -colors A vector of colors which correspond to values in ``breaks``.
# -transparency A single value in ``[0, 1]``. 0 refers to no transparency and 1 refers to full transparency.
# -space Color space in which colors are interpolated. Value should be one of "RGB", "LAB", "XYZ", "sRGB", "LUV", see `colorspace::color-class` for details.
# -hcl_palette Name of the HCL palette. Value should be supported in `grDevices::hcl.pals`.
# -reverse Whether should the colors in ``hcl_palette`` be reversed.
#
# == details
# Colors are linearly interpolated according to the break values and corresponding colors through a certain color space. 
# Values exceeding breaks will be assigned with corresponding maximum or minimum colors.
#
# == values
# A function which accepts a vector of numeric values and returns interpolated colors.
#
# == example
# col_fun = colorRamp2(c(-1, 0, 1), c("green", "white", "red"))
# col_fun(c(-2, -1, -0.5, 0, 0.5, 1, 2))
colorRamp2 = function(breaks, colors, transparency = 0, space = "LAB", 
    hcl_palette = NULL, reverse = FALSE) {

    breaks_o = breaks
    if(!is.null(hcl_palette)) {
        breaks = sort(breaks)
        if(length(breaks) == 2) {
            breaks2 = seq(breaks[1], breaks[2], length = 1000)
            colors = hcl.colors(1000, palette = hcl_palette, rev = reverse)
        } else {
            nb = length(breaks)
            nl = ceiling(1000/nb)
            breaks2 = seq(breaks[1], breaks[2], length = nl)
            for(i in 2:(nb-1)) {
                breaks2  = c(breaks2, seq(breaks[i], breaks[i+1], length = nl)[-1])
            }
            colors = hcl.colors(length(breaks2), palette = hcl_palette, rev = reverse)
        }
        breaks_o = breaks
        breaks = breaks2
    }

    if(length(breaks) != length(colors)) {
        stop("Length of `breaks` should be equal to `colors`.")
    }

    od = order(breaks)
    breaks = breaks[od]

    colors = colors[od]
    
    l = duplicated(breaks)
    breaks = breaks[!l]
    colors = colors[!l]

    if(length(breaks) == 1) {
        stop("You should have at least two distinct break values.")
    } 


    if(! space %in% c("RGB", "LAB", "XYZ", "sRGB", "LUV", "HLS")) {
        stop("`space` should be in 'RGB', 'LAB', 'XYZ', 'sRGB', 'LUV'.")
    }

    colors = t(col2rgb(colors)/255)

    
    if(space == "LUV") {
        i = which(apply(colors, 1, function(x) all(x == 0)))
        colors[i, ] = 1e-5
    }

    transparency = 1-ifelse(transparency > 1, 1, ifelse(transparency < 0, 0, transparency))[1]
    transparency_str = sprintf("%X", round(transparency*255))
    if(nchar(transparency_str) == 1) transparency_str = paste0("0", transparency_str)

    fun = function(x = NULL, return_rgb = FALSE, max_value = 1) {
        if(is.null(x)) {
            stop("Please specify `x`.")
        }

        att = attributes(x)
        if(is.data.frame(x)) x = as.matrix(x)

        l_na = is.na(x)
        if(all(l_na)) {
            return(rep(NA, length(l_na)))
        }

        x2 = x[!l_na]

        x2 = ifelse(x2 < breaks[1], breaks[1],
                   ifelse(x2 > breaks[length(breaks)], breaks[length(breaks)],
                          x2
                   ))
        ibin = .bincode(x2, breaks, right = TRUE, include.lowest = TRUE)
        res_col = character(length(x2))
        for(i in unique(ibin)) {
            l = ibin == i
            res_col[l] = .get_color(x2[l], breaks[i], breaks[i+1], colors[i, ], colors[i+1, ], space = space)
        }
        res_col = paste(res_col, transparency_str[1], sep = "")

        if(return_rgb) {
            res_col = t(col2rgb(as.vector(res_col), alpha = TRUE)/255)
            return(res_col)
        } else {
            res_col2 = character(length(x))
            res_col2[l_na] = NA
            res_col2[!l_na] = res_col

            attributes(res_col2) = att
            return(res_col2)
        }
    }

    attr = list(breaks = breaks_o, colors = fun(breaks_o), transparency = 1-transparency, space = space)

    attributes(fun) = attr
    return(fun)
}

.restrict_in = function(x, lower, upper) {
    x[x > upper] = upper
    x[x < lower] = lower
    x
}

# x: vector
# break1 single value
# break2 single value
# rgb1 vector with 3 elements
# rgb2 vector with 3 elements
.get_color = function(x, break1, break2, col1, col2, space) {

    col1 = coords(as(sRGB(col1[1], col1[2], col1[3]), space))
    col2 = coords(as(sRGB(col2[1], col2[2], col2[3]), space))

    res_col = matrix(ncol = 3, nrow = length(x))
    for(j in 1:3) {
    xx = (x - break2)*(col2[j] - col1[j]) / (break2 - break1) + col2[j]
    res_col[, j] = xx
    }

    res_col = get(space)(res_col)
    res_col = coords(as(res_col, "sRGB"))
    res_col[, 1] = .restrict_in(res_col[,1], 0, 1)
    res_col[, 2] = .restrict_in(res_col[,2], 0, 1)
    res_col[, 3] = .restrict_in(res_col[,3], 0, 1)
    hex(sRGB(res_col))
}


# == title
# Convert back from colors to values
#
# == param
# -r Red channel in `colorspace::sRGB` color space. Value should be between 0 and 1. 
#    The value can also be a character vector of colors or a three-column matrix with r, g, b as columns. In this case, ``g`` and ``b`` are ignored,
# -g Green channel in `colorspace::sRGB` color space. Value should be between 0 and 1.
# -b Blue channel in `colorspace::sRGB` color space. Value should be between 0 and 1.
# -col_fun the color mapping function generated by `colorRamp2`.
#
# == details
# `colorRamp2` maps values to colors and this function does the reversed job.
# Note for some color spaces, it cannot convert back to the original value perfectly.
#
# == value
# A vector of original numeric values.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# x = seq(0, 1, length.out = 11)
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
# col = col_fun(x)
# col2value(col, col_fun = col_fun)
# col2value("red", col_fun = col_fun)
#
# col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"), space = "sRGB")
# col = col_fun(x)
# col2value(col, col_fun = col_fun)
col2value = function(r, g, b, col_fun) {
    if(missing(g) && missing(b)) {
        if(is.character(r)) {
            rgb = col2rgb(r)/255
            r = rgb[1, ]
            g = rgb[2, ]
            b = rgb[3, ]
        } else {
            if(is.list(r)) {
                g = r[[2]]
                b = r[[3]]
                r = r[[1]]
            } else if(is.data.frame(r) || is.matrix(r)) {
                g = r[, 2]
                b = r[, 3]
                r = r[, 1]
            }
        }
    }

    breaks = attr(col_fun, "breaks")
    colors = attr(col_fun, "colors")
    space = attr(col_fun, "space")

    n = length(r)

    if(inherits(colors, "character")) {
        colors = t(col2rgb(colors))/255
    }

    ## convert all colors to the specified space
    m = coords(as(sRGB(r, g, b), space))
    breaks_m = coords(as(sRGB(colors), space))
    n_breaks = length(breaks)

    # interpolation for the three channels seperatedly
    v = numeric()
    for(i in seq_len(nrow(m))) {
        l1 = (breaks_m[-n_breaks, 1] <= m[i, 1] & breaks_m[-1, 1] >= m[i, 1]) | (breaks_m[-n_breaks, 1] >= m[i, 1] & breaks_m[-1, 1] <= m[i, 1])
        l2 = (breaks_m[-n_breaks, 2] <= m[i, 2] & breaks_m[-1, 2] >= m[i, 2]) | (breaks_m[-n_breaks, 2] >= m[i, 2] & breaks_m[-1, 2] <= m[i, 2])
        l3 = (breaks_m[-n_breaks, 3] <= m[i, 3] & breaks_m[-1, 3] >= m[i, 3]) | (breaks_m[-n_breaks, 3] >= m[i, 3] & breaks_m[-1, 3] <= m[i, 3])
        k = which(l1 & l2 & l3)[1]
        v1 = (breaks[k] - breaks[k+1])/(breaks_m[k, 1] - breaks_m[k+1, 1]) * (m[i, 1] - breaks_m[k, 1]) + breaks[k]
        v2 = (breaks[k] - breaks[k+1])/(breaks_m[k, 2] - breaks_m[k+1, 2]) * (m[i, 2] - breaks_m[k, 2]) + breaks[k]
        v3 = (breaks[k] - breaks[k+1])/(breaks_m[k, 3] - breaks_m[k+1, 3]) * (m[i, 3] - breaks_m[k, 3]) + breaks[k]
        vv = c(v1, v2, v3)
        v[i] = mean(vv, na.rm = TRUE)
    }
    return(v)
}

# == title
# Add transparency to colors
#
# == param
# -col A vector of colors.
# -transparency Transparency, numeric value between 0 and 1.
#
# == value
# A vector of colors.
#
# == example
# add_transparency("red", 0.5)
# add_transparency(1, 0.5)
# add_transparency("#FF000080", 0.2)
add_transparency = function (col, transparency = 0) {
    rgb(t(col2rgb(col)/255), alpha = 1 - transparency)
}

