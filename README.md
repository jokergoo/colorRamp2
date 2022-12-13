

# colorRamp2: Generate Color Mapping Functions

## Install

The package can be installed from CRAN:

```r
install.packages("colorRamp2")
```

or directly from GitHub:

```r
devtools::install_github("jokergoo/colorRamp2")
```

## Usage


The `colorRamp2()` function can generate a color mapping function from a vector of break values
and a vector of corresponding colors. Other colors are linearly interpolated in a certain color space.

```r
library(colorRamp2)

col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
col_fun(seq(0, 1, length = 20))
```

```
##  [1] "#0000FFFF" "#522CFFFF" "#7448FFFF" "#8E61FFFF" "#A479FFFF" "#B891FFFF"
##  [7] "#C9A9FFFF" "#DAC1FFFF" "#E9DAFFFF" "#F8F3FFFF" "#FFF5F1FF" "#FFE1D6FF"
## [13] "#FFCDBBFF" "#FFB8A1FF" "#FFA388FF" "#FF8E6EFF" "#FF7756FF" "#FF5E3DFF"
## [19] "#FF3F23FF" "#FF0000FF"
```

```r
plot(NULL, xlim = c(0, 1), ylim = c(0, 1))
x = seq(0, 1, length = 20)
y = rep(0.5, 20)
points(x, y, pch = 16, col = col_fun(x), cex = 2)
```

![image](https://user-images.githubusercontent.com/449218/207290643-548390f6-d09e-4ad0-bcc3-9864ca5b87e6.png)


## License

MIT @ Zuguang Gu
