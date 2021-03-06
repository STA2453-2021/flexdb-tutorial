
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flexdb-tutorial

[`flexdashboard`](https://pkgs.rstudio.com/flexdashboard/index.html) is
an R package for combining visualizations (tabular and graphical) into
flexible dashboards. This tutorial will demonstrate the ease of use in
building complex layouts through a series of examples. flexdashboard
works great with both base R graphics and ggplot, but it really shines
when working with other visualization libraries.

Here are some of the libraries that are used throughout this
presentation along with links to where you can find more information

-   JavaScript visualizations with the [`htmlwidgets`
    package](https://www.htmlwidgets.org/)

-   Tabular data with the [DT package](https://rstudio.github.io/DT/).

-   Value boxes (`flexdashboard::valuebox()`)for highlighting important
    summary data .

-   Gauges (`flexdashboard::gauge()`)for displaying values on a meter
    within a specified range.

-   Maps through the [leaflet
    package](http://rstudio.github.io/leaflet/)

-   The [bslib package](https://rstudio.github.io/bslib/) for adding
    bootstrap/css styling.

-   The [highcharter package](https://jkunst.com/highcharter/) for
    interactive graphics

-   The [rbokeh package](https://hafen.github.io/rbokeh/) for
    interactive graphics

This repository contains a series of .Rmd files demonstrating a good
selection of the variety that is available through flexdashboard. Just
follow along with this README file to know which .Rmd file to use to see
the specified flexibility.

## Using `flexdashboard`

flexdashboard is used via Rmarkdown files (.Rmd) as an output option. In
the YAML metadata at the top of the document, simply specify:

    ---
    title: "My Dashboard"
    output: flexdashboard::flex_dashboard
    ---

### Layouts

A panel in flexdashboard is an area you can place a plot, a table, or
some descriptive text. These are created with the use of 3 hashtags
(`###`). By default, flexdashboard has a column based orientation. You
can start a new column by using the level 2 markdown header (a series of
hyphens)

See the .Rmd files in the `layout-examples` folder for examples of the
different layout possibilities, including

1.  flex rows
2.  flex columns
3.  scrolling
4.  tab panels
5.  multiple pages
6.  story boards

### Widgets

The htmlwidgets package extends R graphics to JavaScript visualization
packages. Visualizations designed with javascript give an extra level of
user interactivity. In addition these visualizations can be resized on
the fly to fit perfectly in your dashboard display.

See the examples in the the widget-examples folder

1.  dygraph
2.  leaflet and datatable
3.  valueboxes and gauges

### Styling and themes

The folder (styling) has examples for adding and altering a navigation
bar, adding links to external pages, and changing the visual theme of
the dashboard.

A large variety of bootswatch themes can be used to later the appearance
of your dashboard.

``` r
themes <- c('default', 'cosmo', 'bootstrap', 'cerulean',
            'journal', 'flatly', 'readable', 'spacelab',
            'united', 'lumen', 'paper', 'sandstone', 'simplex',
            'yeti')
```

In addition, you can use the bslib package to gain the flexibility of
modern css frameworks like bootstrap 5.

## shiny runtime

You can also use shiny to include additional user input capabilities.
See the `with-shiny` folder for an example using NBA data. All the code
and data is available here on this repo.
