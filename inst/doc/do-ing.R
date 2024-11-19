## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
library(where)
library(dplyr)
library(data.table)
library(ggplot2)

## -----------------------------------------------------------------------------
# subgroups <- .(all        = TRUE,
#                long_sepal = Sepal.Length > 6,
#                long_petal = Petal.Length > 5.5)
# 
# (iris %>%
#   filter(x) %>%
#   summarise(across(Sepal.Length:Petal.Width,
#                    mean),
#             .by = Species)) %for% subgroups

## ----subgroups----------------------------------------------------------------
# subgroups <- .(all        = TRUE,
#                long_sepal = Sepal.Length > 6,
#                long_petal = Petal.Length > 5.5)

## ----repetition---------------------------------------------------------------
# # With base R
# iris
# iris[iris[["Sepal.Length"]] > 6, ] # or with(iris, iris[Sepal.Length > 6])
# iris[iris[["Petal.Length"]] > 5.5, ] # or with(iris, iris[Petal.Length > 5.5])
# 
# # With dplyr
# iris
# filter(iris, Sepal.Length > 6)
# filter(iris, Petal.Length > 5.5)
# 
# # With data.table
# iris
# as.data.table(iris)[Sepal.Length > 6]
# as.data.table(iris)[Petal.Length > 5.5]

## ----eval---------------------------------------------------------------------
# lapply(subgroups, function(group) with(iris, iris[eval(group), ]))

## -----------------------------------------------------------------------------
# run(with(iris, iris[subgroup, ]),
#        subgroup = subgroups)
# 
# # or
# with(iris, iris[x, ]) %for% subgroups

## ----filter_summarise---------------------------------------------------------
# library(dplyr)
# 
# subgroups = .(all        = TRUE,
#               long_sepal = Sepal.Length > 6,
#               long_petal = Petal.Length > 5.5)
# functions = .(mean, sum, prod)
# 
# run(
#   iris %>%
#     filter(subgroup) %>%
#     summarise(across(Sepal.Length:Petal.Width,
#                      summary),
#               .by = Species),
#   subgroup = subgroups,
#   summary  = functions
# )

## ----filter_summarise_dt------------------------------------------------------
# library(data.table)
# df <- as.data.table(iris)
# 
# run(df[subgroup, lapply(.SD, functions), keyby = "Species",
#       .SDcols = Sepal.Length:Petal.Width],
#    subgroup  = subgroups,
#    functions = functions)

## ----ggplot-------------------------------------------------------------------
# library(ggplot2)
# 
# plots <- run(
#   ggplot(filter(iris, subgroup),
#          aes(Sepal.Length, Sepal.Width)) +
#     geom_point() +
#     theme_minimal(),
#   subgroup = subgroups
# )
# 
# Map(function(plot, name) plot + ggtitle(name), plots, names(plots))

## ----ggplots------------------------------------------------------------------
# run(
#   ggplot(iris,
#          aes(Sepal.Length, Sepal.Width)) +
#     plot +
#     theme_minimal(),
#   plot = .(geom_point(),
#            geom_smooth())
# )

## ----fail_compound_geom, eval = FALSE-----------------------------------------
# # Fails
# run(
#   ggplot(iris,
#          aes(Sepal.Length, Sepal.Width)) +
#     plot +
#     theme_minimal(),
#   plot = .(geom_point(),
#            geom_smooth(),
#            geom_quantile() + geom_rug())
# )

## ----fail_compound_geom2, eval = FALSE----------------------------------------
# # Fails
# ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#     (geom_quantile() + geom_rug()) +
#     theme_minimal()

## ----compound_geom------------------------------------------------------------
# run(
#   ggplot(iris,
#          aes(Sepal.Length, Sepal.Width)) +
#     plot +
#     theme_minimal(),
#   plot = .(point  = geom_point(),
#            smooth = geom_smooth(),
#            quantilerug = list(geom_quantile(),
#                               geom_rug()))
# )
# 
# # or by separating out the combined geoms as a function (also using a list)
# geom_quantilerug <- function() list(geom_quantile(),
#                                     geom_rug())
# 
# run(
#   ggplot(iris,
#          aes(Sepal.Length, Sepal.Width)) +
#     plot +
#     theme_minimal(),
#   plot = .(point  = geom_point(),
#            smooth = geom_smooth(),
#            quantilerug = geom_quantilerug())
# )

## ----function_on_parts--------------------------------------------------------
# population_summaries <- function(df) run(with(df, df[subgroup, ]),
#                                             subgroup = subgroups)
# 
# as.data.table(iris)[, .(population_summaries(.SD)), keyby = "Species"]

## ----apply_over_pops----------------------------------------------------------
# on_subpopulations <- function(expr,
#                               populations = subgroups)
#   eval(substitute(run(expr, subgroup = populations),
#                   list(expr = substitute(expr))))
# 
# on_subpopulations(as.data.table(iris)[subgroup])
# 
# on_subpopulations(
#   iris %>%
#     filter(subgroup) %>%
#     summarise(across(Sepal.Length:Petal.Width,
#                      mean),
#               .by = Species)
# )
# 
# on_subpopulations(
#   ggplot(filter(iris, subgroup),
#          aes(Sepal.Length, Sepal.Width)) +
#     geom_point() +
#     theme_minimal()
# )

## ----extra_subpop-------------------------------------------------------------
# subgroups = .(all        = TRUE,
#               long_sepal = Sepal.Length > 6,
#               long_petal = Petal.Length > 5.5,
#               veriscolor = Species == "versicolor")

## -----------------------------------------------------------------------------
# analyses <- .(subset    = as.data.table(iris)[subgroup],
#               summarise = iris %>%
#                 filter(subgroup) %>%
#                 summarise(across(Sepal.Length:Petal.Width,
#                                  mean),
#                           .by = Species),
#               plot      = ggplot(filter(iris, subgroup),
#                                  aes(Sepal.Length, Sepal.Width)) +
#                 geom_point() +
#                 theme_minimal())
# 
# lapply(analyses,
#        function(expr) do.call("on_subpopulations", list(expr)))

## -----------------------------------------------------------------------------
# on_subpopulations(
#   ggplot(filter(iris, subgroup),
#          aes(Sepal.Length, Sepal.Width)) +
#     geom_point() +
#     theme_minimal()
# )

## -----------------------------------------------------------------------------
# on_subpopulations <- function(expr,
#                               populations = subgroups) {
#   e <- parent.frame()
#   eval(substitute(run(expr, subgroup = populations, e = e),
#                   list(expr = substitute(expr))))
# }

## ----infixed------------------------------------------------------------------
# as.data.table(iris)[subgroup, lapply(.SD, summary), keyby = "Species",
#                     .SDcols = Sepal.Length:Petal.Width] %where%
#   list(subgroup = subgroups[1:3],
#        summary  = functions)
# 
# # note `subgroup` replaced with 'x'
# as.data.table(iris)[x, lapply(.SD, mean), keyby = "Species",
#                     .SDcols = Sepal.Length:Petal.Width] %for%
#   subgroups

## ----infixed_bracketed--------------------------------------------------------
# (iris %>%
#     filter(x) %>%
#     summarise(across(Sepal.Length:Petal.Width,
#                      mean),
#               .by = Species)) %for% subgroups

## ----with---------------------------------------------------------------------
# (a + b) %with% {
#   a = 1
#   b = 2
# }

