
library(dmr.stats)
library(dmr.util)

dd.chi2 <- function(a1, a2) 1-chisq.test(a1, a2)$p.value
cd.kruskal <- function(a1, a2) 1-kruskal.test(a1, a2)$p.value
cc.spearman <- function(a1, a2) 1-cor.test(a1, a2, method="spearman")$p.value

#' @title Simple attribute filter
#'
#' @param formula 
#' @param data data to perform feature selection
#' @param dd test to perform selection with discrete attributes and discrete target
#' @param cd test to perform selection with continuous attributes and discrete target
#' @param cc test to perform selection with continuous attributes and discrete target
#'
#' @return named by attribute list of test measures
#' 
simple.filter <- function(formula, data, dd=dd.chi2, cd=cd.kruskal, cc=cc.spearman)
{
  attributes <- x.vars(formula, data)
  target <- y.var(formula)
  utility <- function(a)
  {
    unname(switch(attr.type(data[[a]], data[[target]]),
                  dd = dd(data[[a]], data[[target]]),
                  cd = cd(data[[a]], data[[target]]),
                  dc = cd(data[[target]], data[[a]]),
                  cc = cc(data[[a]], data[[target]])))
  }
  sort(sapply(attributes, utility), decreasing=TRUE)
}