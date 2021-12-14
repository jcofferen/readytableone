##' Create a publication-ready table of descriptive statistics stratified by binary variable. 
##'
##' This package will prepare a high-quality table of descriptive statistics for a pre-formatted dataframe containing numeric and factor variables.
##' @title ReadyTableOne: A Quick Table 1 of Descriptive Statistics
##' @param x data frame containing numeric and factor variables
##' @param stratify variable by which to stratify (e.g., treatment)
##' @param nonpara use non-parametric estimates (i.e., median, IQR)
##' @return table of descriptive statistics
##' @author Jake Cofferen
##' @import tidyverse kableExtra
##' @export
##' @examples  
##' rto(df, stratify = "vs")

rto <- function (x, stratify, nonpara = FALSE){
  test <- NULL
  ifelse(is.data.frame(x), 
         test <- "OK",
         test <- "Fail")
  
  ## assign the dataframe
  df <- x
  
  ## variable for stratification
  strat <- stratify
  strat_cols <- nlevels(df[,eval(strat)])
  strat_levels <- levels(df[,eval(strat)])
  
  ## parametric center and spread for continuous variables
  means <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, mean)
  sds <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, sd)
  ns <- df %>% group_by_at(strat) %>% summarise(n = n())
  
  ## nonparametric center and spread for continuous variables
  medians <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, median)
  ranges <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, c(min, max))
  iqrs <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, IQR)
  
  
  ## create numeric summary table
  
  res_num <- matrix(NA, nrow = 2*(ncol(means)-1), ncol = strat_cols+1)
  
  tbl_names <- numeric(ncol(res_num))
  
  for(i in 1:ncol(res_num)){
    ifelse(i == 1,
           tbl_names[i] <- "",
           tbl_names[i] <- paste0(strat_levels[i-1], " (n = ", ns[i-1, 2], ")"))
  }
  
  colnames(res_num) <- tbl_names
  
  ## name variables and center (spread)
  j <- 2
  
  
  for(i in 1:nrow(res_num)){
    ifelse(i %% 2 == 0,
           res_num[i, 1] <- "Mean (SD)",
           res_num[i, 1] <- colnames(means[j]))
    ifelse(i %% 2 != 0,
           j <- j + 1,
           j <- j)
  }
  
  ## add values
  
  j <- 1
  k <- 1
  
  for(b in 1:strat_cols){
    for(i in 1:nrow(res_num)){
      ifelse(i %% 2 == 0,
             res_num[i, k+1] <- paste0(signif(means[k, j], 3), " (", signif(sds[k, j], 3), ")"),
             res_num[i, k+1] <- "")
      ifelse(i %% 2 != 0,
             j <- j + 1,
             j <- j)
    }
    
    j <- 1
    k <- k +1
  }
  
  
  res_num %>% kbl() %>%  add_indent(seq(2, nrow(res_num), 2)) %>% kable_classic(full_width = FALSE)
  
}
