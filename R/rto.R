##' Create a publication-ready table (Description)
##'
##' This package will prepare a high-quality table of descriptive statistics for a pre-formatted dataframe containing numeric and factor variables. (Details)
##' @title add two numbers
##' @param x data frame containing numeric and factor variables
##' @param stratify variable by which to stratify (e.g., treatment)
##' @param nonpara use non-parametric estimates (i.e., median, IQR)
##' @return table of descriptive statistics
##' @author Jake Cofferen
##' @export

rto <- function (x, stratify = y, nonpara = FALSE){

  ## assign the dataframe
  df <- x
  
## variable for stratification
strat <- y
strat_cols <- nlevels(df[,eval(strat)])
strat_levels <- levels(df[,eval(strat)])
strat_labels <- c("V-shaped", "Straight")

## parametric center and spread for continuous variables
means <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, mean)
sds <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, sd)
ns <- df %>% group_by_at(strat) %>% summarise(n = n())



means <- data.frame(means[-1])
sds <- data.frame(sds[-1])

#colnames(means) <- nums_labels

## nonparametric center and spread for continuous variables
medians <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, median)
ranges <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, c(min, max))
iqrs <- df %>% group_by_at(strat) %>% summarise_if(is.numeric, IQR)


## create numeric summary table

res_num <- matrix(NA, nrow = 2*(ncol(means)-1), ncol = strat_cols+1)

colnames(res_num) <- c("", 
                       paste0(strat_labels[1], " (n = ", ns[1, 2], ")"),
                       paste0(strat_labels[2], " (n = ", ns[2, 2], ")"))

## name variables and center (spread)
j <- 1


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

for(i in 1:nrow(res_num)){
  ifelse(i %% 2 == 0,
         res_num[i, 2] <- paste0(signif(means[1, j], 3), " (", signif(sds[1, j], 3), ")"),
         res_num[i, 2] <- "")
  ifelse(i %% 2 != 0,
         j <- j + 1,
         j <- j)
}

j <- 1

for(i in 1:nrow(res_num)){
  ifelse(i %% 2 == 0,
         res_num[i, 3] <- paste0(signif(means[2, j], 3), " (", signif(sds[2, j], 3), ")"),
         res_num[i, 3] <- "")
  ifelse(i %% 2 != 0,
         j <- j + 1,
         j <- j)
}

res_num %>% kbl(align = c("l", "c", "c")) %>%  add_indent(seq(2, nrow(res_num), 2)) %>% kable_classic(full_width = FALSE)


}