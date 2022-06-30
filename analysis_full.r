# library(tidyverse)
# install.packages('purrr', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(purrr)
# library(gplots)

cat("Loading dataset... ")
KDD <- read.csv("kddcup.data")
cat("done\n")

cat("\n === Meta analysis ===\n")
n_rows <- nrow(KDD)
n_cols <- ncol(KDD)
cat("Dataset has", n_rows, "rows and", n_cols, "columns\n")
col_names <- colnames(KDD)
cat("Column names: ["); cat(col_names, sep=', '); cat("]\n")

cat("\nAccording to the documentation, IVs\n")
cat_vars = c("Atr.1", "Atr.2", "Atr.3", "Atr.6", "Atr.7", "Atr.8", "Atr.10", "Atr.11", "Atr.13", "Atr.14", "Atr.17", "Atr.18", "Atr.19", "Atr.20", "Atr.21")
cat(cat_vars, "\n")
cat("are categorical, and\n")
real_vars = c("Atr.0", "Atr.4", "Atr.5", "Atr.9", "Atr.12", "Atr.15", "Atr.16", "Atr.22", "Atr.23", "Atr.24", "Atr.25", "Atr.26", "Atr.27", "Atr.28", "Atr.29", "Atr.30", "Atr.31", "Atr.32", "Atr.33", "Atr.34", "Atr.35", "Atr.36", "Atr.37", "Atr.38", "Atr.39", "Atr.40")
cat(real_vars, "\n")
cat("are real\n")
cat("The task is to classify the Class column (categorical)\n")

cat("\n === Individual column analysis ===\n")
unbalansed_cols = c()
const_cols = c()
for (col_name in col_names) {
    cat("\nAnalysing", col_name, "...\n")
    col <- KDD[[col_name]]
    
    unique_num <- length(unique(col))
    cat("Number of unique values:", unique_num, "\n")
    
    if (unique_num > 5) { cat("Top 5 most common values:\n")
    } else { cat("Most common values:\n") }
    top5_tbl <- sort(table(col, useNA = "ifany"), decreasing=TRUE)[1:min(5, unique_num)]
    top5_tbl_info <- stack(map(top5_tbl, function(x) paste(x, " (", round(x / n_rows * 100, 2), "%)", sep='')))
    colnames(top5_tbl_info)[1] <- "frequency"
    colnames(top5_tbl_info)[2] <- "value"
    print(top5_tbl_info[,c(2,1)])

    if (top5_tbl[1] > n_rows * 0.9998) {
        unbalansed_cols = append(unbalansed_cols, col_name)
    }
    if (top5_tbl[1] == n_rows) {
        const_cols = append(const_cols, col_name)
    }
}
cat("\nColumns", unbalansed_cols); cat(" are heavily unbalanced:\n")
cat("the most frequent value takes >99.98% of the total sample count (other vals total freq < ", floor(n_rows * 0.0002), ")\n", sep='')
cat("Column", const_cols, "is constant.\n")

cat("\nAtr.22 is equal to Atr.23 in ", round(length(which(KDD["Atr.22"] == KDD["Atr.23"])) / n_rows * 100, 2), "% of the cases\n", sep='')
cat("Atr.24 is equal to Atr.25 in ", round(length(which(KDD["Atr.24"] == KDD["Atr.25"])) / n_rows * 100, 2), "% of the cases\n", sep='')
Atr_28_one_is_zero <- KDD["Atr.28"]; Atr_28_one_is_zero[Atr_28_one_is_zero == 1] = 0;
cat("Atr.28, where 1 is changed to 0, is equal to Atr.29 in ", round(length(which(Atr_28_one_is_zero == KDD["Atr.29"])) / n_rows * 100, 2), "% of the cases\n", sep='')
cat("Atr.37 is equal to Atr.38 in ", round(length(which(KDD["Atr.37"] == KDD["Atr.38"])) / n_rows * 100, 2), "% of the cases\n", sep='')

cat("\nGenerating pairs.pdf... ")
kdd_1000 <- KDD[sample(n_rows, 1000), ]
pdf(file="pairs_plot.pdf", width=30, height=30)
pairs(kdd_1000[, real_vars])
dev.off()
cat("done.\n")
