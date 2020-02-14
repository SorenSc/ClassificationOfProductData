# Libraries
library(scmamp)
library(multcompView)

# Read in data
results <- read.csv("~/DESIGN Paper/Code/results.csv")
columnNames = list('N', 'kNN', 'LR', 'RF', 'NN')
names(results) = columnNames
results = as.matrix(results)
results = results[,2:5]
head(results)

# Procedure based on the paper 'A practical tutorial on the use of nonparametric statistical tests as a methodology for comparing evolutionary and swarm intelligence algorithms' by Derrac et al. from 2011
friedman_result = postHocTest(results, test = 'friedman', correct = 'bergmann')
# heatmap(friedman_result$corrected.pval)
print(round(friedman_result$corrected.pval, digits=4))
#        kNN     LR     RF     NN
# kNN     NA 0.0000 0.0707 0.0000
# LR  0.0000     NA 0.0000 0.0043
# RF  0.0707 0.0000     NA 0.0000
# NN  0.0000 0.0043 0.0000     NA

groups = multcompLetters(friedman_result$corrected.pval, threshold = 0.01)
print(groups$monospacedLetters)
#   kNN    LR    RF    NN 
# "a  " " b " "a  " "  c" 