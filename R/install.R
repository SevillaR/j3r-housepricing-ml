libraries <- c('ggplot2', 'dplyr', 'tidyr', 'skimr', 'caret', 'xgboost')

for (lib in libraries) {
  # if (!lib %in% installed.packages()) install.packages(lib)
  if (require(lib) == FALSE)   install.packages(lib)
}


