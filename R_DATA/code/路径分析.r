devtools::install_github('timelyportfolio/sunburstR', force = TRUE)
#　导入sequences数据
sequences = read.csv(
  system.file('examples/visit-sequences.csv', package = 'sunburstR'),
  header = FALSE,
  stringsAsFactors = FALSE
)
# 查看前6行
head(sequences)
library(sunburstR)
sunburst(sequences)