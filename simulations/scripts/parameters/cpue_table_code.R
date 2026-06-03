library(gt)
library(tidyverse)

df <- read.csv("simulations/cpue-dataframe.csv")

df %>% 
  arrange(Method, Study) %>%                # keep rows grouped & ordered
  gt(groupname_col = "Method") %>%             # turn Method into row-group header
  cols_hide(columns = Method) %>%              # don’t repeat Method inside the table
  cols_label(
    Study = "",                             # blank stub under Method group
    CPUE.estimate  = "CPUE Estimate"
  ) %>% 
  fmt_number(
    columns = CPUE.estimate,
    decimals = 3,
    use_seps = FALSE
  ) %>%
  tab_header(title = "Burbot CPUE estimates by capture method and study") %>% 
  
  # Optional niceties to echo the notebook layout ----
tab_style(                                   # indent the Location names a bit
  style = cell_text(indent = px(20)),
  locations = cells_body(columns = Study)
) %>% 
  tab_style(                                   # bold the row-group labels
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )


