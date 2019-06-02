
library(tidyverse)

idx.returns <- read_csv("../data/index-returns.csv", col_names = TRUE)
sec.returns <- read_csv("../data/security-returns.csv", col_names = TRUE)

cleaned.sec <- sec.returns %>% 
  select(date, TICKER, RET) %>%
  mutate(
    RET = as.numeric(RET)
  ) %>%
  filter(!is.na(RET)) %>%
  group_by(date, TICKER) %>% 
  filter(row_number()==1)

pivot.sec <- cleaned.sec %>% spread(key=TICKER, value=RET) %>% ungroup()
idx.returns.filtered <- idx.returns %>% filter(caldt <= 19911231) %>% mutate(date = caldt)

results <- inner_join(idx.returns.filtered, pivot.sec, by="date") %>% select(-c(caldt, date))

# remove any with NAs
results <- results %>% select_if(~ !any(is.na(.)))

# write outputs
write.table(results, "combined-data.csv", sep=",", col.names=FALSE, row.names = FALSE)
