library(tidyverse)

forms <- read_csv("data/forms_ballet.csv")

forms %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    arrange(desc(totfuncexpns)) %>%
    select(ein:state, tax_pd, totfuncexpns, totrevenue, totprgmrevnue, totcntrbgfts) %>%
    mutate(r = round(totcntrbgfts / totprgmrevnue, 2)) %>%
    filter(r < Inf) %>%
    arrange(desc(r)) %>%
    print.data.frame()
