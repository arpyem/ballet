library(tidyverse)

ein <- read_delim(
    file = "data/ein.txt", 
    delim = "|", 
    col_names = c(
        "ein", 
        "company_name", 
        "city", 
        "state", 
        "country", 
        "deductability_code"
    ), 
    col_types = cols()
)

ein_ballet <- ein %>%
    filter(grepl(pattern = "ballet", x = company_name, ignore.case = TRUE)) %>%
    arrange(company_name)


forms <- read_csv(file = "data/form_990.csv", col_types = cols(ein = "c"))

forms_ballet <- ein_ballet %>% inner_join(forms, by = "ein")

# SRQ
ein_ballet %>%
    filter(ein == "650135900") %>%
    inner_join(forms)

write_csv(x = forms_ballet, file = "data/forms_ballet.csv")

