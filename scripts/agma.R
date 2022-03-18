library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

excel_sheets(path = "master.xlsx")
df_company = read_excel(path = "master.xlsx", sheet = "Company Info")
df_salary = read_excel(path = "master.xlsx", sheet = "Compensation")

df_salary_latest = df_salary %>%
    group_by(Company) %>%
    filter(Year == max(Year)) %>%
    ungroup()

df_salary_summary = df_salary %>%
    group_by(Company, Year) %>%
    summarise(
        mean = mean(`Income Weekly`, na.rm = TRUE),
        median = median(`Income Weekly`, na.rm = TRUE),
        min = min(`Income Weekly`, na.rm = TRUE),
        max = max(`Income Weekly`, na.rm = TRUE)
    ) %>%
    arrange(median) %>%
    ungroup()

company_order <- df_salary_summary %>%
    group_by(Company) %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    arrange(median) %>%
    pull(Company)

df_salary %>%
    mutate(Company = factor(Company, levels = company_order)) %>%
    ggplot(aes(x = Company, y = `Income Weekly`)) +
    geom_boxplot(width = 0.5) +
    scale_y_continuous(name = "Weekly Income", labels = scales::label_dollar()) +
    coord_flip() +
    facet_wrap(~Year)


df_salary %>%
    group_by(Company) %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    mutate(Company = factor(Company, levels = company_order)) %>%
    ggplot(aes(x = Company, y = `Income Weekly`)) +
    geom_boxplot(width = 0.5) +
    scale_y_continuous(name = "Weekly Income", labels = scales::label_dollar()) +
    coord_flip()


df_salary %>%
    group_by(Company) %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    mutate(Company = factor(Company, levels = company_order)) %>%
    ggplot(aes(x = Company, y = `Income Total`)) +
    geom_boxplot(width = 0.5) +
    scale_y_continuous(name = "Base Pay", labels = scales::label_dollar()) +
    coord_flip()





df_salary_latest %>%
    group_by(Company) %>%
    summarise(min = min(`Income Total`), max = max(`Income Total`)) %>%
    ungroup() %>%
    arrange(max) %>%
    mutate(Company = factor(Company, levels = company_order)) %>%
    ggplot(aes(x = Company, y = min, xend = Company, yend = max)) +
    geom_segment() +
    scale_y_continuous(name = "Base Pay", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal()




# Compensation ranges
df_salary_latest %>%
    group_by(Company) %>%
    summarise(min = min(`Income Total`), max = max(`Income Total`)) %>%
    ungroup() %>%
    arrange(max) %>%
    mutate(Company = factor(Company, levels = unique(Company))) %>%
    mutate(highlight = grepl(pattern = "sarasota", Company, ignore.case = TRUE)) %>%
    ggplot(aes(x = Company, ymin = min, ymax = max, col = highlight)) +
    geom_errorbar(width = 0.3, size = 1) +
    scale_color_manual(values = c("TRUE" = "darkorange2", "FALSE" = "dodgerblue2")) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Base Pay", labels = scales::label_dollar()) +
    coord_flip() +
    ggtitle("Compensation ranges across all ranks", "excluding seniority") +
    theme_minimal() +
    theme(
        legend.position = "none"
    )




df_index = read_excel(path = "master.xlsx", sheet = "Company Info") %>%
    mutate(
        cost_of_living = as.numeric(`Cost of Living Index`),
        rent_index = as.numeric(`Rent Index`),
        purchasing_power = as.numeric(`Local Purchasing Power Index`),
        expenses = as.numeric(`Annual Expenses / Budget`),
        salary_artistic_director = as.numeric(`Artistic Director's Annual Salary`),
        salary_executive_director = as.numeric(`Executive Director's Annual Salary`)
    )

df_index %>%
    ggplot(aes(x = cost_of_living, y = purchasing_power)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point() +
    theme_minimal()

df_index %>%
    ggplot(aes(x = cost_of_living, y = rent_index)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point() +
    theme_minimal()

df_index %>%
    ggplot(aes(x = rent_index, y = purchasing_power)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point() +
    theme_minimal()



fit_index = lm(purchasing_power ~ cost_of_living + rent_index + expenses, data = df_index)
summary(fit_index)



df_salary_latest %>%
    group_by(Company) %>%
    summarise(min = min(`Income Total`), max = max(`Income Total`)) %>%
    ungroup() %>%
    arrange(max) %>%
    left_join(
        df_index %>% select(Company, contains("index"), contains("expense")),
        by = "Company"
    ) %>%
    mutate(Company = factor(Company, levels = unique(Company))) %>%
    mutate(highlight = grepl(pattern = "sarasota", Company, ignore.case = TRUE)) %>%
    ggplot(aes(x = Company, ymin = min, ymax = max, col = highlight)) +
    geom_errorbar(width = 0.3, size = 1) +
    scale_color_manual(values = c("TRUE" = "darkorange2", "FALSE" = "dodgerblue2")) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Base Pay", labels = scales::label_dollar()) +
    coord_flip() +
    ggtitle("Compensation ranges across all ranks", "excluding seniority") +
    theme_minimal() +
    theme(
        legend.position = "none"
    )





df_index = read_csv(
    file = "data/indices.csv", 
    col_names = c(
        "rank", 
        "city", 
        "index_cost_of_living", 
        "index_rent", 
        "index_cost_of_living_rent", 
        "index_groceries", 
        "index_restaurant", 
        "index_local_purchase_power"
    ), 
    skip = 1
) %>%
    select(-rank) %>%
    separate(col = city, into = c("city", "state", "country"), sep = ", ") %>%
    mutate(country = ifelse(is.na(country), state, country)) %>%
    mutate(state = ifelse(state == country, NA_character_, state))


# Use Tampa for Sarasota (Sarasota appears more expensive actually)
df = df_salary_latest %>%
    left_join(
        df_company %>% 
            select(Company, city = City, state = State, budget = `Annual Expenses / Budget`) %>%
            mutate(city = ifelse(Company == "Sarasota Ballet", "Tampa", city)),
        by = "Company"
    ) %>%
    left_join(df_index, by = c("city", "state"))


df_budget = df %>%
    group_by(Company) %>%
    summarise(budget = mean(budget), budget_std = scale(budget)) %>%
    ungroup()

df %>%
    mutate(company = fct_reorder(Company, budget)) %>%
    mutate(budget_std = scale(budget)[, 1]) %>%
    mutate(income_std = scale(`Income Total`)[, 1]) %>%
    ggplot(aes(x = company, y = budget_std)) +
    geom_boxplot(aes(y = income_std)) +
    geom_point(lty = 2, col = "blue", size = 3) +
    coord_flip() +
    theme_minimal()

df_plot = df %>%
    mutate(company = fct_reorder(Company, budget)) %>%
    group_by(company) %>%
    summarise(income = median(`Income Total`), budget = mean(budget)) %>%
    ungroup() %>%
    mutate(budget_scaled = scale(budget)) %>%
    mutate(income_scaled = scale(income)) %>%
    mutate(budget_std = budget_scaled + abs(min(budget_scaled))) %>%
    mutate(income_std = income_scaled + abs(min(income_scaled)))

summary(df_plot$income)
summary(df_plot$income * attr(df_plot$income, 'scaled:scale') + attr(df_plot$income , 'scaled:center'))
breaks = 0:4
0:4 * attr(df_plot$income, 'scaled:scale') + attr(df_plot$income , 'scaled:center')

breaks = c(10000, 30000, 50000, 70000, 90000)
breaks = (breaks - attr(df_plot$income_scaled , 'scaled:center')) / attr(df_plot$income_scaled, 'scaled:scale')

df_plot %>%
    ggplot(aes(x = company)) +
    geom_col(aes(y = budget_std), fill = "blue", width = .33, alpha = 0.33) +
    geom_point(aes(y = income_std)) +
    scale_y_continuous(name = "Median Salary", breaks = breaks, labels = function(x) (x + abs(min(df_plot$income_scaled))) * attr(df_plot$income_scaled, 'scaled:scale') + attr(df_plot$income_scaled, 'scaled:center')) +
    coord_flip() +
    theme_minimal()

    
    
# Plots

s_budget = df %>% filter(grepl("Sarasota", Company)) %>% pull(budget) %>% head(1)

df_group = df %>%
    distinct(Company, budget) %>%
    mutate(company = fct_reorder(Company, budget)) %>%
    mutate(group = abs(s_budget - budget) < 1000000)


# Company size
df_group %>%
    ggplot(aes(x = company, y = budget, fill = group)) +
    geom_col(width = 0.25) +
    xlab("") +
    scale_y_continuous(name = "Annual Budget", labels = scales::label_dollar(scale = 1/1000000, suffix = "M")) +
    scale_fill_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")

# Salary Range
df %>%
    group_by(Company) %>%
    summarise(income = median(`Income Total`), income_min = min(`Income Total`), income_max = max(`Income Total`)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, income)) %>%
    left_join(df_group %>% select(company, group), by = "company") %>%
    ggplot(aes(x = company, y = income, ymin = income_min, ymax = income_max, col = group, group = group)) +
    geom_errorbar(width = 0.5, size = 1) +
    xlab("") +
    scale_y_continuous(name = "Annual Income Range (All Ranks)", labels = scales::label_dollar(scale = 1/1000, suffix = "K")) +
    scale_color_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")


# Cost of living
df %>%
    group_by(Company) %>%
    summarise(index_cost_of_living = mean(index_cost_of_living)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, index_cost_of_living)) %>%
    left_join(df_group %>% select(company, group), by = "company") %>%
    ggplot(aes(x = company, y = index_cost_of_living, fill = group, group = group)) +
    geom_col(width = 0.25) +
    xlab("") +
    scale_y_continuous(name = "Cost of Living Index") +
    scale_fill_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")


# Rent
df %>%
    group_by(Company) %>%
    summarise(index_rent = mean(index_rent)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, index_rent)) %>%
    left_join(df_group %>% select(company, group), by = "company") %>%
    ggplot(aes(x = company, y = index_rent, fill = group, group = group)) +
    geom_col(width = 0.25) +
    xlab("") +
    scale_y_continuous(name = "Rent Index") +
    scale_fill_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")


# Local Purchasing Power
df %>%
    group_by(Company) %>%
    summarise(index_local_purchase_power = mean(index_local_purchase_power)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, index_local_purchase_power)) %>%
    left_join(df_group %>% select(company, group), by = "company") %>%
    ggplot(aes(x = company, y = index_local_purchase_power, fill = group, group = group)) +
    geom_col(width = 0.25) +
    xlab("") +
    scale_y_continuous(name = "Local Purchasing Power Index") +
    scale_fill_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")


# Groceries Index
df %>%
    group_by(Company) %>%
    summarise(index_groceries = mean(index_groceries)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, index_groceries)) %>%
    left_join(df_group %>% select(company, group), by = "company") %>%
    ggplot(aes(x = company, y = index_groceries, fill = group, group = group)) +
    geom_col(width = 0.25) +
    xlab("") +
    scale_y_continuous(name = "Grocery Price Index") +
    scale_fill_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")


# Restaurant Cost Index
df %>%
    group_by(Company) %>%
    summarise(index_restaurant = mean(index_restaurant)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, index_restaurant)) %>%
    left_join(df_group %>% select(company, group), by = "company") %>%
    ggplot(aes(x = company, y = index_restaurant, fill = group, group = group)) +
    geom_col(width = 0.25) +
    xlab("") +
    scale_y_continuous(name = "Restaurant Price Index") +
    scale_fill_manual(values = c("FALSE" = "dodgerblue2", "TRUE" = "darkorange2")) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")

color_values = c("FALSE" = "#6f69c2", "TRUE" = "#cf6436")

df_ranks = df %>%
    # filter(grepl(pattern = "sarasota|tulsa|milw", Company, TRUE)) %>%
    mutate(rank = case_when(
        grepl("apprentice|new dancer", Rank, TRUE) ~ "Apprentice",
        grepl("corps", Rank, TRUE) ~ "Corps",
        grepl("solo|coryp", Rank, TRUE) ~ "Soloist",
        grepl("princi", Rank, TRUE) ~ "Principal",
        TRUE ~ NA_character_
    )) %>%
    group_by(Company) %>%
    filter(!all(is.na(rank))) %>%
    filter(length(unique(rank)) > 2) %>%
    filter(!is.na(rank)) #%>%
    # count(rank, Rank)

df_rank_mean = df_ranks %>%
    group_by(rank) %>%
    summarise(income = median(`Income Total`)) %>%
    ungroup()

p = df_ranks %>%
    group_by(Company, rank) %>%
    summarise(income = mean(`Income Total`)) %>%
    ungroup() %>%
    mutate(company = fct_reorder(Company, income, min)) %>%
    mutate(s = grepl("Sarasota", Company)) %>%
    ggplot(aes(x = company, y = income, fill = s)) +
    geom_col(width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "Annual Income", labels = scales::label_dollar(scale = 1/1000, suffix = "K")) +
    facet_wrap(vars(rank)) +
    coord_flip() +
    scale_fill_manual(values = color_values) +
    theme(legend.position = "none")

ggplotly(p)


