library(tidyverse)

forms <- read_csv("data/forms_ballet.csv")

forms_fields <- read_csv(file = "data/form_990_fields.csv", col_names = c("field_code", "field_description", "form_location"), col_types = cols(), skip = 3)

sum(forms$totfuncexpns)

forms %>%
    filter(totfuncexpns > 5000000) %>%
    arrange(desc(totfuncexpns)) %>%
    select(ein:state, tax_pd, totfuncexpns, totrevenue, totprgmrevnue, totcntrbgfts) 
    

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


# Revenue



# Functional Expenses
forms %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 10000000) %>%
    select(
        ein:tax_pd,
        total_functional_expense = totfuncexpns,
        grants_gov = grntstogovt,
        grants_ind = grnsttoindiv,
        grants_int = grntstofrgngovt,
        member_benefits = benifitsmembrs,
        comp_officers = compnsatncurrofcr,
        comp_dq = compnsatnandothr,
        salaries_other = othrsalwages,
        pension_contributions = pensionplancontrb,
        benefits_other = othremplyeebenef,
        tax_payroll = payrolltx,
        fees_management = feesforsrvcmgmt,
        fees_legal = legalfees,
        fees_accounting = accntingfees,
        fees_lobbying = feesforsrvclobby,
        fees_fundraising = profndraising,
        fees_investment = feesforsrvcinvstmgmt,
        fees_other = feesforsrvcothr,
        advertising = advrtpromo,
        expenses_office = officexpns,
        expenses_technology = infotech,
        expenses_royalties = royaltsexpns,
        expenses_occupancy = occupancy,
        expenses_travel = travel,
        expenses_travel_public_official = travelofpublicoffcl,
        expenses_conventions = converconventmtng,
        interest = interestamt,
        affiliates = pymtoaffiliates,
        depreciation = deprcatndepletn,
        insurance = insurance,
        contains("othrexp")
    ) %>%
    pivot_longer(c(grants_gov:insurance, contains("othrexp")), names_to = "expense", values_to = "cost") %>%
    group_by(ein, company_name) %>%
    mutate(total = sum(cost)) %>% 
    ungroup() %>%
    mutate(percent = round(cost / total * 100, 1)) %>%
    ggplot(aes(x = company_name, y = percent)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~expense, scales = "free")


# Total salary spend excl. officers
forms %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    arrange(othrsalwages) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = othrsalwages)) +
    geom_col(fill = "dodgerblue2", width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "Salary Spend", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal()


# Advertising & Promotion
forms %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    arrange(advrtpromo) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = advrtpromo)) +
    geom_col(fill = "dodgerblue2", width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "Salary Spend", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal()

forms %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    filter(advrtpromo > 0) %>%
    mutate(promo_per_rev = totrevenue / advrtpromo) %>%
    arrange(desc(promo_per_rev)) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = promo_per_rev)) +
    geom_col(fill = "dodgerblue2", width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "Salary Spend", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal()

forms %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    filter(advrtpromo > 0) %>%
    mutate(promo_per_rev = totprgmrevnue / advrtpromo) %>%
    arrange(desc(promo_per_rev)) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = promo_per_rev)) +
    geom_col(fill = "dodgerblue2", width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "Program revenue per dollar of advertising spend", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal()

forms %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    filter(advrtpromo > 0) %>%
    mutate(promo_per_rev = totrevenue / advrtpromo) %>%
    arrange(promo_per_rev) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = promo_per_rev)) +
    geom_col(fill = "dodgerblue2", width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "Revenue per dollar of advertising spend", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal()

forms %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    filter(advrtpromo > 0) %>%
    mutate(promo_per_rev = totrevenue / advrtpromo, contrib = totrevenue - totprgmrevnue, contribution_ratio = contrib / totrevenue) %>%
    arrange(desc(promo_per_rev)) %>%
    select(ein:state, totfuncexpns, totrevenue,  totprgmrevnue, contrib, advrtpromo, contribution_ratio, promo_per_rev)


# Percent Donation Revenue
forms %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    mutate(percent = round(totcntrbgfts / totrevenue * 100, 1)) %>%
    arrange(desc(percent)) %>%
    select(ein:state, totfuncexpns, totrevenue, totcntrbgfts, totprgmrevnue, percent) %>%
    print.data.frame() %>%
    arrange(percent) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = percent)) +
    geom_hline(yintercept = 50, alpha = 0.25) +
    geom_col(fill = "dodgerblue2", width = 0.67, alpha = 0.8) +
    geom_text(aes(label = paste0(round(percent), "%")), hjust = "right", nudge_y = -0.5, size = 3, color = "white") +
    xlab("") +
    scale_y_continuous(name = "Percent of revenue from contributions and grants", labels = scales::label_number(suffix = "%"), limits = c(0, 100)) +
    coord_flip() +
    theme_minimal() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )



# Salary/Headcount
forms %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    filter(totfuncexpns > 5000000) %>%
    # mutate(salary_per_headcount = (othrsalwages + compnsatncurrofcr + compnsatnandothr) / noemplyeesw3cnt) %>%
    mutate(salary_per_headcount = othrsalwages / noemplyeesw3cnt) %>%
    arrange(salary_per_headcount) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = salary_per_headcount)) +
    # geom_col(fill = "dodgerblue2", width = 0.33) +
    geom_segment(aes(xend = company_name, yend = 0), col = "dodgerblue2") +
    geom_point(col = "dodgerblue2", size = 2) +
    xlab("") +
    scale_y_continuous(name = "Salary Cost per Employee", labels = scales::label_dollar(scale = 10^-3, suffix = "K")) +
    coord_flip() +
    theme_minimal(base_size = 16) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )

# Functional Cost/Headcount
forms %>%
    filter(totfuncexpns > 5000000) %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup() %>%
    # mutate(salary_per_headcount = (othrsalwages + compnsatncurrofcr + compnsatnandothr) / noemplyeesw3cnt) %>%
    mutate(expense_per_headcount = totfuncexpns / noemplyeesw3cnt) %>%
    arrange(expense_per_headcount) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = expense_per_headcount)) +
    # geom_col(fill = "dodgerblue2", width = 0.33) +
    geom_segment(aes(xend = company_name, yend = 0), col = "dodgerblue2") +
    geom_point(col = "dodgerblue2", size = 2) +
    xlab("") +
    scale_y_continuous(name = "Functional Cost per Employee", labels = scales::label_dollar(scale = 10^-3, suffix = "K")) +
    coord_flip() +
    theme_minimal(base_size = 16) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )


# Sarasota
forms %>%
    filter(ein == "650135900") %>%
    filter(tax_pd == max(tax_pd)) %>%
    select(
        ein:tax_pd,
        total_functional_expense = totfuncexpns,
        grants_gov = grntstogovt,
        grants_ind = grnsttoindiv,
        grants_int = grntstofrgngovt,
        member_benefits = benifitsmembrs,
        comp_officers = compnsatncurrofcr,
        comp_dq = compnsatnandothr,
        salaries_other = othrsalwages,
        pension_contributions = pensionplancontrb,
        benefits_other = othremplyeebenef,
        tax_payroll = payrolltx,
        fees_management = feesforsrvcmgmt,
        fees_legal = legalfees,
        fees_accounting = accntingfees,
        fees_lobbying = feesforsrvclobby,
        fees_fundraising = profndraising,
        fees_investment = feesforsrvcinvstmgmt,
        fees_other = feesforsrvcothr,
        advertising = advrtpromo,
        expenses_office = officexpns,
        expenses_technology = infotech,
        expenses_royalties = royaltsexpns,
        expenses_occupancy = occupancy,
        expenses_travel = travel,
        expenses_travel_public_official = travelofpublicoffcl,
        expenses_conventions = converconventmtng,
        interest = interestamt,
        affiliates = pymtoaffiliates,
        depreciation = deprcatndepletn,
        insurance = insurance,
        contains("othrexp")
    ) %>%
    pivot_longer(c(grants_gov:insurance, contains("othrexp")), names_to = "expense", values_to = "cost") %>%
    group_by(ein, company_name) %>%
    mutate(total = sum(cost)) %>% 
    ungroup() %>%
    filter(cost > 0) %>%
    mutate(percent = round(cost / total * 100, 1)) %>%
    arrange(percent) %>%
    mutate(expense = factor(expense, levels = unique(expense))) %>%
    ggplot(aes(x = expense, y = cost)) +
    geom_col(fill = "dodgerblue2", alpha = 0.8, width = 0.33) +
    scale_y_continuous(name = "Cost per employee", labels = scales::label_dollar()) +
    coord_flip() +
    theme_minimal(base_size = 14)






df <- forms %>%
    filter(!grepl("endowment", company_name, TRUE)) %>%
    group_by(ein) %>%
    filter(tax_pd == max(tax_pd)) %>%
    ungroup()

# Headcount
df %>%
    filter(totfuncexpns > 5000000) %>%
    arrange(noemplyeesw3cnt) %>%
    mutate(company_name = factor(company_name, levels = unique(company_name))) %>%
    ggplot(aes(x = company_name, y = noemplyeesw3cnt)) +
    geom_col(fill = "dodgerblue2", width = 0.33) +
    xlab("") +
    scale_y_continuous(name = "") +
    coord_flip() +
    theme_minimal(base_size = 16) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )
    


