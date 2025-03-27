if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}

library(readr)
nyc_payroll <- read_csv("data/mp01/nyc_payroll_export.csv")

glimpse(nyc_payroll)

library(dplyr)
library(stringr)

nyc_payroll <- nyc_payroll %>%
  mutate(
    agency_name = str_to_title(agency_name),
    last_name = str_to_title(last_name),
    first_name = str_to_title(first_name),
    work_location_borough = str_to_title(work_location_borough),
    title_description = str_to_title(title_description),
    leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
  )

carrer_adams <- nyc_payroll |> 
  filter(last_name == "Adams" & first_name == "Eric" & mid_init == "L") |>
  arrange(fiscal_year) |>
  summarize(fiscal_year,title_description,agency_name,base_salary)

carrer_adams <- carrer_adams %>%
  mutate(base_salary = ifelse(fiscal_year == "2022"&title_description=="Borough President", 
                              179200/2, base_salary)) |>
  mutate(base_salary = ifelse(fiscal_year == "2022"&title_description=="Mayor", 
                              258750/2, base_salary))


carrer_adams <- carrer_adams |>
  rename(
    'Fiscal Year' = fiscal_year,
    Position = title_description,
    Agency = agency_name,
    'Total Salary' = base_salary
  )

carrer_adams
glimpse(carrer_adams)

install.packages("DT")
library(DT)
library(scales)


carrer_adams |> 
  mutate(`Total Salary` = dollar(`Total Salary`)) |>
  datatable(options=list(searching=FALSE, 
                         paging=FALSE,
                         info=FALSE))

```{r}
#| echo: false
#| message: false
#| warning: false
#| code-fold: true
#| code-summary: "Show the code"
# Set a CRAN mirror (e.g., RStudio's default mirror)
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Now install the package
install.packages("DT")
```

unique(nyc_payroll$pay_basis)


total_compensation <- nyc_payroll |>
  mutate(
    total_compensation = case_when(
      pay_basis == "per Annum" ~ base_salary, 
      pay_basis == "per Hour" ~ base_salary * (regular_hours+1.5* ot_hours),
      pay_basis == "per Day" ~ base_salary * (regular_hours+ot_hours)/7.5,
      TRUE ~ NA_real_  
    )
  )

#Which job title has the highest base rate of pay?
total_compensation |> mutate(
  base_rate = case_when(
    pay_basis == "per Annum" ~ base_salary, 
    pay_basis == "per Hour" ~ base_salary * 2000,
    pay_basis == "per Day" ~ base_salary * 2000/7.5,
    TRUE ~ NA_real_  
  )
) |> group_by(title_description) |> summarize(mean_rate=mean(base_rate)) |>
  arrange(desc(mean_rate))

#Which individual & in what year had the single highest city total payroll
total_compensation |> 
  summarize(fiscal_year,first_name,last_name,total_compensation) |>
  arrange(desc(total_compensation))

#Which individual worked the most overtime hours in this data set?
total_compensation |> summarize(fiscal_year,first_name,last_name,ot_hours) |>
  arrange(desc(ot_hours))

#Which agency has the highest average total annual payroll 
#(base and overtime pay per employee)?
total_compensation |> group_by(agency_name) |>
  summarize(avg_tot_pay=mean(total_compensation)) |>
  arrange(desc(avg_tot_pay))

#Which agency has the most employees on payroll in each year?
total_compensation |>
  group_by(fiscal_year, agency_name) |>
  summarize(num_empl=n(),.groups="drop") |>
  group_by(fiscal_year) |>
  slice_max(num_empl,n=1)

#Which agency has the highest overtime usage (compared to regular hours)?
total_compensation |>
  group_by(agency_name) |>
  summarize(ot_usage=mean(ot_hours)/mean(regular_hours)) |>
  arrange(desc(ot_usage))

#What is the average salary of employees who work outside the five boroughs?
# (That is, whose work_location_borough is not one of the five counties.)
total_compensation |>
  filter(!work_location_borough %in% c("Manhattan", "Brooklyn", "Queens", 
                                     "Bronx", "Richmond")) |>
  summarize(avg_salary=mean(total_compensation,na.rm=TRUE))

unique(total_compensation$work_location_borough)

#How much has the city’s aggregate payroll grown over the past 10 years?
growth <- total_compensation |>
  group_by(fiscal_year) |>
  summarize(agg_pay=mean(total_compensation,na.rm=TRUE))

agg_pay_2024 <- growth |> filter(fiscal_year=="2024") |>
  pull(agg_pay)

agg_pay_2014 <- growth |> filter(fiscal_year=="2014") |>
  pull(agg_pay)

growth_amount <- agg_pay_2024 - agg_pay_2014
growth_percentage <- growth_amount/agg_pay_2014*100
growth_percentage

#Compute the total mayor pay for each fiscal year and identify employees 
#who made more than this amount in the same fiscal year.
mayor_pay_per_year <- total_compensation |>
  filter(title_description == "Mayor") |>
  group_by(fiscal_year) |>
  summarize(mayor_pay = mean(total_compensation))

employees_above_mayor <- total_compensation |>
  left_join(mayor_pay_per_year, by = "fiscal_year") |>
  filter(total_compensation > mayor_pay)

#Determine total savings if these employees’ compensation were capped at 
#the mayor’s salary.
employees_above_mayor <- employees_above_mayor |>
  mutate(savings = total_compensation - mayor_pay)
total_savings <- sum(employees_above_mayor$savings)
            
#Identify which agencies and job titles (if any) would bear the brunt of 
#this policy.
savings_by_agency_and_title <- employees_above_mayor |>
  filter(fiscal_year>"2014") |>
  group_by(agency_name,title_description ) |>
  summarize(avg_savings = mean(savings)) |>
  arrange(desc(avg_savings))

#For each combination of agency and job title, identify the total number of 
#overtime hours worked and see how many full-time employees it would take to 
#replace that much overtime.
overtime_by_agency_and_title <- total_compensation |>
  group_by(agency_name , title_description) |>
  summarize(total_overtime_hours = sum(ot_hours)) |>
  arrange(desc(total_overtime_hours))

overtime_with_ft_equivalents <- overtime_by_agency_and_title |>
  mutate(full_time_equivalents = total_overtime_hours / 2000)

#For each combination of agency and job title, calculate the total savings 
#possible by converting all overtime hours to regular time hours for 
#(new employees).
base_rate <- total_compensation |> mutate(
  base_rate = case_when(
    pay_basis == "per Annum" ~ base_salary, 
    pay_basis == "per Hour" ~ base_salary * 2000,
    pay_basis == "per Day" ~ base_salary * 2000/7.5,
    TRUE ~ NA_real_  
  )
)

overtime_by_agency_and_title <- base_rate |>
  group_by(agency_name, title_description) |>
  summarize(total_overtime_hours = sum(ot_hours), 
            avg_regular_salary = mean(base_rate)) |>
  arrange(desc(total_overtime_hours)) 

overtime_by_agency_and_title <- overtime_by_agency_and_title |>
  mutate(overtime_cost = total_overtime_hours * 1.5 * avg_regular_salary / 2000)
  
savings_by_agency_and_title <- overtime_by_agency_and_title |>
  mutate(savings = overtime_cost - (total_overtime_hours * avg_regular_salary / 2000)) 

#Determine the aggregate savings possible by agency. This will let the CATS 
#Commission recommend the agencies where this hiring action would have the 
#largest benefit.
aggregate_savings_by_agency <- savings_by_agency_and_title |>
  group_by(agency_name) |>
  summarize(total_savings = sum(savings)) |>
  arrange(desc(total_savings))




