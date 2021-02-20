
# https://curious-joe.net/post/2021-02-13-the-race-to-inequality-in-the-us/
# an animated graph that will show you how America’s
# races have historically been different in their student
# debt accumulation and how it’s getting worse increasingly.



# libraries
# devtools::install_github("thomasp85/transformr")
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gganimate) 
library(extrafont)
library(knitr)
library(kableExtra)

# data
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')

# glimpse of the data
student_debt %>%
  head(5) %>%
  knitr::kable(caption = "Top 5 lines of the data") %>%
  kable_classic(full_width = F, html_font = "Cambria")

###########################################
student_debt %>%
  mutate(race = fct_relevel(race, "Black", "White", "Hispanic")) %>%
  ggplot(
    aes(x = year, 
        y=loan_debt, 
        color = race)
  ) +
  
  geom_line(aes(),
            size = 1,
            linetype = "dashed") +
  
  geom_point(aes(
    size = loan_debt_pct,
    group = seq_along(year)),
    show.legend = FALSE) +
  
  geom_text(aes(
    label = ifelse(year >= 2010, 
                   paste0(round(loan_debt_pct * 100), "%"),
                   ""),
    group = seq_along(year)
  ), 
  show.legend = FALSE,
  size = 4,
  hjust = 1, vjust = 0) + 
  
  #theme_race() + 
  
  transition_reveal(as.integer(year)) + # as.integer(year) makes the year showing in subtitle as integer.
  
  scale_x_continuous(breaks = seq(1989, 2016, 3)) + 
  scale_y_continuous(breaks = seq(500, 15000, 1500), 
                     labels = scales::dollar) +
  scale_color_manual(values = c("White" = "#ffffff", "Black" = "#787575", "Hispanic" = "#f5bf42")) + 
  
  labs(title="Average Student Loan Taken by the US Families",
       x = NULL, color = NULL,
       y = "Average Loan Debt \n($ normalized to 2016 dollars)", 
       caption = "Source: Urban Institute, and the US Census,\n2017 | Arafath Hossain",
       subtitle ="Point sizes represent % of families with student loans \nYear: {frame_along}") -> plot


plot %>% 
  animate(fps = 10,
          end_pause = 12,
          height = 6,
          width = 10,
          units = "in",
          res = 150)

#   1   ########################################################

student_debt %>%
  ggplot(
    aes(x = year, 
        y=loan_debt, 
        group = race)
  ) +
  geom_line()

#     2  #######################################

student_debt %>%
  ggplot(
    aes(x = year, 
        y=loan_debt, 
        group = race)
  ) +
  geom_line(aes(color = race)) +
  scale_x_continuous(breaks = seq(1989, 2016, 3)) + 
  scale_y_continuous(breaks = seq(500, 15000, 1500), 
                     labels = scales::dollar) +
  labs(title="Average Family Student Loan",
       x = NULL, color = NULL, y = "Average Loan Debt")


#      3      #######################################

student_debt %>%
  ggplot(
    aes(x = year, 
        y=loan_debt, 
        group = race)
  ) +
  
  geom_line(aes(color = race)) +
  
  geom_point(aes(
    size = loan_debt_pct)
  ) +
  
  
  scale_x_continuous(breaks = seq(1989, 2016, 3)) + 
  scale_y_continuous(breaks = seq(500, 15000, 1500), 
                     labels = scales::dollar) +
  labs(title="Average Family Student Loan",
       color = NULL,
       x = NULL, y = "Average Loan Debt",
       subtitle ="Point sizes represent % of families with student loans")

#      4    ############################################
student_debt %>%
  mutate(race = fct_relevel(race, "Black", "White", "Hispanic")) %>%
  ggplot(
    aes(x = year, 
        y=loan_debt, 
        group = race,
        color = race)
  ) +
  
  geom_line(aes()) +
  
  geom_point(aes(
    size = loan_debt_pct),
    show.legend = FALSE) +
  
  geom_text(aes(
    label = paste0(round(loan_debt_pct * 100, 2), "%")
  ), 
  show.legend = FALSE,
  hjust = 1, vjust = 0) + 
  
  scale_x_continuous(breaks = seq(1989, 2016, 3)) + 
  scale_y_continuous(breaks = seq(500, 15000, 1500), 
                     labels = scales::dollar) +
  scale_color_manual(values = c("White" = "#ffffff", "Black" = "#787575", "Hispanic" = "#f5bf42")) + 
  
  labs(title="Average Family Student Loan",
       color = NULL,
       x = NULL, y = "Average Loan Debt",
       subtitle ="Point sizes represent % of families with student loans")


##    5   ###############################################


student_debt %>%
  mutate(race = fct_relevel(race, "Black", "White", "Hispanic")) %>%
  ggplot(
    aes(x = year, 
        y=loan_debt, 
        group = race,
        color = race)
  ) +
  
  geom_line(aes(),
            size = 1,
            linetype = "dashed") +
  
  geom_point(aes(
    size = loan_debt_pct),
    show.legend = FALSE) +
  
  geom_text(aes(
    label = ifelse(year >= 2010, 
                   paste0(round(loan_debt_pct * 100), "%"),
                   "")
  ), 
  show.legend = FALSE,
  size = 4,
  hjust = 1, vjust = 0) + 
  
  theme_dark() + 
  
  scale_x_continuous(breaks = seq(1989, 2016, 3)) + 
  scale_y_continuous(breaks = seq(500, 15000, 1500), 
                     labels = scales::dollar) +
  scale_color_manual(values = c("White" = "#ffffff", "Black" = "#787575", "Hispanic" = "#f5bf42")) + 
  
  labs(title="Average Family Student Loan",
       color = NULL,
       x = NULL, y = "Average Loan Debt",
       subtitle ="Point sizes represent % of families with student loans")


##    6   #########################################################

