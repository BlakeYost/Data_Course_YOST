library(tidyverse)
library(janitor)
library(broom)
library(easystats)

#1. Load and clean FacultySalaries_1995.csv file and Re-create the graph below…

df <- read_csv("FacultySalaries_1995.csv") %>% 
  janitor::clean_names()

df_salary <- df %>%
  gather(key = "salary_type", value = "avg_salary", -fed_id, -univ_name, -state, -tier,
         -avg_full_prof_comp, -avg_assoc_prof_comp, -avg_assist_prof_comp, -avg_prof_comp_all,
         -num_full_profs, -num_assoc_profs, -num_assist_profs, -num_instructors, -num_faculty_all)
df_salary <- separate(df_salary, salary_type, into = c("prof_type", "salary_type"), sep = "_")
df_salary$avg_salary <- as.numeric(df_salary$avg_salary)
df_salary <- 
  df_salary %>% 
  select(fed_id, univ_name, state, tier, salary_type, avg_salary, num_full_profs, num_assoc_profs, num_assist_profs, num_instructors, num_faculty_all)

df_comp <- df %>%
  gather(key = "comp_type", value = "avg_comp", -fed_id, -univ_name, -state, -tier,
         -avg_full_prof_salary, -avg_assoc_prof_salary, -avg_assist_prof_salary, -avg_prof_salary_all,
         -num_full_profs, -num_assoc_profs, -num_assist_profs, -num_instructors, -num_faculty_all)
df_comp <- separate(df_comp, comp_type, into = c("prof_type", "comp_type"), sep = "_")
df_comp$avg_comp <- as.numeric(df_comp$avg_comp)

df_comp <- 
  df_comp %>% 
  select(comp_type, avg_comp)

df1 <- cbind(df_salary, df_comp)

df1_filtered <- df1[df1$salary_type != "prof", ]
df1_filtered2 <- df1_filtered[df1_filtered$tier != "VIIB", ]
df1_filtered2 %>% 
  ggplot(aes(x=salary_type, y = avg_salary, fill = salary_type))+
  geom_boxplot()+
  facet_wrap(~tier)+
  theme_minimal()+
  labs(x = "Rank", y = "Salary", fill = "Rank")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
# 2. Build an ANOVA model and display the summary output in your report.

m1 <- glm(data=df1,
          formula = avg_salary ~ state + tier + salary_type)
summary(m1)

# 3. Load and tidy the "Juniper_Oils.csv" data. 

dat <- read_csv("Juniper_Oils.csv") %>% 
  janitor::clean_names()

dat1 <- dat %>%
  gather(key = "compound", value = "concentration", -sample_id, -project, -amplicon, -tree_species, -burn_year,
         -latitude, -longitude, -field_office, -blm_fire_name, -tracking_number, -yield_percent,
         -bolt_surface_area_cm2, -raw_exit_holes_per_cm2, -raw_exit_holes, -living_larvae, -chem_total,
         -chem_mean, -years_since_burn)

# 4. Make a graph with x = YearsSinceBurn, y = Concentration, facet = ChemicalID(use free y-axis scales)

dat1 %>% 
  ggplot(aes(x=years_since_burn, y=concentration))+
  geom_smooth()+
  facet_wrap(~compound, scales = "free")+
  labs(x="YearsSinceBurn", y="Concentration")

# 5. Use a glm to find which chemicals show concentrations that are significantly affected by "Years Since Burn"
m2 <- glm(data=dat1,
          formula = concentration ~ years_since_burn+compound)
summary(m2)

tidy_dat1 <- tidy(m2)

pvalue_tidy_dat1 <- tidy_dat1 %>%
  filter(p.value < 0.05)
