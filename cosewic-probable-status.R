library(tidyverse)

# taken from: https://github.com/Pacific-salmon-assess/COSEWIC-prioritize/blob/main/probable-status.Rmd

# read in data ----
raw_sp_data <- read.csv("./data/CU_Spawner_Abund_20220812.csv", 
                        header=TRUE) %>%
  mutate(logSp=log(Spawner.Abundance))

cu_metadata <- read.csv("./data/CU_Metadata_20220812.csv",  
                        header=TRUE, na.strings=c("", "NA")) %>%
  filter(!is.na(gen_length)) %>%
  rename(CUID=cuid)

# join, dump pops with NA DUs ----
sp_data <- left_join(raw_sp_data, select(cu_metadata, Region, CUID, du_number, gen_length, Sites, cu_enh_rank, COSEWIC_status, spp), 
                     by="CUID") %>%
  drop_na(du_number, Spawner.Abundance) %>%
  mutate(Species = spp) %>%
  group_by(du_number, Region.y, Year, gen_length, Sites, cu_enh_rank, COSEWIC_status, Species) %>%
  rename(Region = Region.y) %>%
  summarise(mat_ind = sum(Spawner.Abundance))%>%
  arrange(du_number, Year)

# filter to DUs with spawner estimates in at least 50% of last three generations (and 20% of all years) ----
last3_gens <- sp_data %>%
  group_by(du_number) %>%
  mutate(three_gens = gen_length*3) %>%
  filter(Year > (last(Year) - three_gens), Year <= last(Year))

deficient_recent_perc <- 0.3

deficient_recent <- last3_gens %>%
  group_by(du_number) %>%
  summarise(n = n(), #years with data  
            n_proper = mean(gen_length)*3) %>% # years for 3 gens of data if fully sampled
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_recent_perc) %>%
  pull(du_number)

last3_gens_filtered <- filter(last3_gens, !(du_number %in% deficient_recent))

deficient_historical_perc <- 0.2

deficient_historical <- sp_data %>%
  group_by(du_number) %>%
  summarise(n = n(),
            n_proper = last(Year) - first(Year) + 1) %>%
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_historical_perc) %>%
  pull(du_number)

sp_data_filtered <- filter(sp_data, !(du_number %in% deficient_historical))

# estimates rate of change using simple lm ----
summary_table <- NULL

for(i in unique(sp_data_filtered$du_number)){
  sub_raw <- filter(sp_data, du_number==i)
  sub_data <- filter(sp_data_filtered, du_number==i)
  sub_last3 <- filter(last3_gens_filtered, du_number==i)
  if(nrow(sub_data)==0){
    slope_all <- NA
  } else{
    if(nrow(sub_last3)==0){
      slope_recent <- NA
    } else{
      model_all <- lm(log(sub_data$mat_ind)~sub_data$Year)
      model_recent <- lm(log(sub_last3$mat_ind)~sub_last3$Year)
      per_change_all <- round(as.numeric((exp(model_all$coefficients[2]*(3*unique(sub_data$gen_length)))-1)*100))
      per_change_recent <- round(as.numeric((exp(model_recent$coefficients[2]*(3*unique(sub_data$gen_length)))-1)*100))
    }
  }
  recent_abundance <- last(sub_raw$mat_ind)
  last_year_monitored <- last(sub_raw$Year)
  
  output <- data.frame(du_number=i, Region = unique(sub_raw$Region), per_change_recent, per_change_all, recent_abundance, last_year_monitored)
  
  summary_table <- bind_rows(output, summary_table)
}

# assess probable designation, rename a couple of regions ----
prob_desg <- summary_table %>%
  mutate(prob_designation_recent = case_when(
    per_change_recent  > -29 ~ "Not at risk",
    per_change_recent < -29 & per_change_recent > -49 ~ "Special concern",
    per_change_recent < -49 & per_change_recent > -69 ~ "Threatened",
    per_change_recent < -69 ~ "Endangered")) %>%
  mutate(prob_designation_all = case_when(
    per_change_all  > -29 ~ "Not at risk",
    per_change_all < -29 & per_change_all > -49 ~ "Special concern",
    per_change_all < -49 & per_change_all > -69 ~ "Threatened",
    per_change_all < -69 ~ "Endangered"))

master_desg <- left_join(cu_metadata, select(prob_desg, Region, du_number, per_change_recent, per_change_all, recent_abundance, last_year_monitored, prob_designation_recent,prob_designation_all), 
                     by="du_number") %>%
  mutate(COSEWIC_status = ifelse(is.na(COSEWIC_status), "Not assessed", COSEWIC_status)) %>%
  rename(Region = Region.x) %>%
  mutate(region_full = case_when(
    Region == "HG" ~ "Haida Gwaii",
    Region == "CC" ~ "Central Coast",
    Region == "VIMI" ~ "Van. Isl. Main. Inl.",
    TRUE ~ Region))

# wrangle assessed and un-assessed status'
assessed <- master_desg %>%
  filter(COSEWIC_status != "Not assessed")

not_assessed <- master_desg %>%
  filter(COSEWIC_status == "Not assessed") %>%
  mutate(prob_designation_recent = ifelse(is.na(prob_designation_recent), "Data defficient", 
                                          prob_designation_recent),
         prob_designation_all = ifelse(is.na(prob_designation_all), "Data defficient", 
                                          prob_designation_all))

designations <- rbind(assessed,not_assessed)%>%
  mutate(species = spp) %>%
  select(region_full, du_number,species, gen_length, COSEWIC_status,per_change_recent, per_change_all, recent_abundance, last_year_monitored, prob_designation_recent)%>%
  arrange(du_number)%>%
  distinct(du_number,.keep_all = TRUE)

unassessed <- designations %>%
  filter(COSEWIC_status == "Not assessed",
         prob_designation_recent != "Data defficient")

#write.csv(designations, "./output/master-prob-status/master_status.csv")

NA_regions <- filter(designations, is.na(region_full))


# Status summaries 
## All DUs - plot

for(i in 1:length(designations$du_number)){
  if(designations$COSEWIC_status[i] == "Not assessed") {designations$status[i] <- designations$prob_designation_recent[i]}else{designations$status[i] <- designations$COSEWIC_status[i]}
  if(designations$status[i] == "Status assessment recommended"){designations$status[i] <- designations$prob_designation_recent[i]}
}

count_bins_status <- designations %>%
  group_by(species, region_full, status) %>%
  summarize(du_count=n()) %>%
  mutate(freq = du_count / sum(du_count)) %>%
  mutate(status = factor(status, levels = rev(c("Extinct","Endangered","Threatened", "Special concern", "Not at risk", "Data defficient"))), 
         region_full = factor(region_full, levels = c("Yukon", "Nass","Skeena", "Haida Gwaii", "Central Coast", "Van. Isl. Main. Inl.", "Fraser"))) %>%
  drop_na(status, region_full)
  
ggplot(count_bins_status %>% filter(region_full== "Central Coast", species != "Sockeye-River"), aes(x = species, y = freq)) + 
  geom_col(aes(fill=status)) +
  scale_fill_manual(values = c("grey","dark green", "yellow", "orange","red","black")) +
  xlab("Species") +
  ylab("Proportion of Designatable Units") +
  labs(fill = "COSEWIC extinction risk") +
  theme_bw()+
  theme_sleek()

ggsave("figs/all-cosewic.jpeg", width = 7, height = 4, units = "in")

