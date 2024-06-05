#Script to compute median CPUE reference all potential CCAMLR areas 

#Load CCAMLR catch and effort --------------------------------------------------

ccamlr_catch = read.csv('Data/CCAMLR_2023_catch.csv')
ccamlr_effort = read.csv('Data/CCAMLR_2023_effort.csv')
ccamlr_area = read.csv('Data/CCAMLR_2023_area.csv')
ccamlr_fishing_gear = read.csv('Data/CCAMLR_2023_fishing_gear.csv')

# join tables

df_ccamlr = ccamlr_effort %>% left_join(ccamlr_catch %>% dplyr::select(effort_id, taxon_code, greenweight_caught_tonne), by="effort_id")
df_ccamlr = df_ccamlr %>% left_join(ccamlr_area, by='asd_code')
df_ccamlr = df_ccamlr %>% left_join(ccamlr_fishing_gear, by='gear_type_code')


#CPUE by reference areas --------------------------------------------------
# CI 586 
# HIMI 5852

#get last 3 seasons
df_ccamlr_RefArea = df_ccamlr %>% filter(asd_code %in% c(586, 5852)) %>% 
  filter(year >= Est_Season-2) %>% 
  filter(taxon_code == 'TOP', gear_type_code=='LLS')



medCPUE_RefArea = df_ccamlr_RefArea %>% group_by(year, asd_code, gear_type_code) %>% 
  dplyr::summarise(catch=sum(greenweight_caught_tonne), effort=sum(hook_count), haul=sum(haul_count)) %>% 
  dplyr::mutate(CPUE = (catch*1000/effort)*1000)

df_ccamlr_RefArea %>% mutate(CPUE= ((1000*greenweight_caught_tonne)/hook_count)) %>% 
  group_by(asd_code) %>% dplyr::summarise(medCPUE= median(CPUE)) #  cvCPUE= cv(CPUE)/100
