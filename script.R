## Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("giocomai/latlon2map")
pacman::p_load(tidyverse, 
               sf, 
               here)


## geometries
lau <- ll_get_lau_eu() %>% 
  janitor::clean_names() %>% 
  #st_set_geometry(NULL) %>% 
  select(cntr_code, gisco_id, lau_name) 

brussels <- ll_get_lau_eu("BE100") %>% 
  janitor::clean_names() %>% 
  select(cntr_code, nuts_id, nuts_name) %>% 
  rename("gisco_id" = "nuts_id") %>% 
  rename("lau_name" = "nuts_name")  

lisbon <- ll_get_lau_eu("PT_Q597") %>% 
  janitor::clean_names() %>% 
  mutate(cntr_code = substr(gisco_id, 1, 2)) %>% 
  rename("lau_name" = "concelho") %>% 
  select(cntr_code, gisco_id, lau_name) 

kiev <- ll_get_lau_eu("UA_UKR.11_1") %>% 
  janitor::clean_names() %>% 
  mutate(cntr_code = country) %>%  
  mutate(lau_name = varname_1) %>% 
  mutate(gisco_id = paste0("UA_", gid_1)) %>% 
  select(cntr_code, gisco_id, lau_name)  

chisinau <- ll_get_lau_eu("MD010") %>% 
  janitor::clean_names() %>% 
  mutate(cntr_code = adm0_pcode) %>%
  mutate(lau_name = adm1_en) %>% 
  mutate(gisco_id = GISCO_ID) %>% 
  select(cntr_code, gisco_id, lau_name)  

all_lau <- lau %>% 
  rbind(chisinau) %>% 
  rbind(kiev) %>% 
  rbind(lisbon) %>% 
  rbind(brussels) 


nuts_0 <- latlon2map::ll_get_nuts_eu(level = 0, resolution = 60) %>% 
  st_set_geometry(NULL) %>% 
  janitor::clean_names() %>% 
  mutate(cntr_code = nuts_id,
         cntr_name = name_latn) %>% 
  select(cntr_code, cntr_name) 

nuts_2 <- latlon2map::ll_get_nuts_eu(level = 2, resolution = 1) %>% 
  janitor::clean_names() %>% 
  select(cntr_code, nuts_id, name_latn) %>% 
  left_join(nuts_0) %>% 
  select(1,5,2,3) 

world <- latlon2map::ll_get_world(resolution = 1) %>% 
  janitor::clean_names() %>% 
  mutate(cntr_code = cntr_id,
         cntr_name = name_engl) %>% 
  select(cntr_code, cntr_name)  

eu_countries <- unique(nuts_0$cntr_code)
europe_cntr <- append(eu_countries, c("RU", "UA", "MD", "BA", "BY", "UK", "XK", "AM", "GE", "AZ"))


sf::sf_use_s2(FALSE)


## Data
lau_names <- all_lau %>% 
  st_set_geometry(NULL) %>% 
  select(2,3)

id_matcher <- list.files(here("data_geojson"), full.names = F) %>% 
  as_tibble() %>%  
  mutate(src = as.character(seq(1, length(.$value))),
         gisco_id = str_remove(value, pattern = "\\-.*")) %>% 
  select(-1) %>% 
  left_join(lau_names) %>% 
  view()

data_geo <- list.files("data_geojson", pattern = "*.geojson", full.names = TRUE) %>% 
  map(., function(f) st_read(f)) %>% 
  map_df(I, .id = "src") %>% 
  select(-gisco_id) %>% 
  left_join(id_matcher) %>% 
  mutate(country = substr(gisco_id, 1, 2)) %>% 
  mutate(id = row_number())  

data <- data_geo %>% st_set_geometry(NULL)

#total_streets 
total_streets <- data %>% 
  select(gisco_id, lau_name, street_name) %>% 
  distinct() %>% 
  group_by(gisco_id, lau_name) %>% 
  count() %>% 
  rename("total_streets" = "n") %>% 
  view()


#persons
person_streets <- data %>% 
  filter(person == 1) %>% 
  select(gisco_id, lau_name, street_name) %>% 
  distinct() %>% 
  group_by(gisco_id, lau_name) %>% 
  count() %>% 
  rename("person_streets" = "n")  

#others
mixed_streets <- data %>% 
  filter(person == 1) %>% #le persone
  select(gisco_id, lau_name, street_name, gender) %>% 
  distinct() %>% #elimino strade dedicate a più persone dello stesso genere
  group_by(gisco_id, lau_name, street_name) %>% 
  count() %>%  
  filter(n > 1) %>% 
  select(gisco_id, lau_name, street_name) 


other_streets <- data %>% 
  filter(person == 1) %>%
  filter(!gender %in% c("male", "female")) %>% 
  group_by(gisco_id, lau_name, street_name, gender) %>% 
  count() %>% 
  ungroup() %>% 
  select(gisco_id, lau_name, street_name) 


mixed_others <- mixed_streets %>% 
  rbind(other_streets) %>% 
  distinct() %>% 
  group_by(gisco_id, lau_name) %>% 
  count() %>% 
  rename("mixed_uncertain_streets" = "n") 


#females
female_streets <- data %>% 
  filter(person == 1) %>% 
  filter(gender == "female") %>% 
  select(gisco_id, lau_name, street_name) %>% 
  anti_join(mixed_others_names) %>%
  distinct() %>% 
  group_by(gisco_id, lau_name) %>% 
  count() %>% 
  rename("female_streets" = "n") 


#males
male_streets <- data %>% 
  filter(person == 1) %>% 
  filter(gender == "male") %>% 
  select(gisco_id, lau_name, street_name) %>% 
  anti_join(mixed_others_names) %>%
  distinct() %>% 
  group_by(gisco_id, lau_name) %>% 
  count() %>% 
  rename("male_streets" = "n") 


#distinct individuals
all_genders_nodub_wiki <- data %>% 
  filter(person == 1) %>%  
  drop_na(named_after_id) %>%  
  select(gisco_id, lau_name, named_after_id, gender) %>% 
  distinct() %>% 
  group_by(gisco_id, lau_name, gender) %>% 
  count() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 


all_genders_nodub_nowiki <- data %>% 
  filter(person == 1) %>%  
  filter(is.na(named_after_id)) %>%  
  select(gisco_id, lau_name, street_name, gender) %>% 
  distinct() %>% 
  group_by(gisco_id, lau_name, gender) %>% 
  count() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 


all_distinct_individuals <- all_genders_nodub_wiki %>% 
  left_join(all_genders_nodub_nowiki, 
            by = c("gisco_id", "lau_name"),
            suffix = c("_wiki", "_no_wiki")) %>%  
  mutate(total_unique_individuals = rowSums(across(where(is.numeric))))  


final_gender <- total_streets %>% 
  left_join(person_streets) %>% 
  left_join(male_streets) %>% 
  left_join(female_streets) %>% 
  left_join(mixed_others) %>% 
  left_join(all_distinct_individuals)  


##Top individuals in cities

humans_idlab <- data %>% 
  filter(person == 1) %>% 
  drop_na(named_after_id) %>% 
  select(named_after_id, label, gender) %>% 
  unique()  

top_individuals <- humans %>% 
  group_by(named_after_id) %>% 
  count() %>% 
  drop_na(named_after_id) %>% 
  left_join(humans_idlab) %>% 
  ungroup() %>% 
  select(1,3,4,2) %>% 
  arrange(-n) 

top_h <- unique(top_individuals$named_after_id)

city_count <- humans %>% 
  select(named_after_id, lau_name) %>% 
  filter(named_after_id %in% top_h) %>% 
  distinct() %>% 
  group_by(named_after_id) %>% 
  count() %>% 
  rename("city_number" = "n") 

individuals_cities <- humans %>% 
  filter(named_after_id %in% top_h) %>% 
  select(named_after_id, lau_name) %>% 
  unique() %>% 
  group_by(named_after_id) %>% 
  mutate(cities = paste0(lau_name, collapse = "|")) %>% 
  select(-2) %>% 
  left_join(city_count) %>% 
  distinct() 


bplace_1 <- humans %>% 
  filter(named_after_id %in% top_h) %>% 
  select(named_after_id, country_of_citizenship_label, place_of_birth_latitude, place_of_birth_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_birth_longitude) %>%
  st_as_sf(coords = c("place_of_birth_longitude", "place_of_birth_latitude"), crs = 4326) %>% 
  st_join(nuts_2) %>% 
  st_set_geometry(NULL)  

antjnr <- bplace_1 %>% 
  filter(is.na(cntr_code))

bplace_2 <- humans %>% 
  filter(named_after_id %in% antjnr$named_after_id) %>% 
  select(named_after_id, country_of_citizenship_label, place_of_birth_latitude, place_of_birth_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_birth_longitude) %>%
  st_as_sf(coords = c("place_of_birth_longitude", "place_of_birth_latitude"), crs = 4326) %>% 
  st_join(world) %>% 
  st_set_geometry(NULL) %>% 
  add_column(nuts_id = NA,
             name_latn = NA)  


miss_bpl <- bplace_2 %>% 
  filter(is.na(cntr_code))  

bplace_3 <- humans %>% 
  filter(named_after_id %in% miss_bpl$named_after_id) %>% 
  select(named_after_id, country_of_citizenship_label, place_of_birth_latitude, place_of_birth_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_birth_longitude) %>%
  st_as_sf(coords = c("place_of_birth_longitude", "place_of_birth_latitude"), crs = 4326) %>% 
  st_join(nuts_2, st_is_within_distance, dist = 1500) %>% 
  st_set_geometry(NULL) %>% 
  view()


miss_bpl_lst <- bplace_3 %>% 
  filter(is.na(cntr_code))  


bplace_4 <- humans %>% 
  filter(named_after_id %in% miss_bpl_lst$named_after_id) %>% 
  select(named_after_id, country_of_citizenship_label, place_of_birth_latitude, place_of_birth_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_birth_longitude) %>%
  st_as_sf(coords = c("place_of_birth_longitude", "place_of_birth_latitude"), crs = 4326) %>% 
  st_join(world, st_is_within_distance, dist = 1500) %>% 
  st_set_geometry(NULL) %>% 
  add_column(nuts_id = NA,
             name_latn = NA) 


bplace <- bplace_1 %>% 
  drop_na(cntr_code) %>% 
  rbind(bplace_2 %>% drop_na(cntr_code)) %>% 
  rbind(bplace_3 %>% drop_na(cntr_code)) %>% 
  rbind(bplace_4)



individuals_cities_countries <- humans %>% 
  filter(named_after_id %in% top_h) %>% 
  select(named_after_id, country, lau_name) %>% 
  unique() %>%  
  left_join(bplace %>% select(named_after_id, cntr_code)) %>% 
  mutate(cntr_code = replace_na(cntr_code, "NAAA")) %>% 
  filter(country != cntr_code) %>% 
  group_by(named_after_id) %>% 
  count() %>% 
  rename("foreign_cities" = "n") 


top_individual_cities <- top_individuals %>% 
  left_join(individuals_cities) %>% 
  left_join(bplace) %>% 
  left_join(individuals_cities_countries) %>% 
  distinct()  


matcher_moreinfo <- humans %>% 
  filter(named_after_id %in% top_individual_cities$named_after_id) %>% 
  select(named_after_id, date_of_birth, date_of_death, description, occupation_label, occupation_category) %>% 
  distinct()  


individuals_all <- top_individual_cities %>% 
  left_join(matcher_moreinfo)  


coords <- humans %>% 
  select(named_after_id, place_of_birth_latitude, place_of_birth_longitude) %>% 
  drop_na(place_of_birth_latitude) 


individuals_all_def <- individuals_all %>% 
  left_join(coords) %>% 
  select(label,
         named_after_id,
         description,
         occupation_label,
         occupation_category,
         date_of_birth,
         date_of_death,
         place_of_birth_latitude,
         place_of_birth_longitude,
         country_of_citizenship_label,
         cntr_code,
         cntr_name,
         nuts_id,
         name_latn,
         n,
         city_number,
         foreign_cities,
         cities) 

top_males <- all_individuals_famous %>% 
  filter(gender == "male") %>% 
  select(-gender) %>% 
  slice(1:100) 

top_females <- all_individuals_famous %>% 
  filter(gender == "female") %>% 
  select(-gender) %>% 
  slice(1:100)  


## Death abd birth places

coordinate_data <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id, place_of_birth_latitude, place_of_birth_longitude) %>% 
  distinct() %>%  
  drop_na(place_of_birth_latitude) %>% 
  group_by(gisco_id, lau_name) %>% 
  count(name = "birthplace_data")  


born_city <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id, place_of_birth_latitude, place_of_birth_longitude) %>% 
  distinct() %>%  
  drop_na(place_of_birth_latitude) %>% 
  st_as_sf(coords = c("place_of_birth_longitude", "place_of_birth_latitude"), crs = 4326) %>% 
  st_join(all_lau, suffix = c("", "_y")) %>% 
  st_set_geometry(NULL) %>% 
  mutate(born_city = case_when(gisco_id == gisco_id_y  ~ "yes",
                               gisco_id != gisco_id_y  ~ "no")) %>% 
  filter(born_city == "yes") %>% 
  group_by(gisco_id, lau_name) %>% 
  count(name = "born_city") 


died_city <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id, place_of_death_latitude, place_of_death_longitude) %>% 
  distinct() %>%  
  drop_na(place_of_death_latitude) %>% 
  st_as_sf(coords = c("place_of_death_longitude", "place_of_death_latitude"), crs = 4326) %>% 
  st_join(all_lau, suffix = c("", "_y")) %>% 
  st_set_geometry(NULL) %>% 
  mutate(died_city = case_when(gisco_id == gisco_id_y  ~ "yes",
                               gisco_id != gisco_id_y  ~ "no")) %>% 
  filter(died_city == "yes") %>% 
  group_by(gisco_id, lau_name) %>% 
  count(name = "died_city") 


born_country <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id) %>% 
  left_join(all_individuals_data) %>% 
  distinct() %>%  
  drop_na(cntr_code) %>% 
  mutate(country = str_sub(gisco_id, 1, 2)) %>%
  mutate(born_country = case_when(country == cntr_code  ~ "yes",
                                  country != cntr_code  ~ "no")) %>%  
  filter(born_country == "yes") %>% 
  group_by(gisco_id, lau_name) %>% 
  count(name = "born_country")  


bplace_1 <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id, place_of_death_latitude, place_of_death_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_death_longitude) %>% 
  st_as_sf(coords = c("place_of_death_longitude", "place_of_death_latitude"), crs = 4326) %>% 
  st_join(nuts_2) %>% 
  st_set_geometry(NULL)  

antjnr <- bplace_1 %>% 
  filter(is.na(cntr_code))

bplace_2 <- data_humans %>% 
  filter(named_after_id %in% antjnr$named_after_id) %>% 
  select(gisco_id, lau_name, label, named_after_id, place_of_death_latitude, place_of_death_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_death_longitude) %>% 
  st_as_sf(coords = c("place_of_death_longitude", "place_of_death_latitude"), crs = 4326) %>% 
  st_join(world) %>% 
  st_set_geometry(NULL) %>% 
  add_column(nuts_id = NA,
             name_latn = NA)  


miss_bpl <- bplace_2 %>% 
  filter(is.na(cntr_code))  

bplace_3 <- data_humans %>% 
  filter(named_after_id %in% miss_bpl$named_after_id) %>%
  select(gisco_id, lau_name, label, named_after_id, place_of_death_latitude, place_of_death_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_death_longitude) %>% 
  st_as_sf(coords = c("place_of_death_longitude", "place_of_death_latitude"), crs = 4326) %>% 
  st_join(nuts_2, st_is_within_distance, dist = 1500) %>% 
  st_set_geometry(NULL)  


miss_bpl_lst <- bplace_3 %>% 
  filter(is.na(cntr_code))  


bplace_4 <- data_humans %>% 
  filter(named_after_id %in% miss_bpl_lst$named_after_id) %>% 
  select(gisco_id, lau_name, label, named_after_id, place_of_death_latitude, place_of_death_longitude) %>% 
  distinct() %>% 
  drop_na(place_of_death_longitude) %>% 
  st_as_sf(coords = c("place_of_death_longitude", "place_of_death_latitude"), crs = 4326)  %>% 
  st_join(world, st_is_within_distance, dist = 3500) %>% 
  st_set_geometry(NULL) %>% 
  add_column(nuts_id = NA,
             name_latn = NA)  


death_place_cntr <- bplace_1 %>% 
  drop_na(cntr_code) %>% 
  rbind(bplace_2 %>% drop_na(cntr_code)) %>% 
  rbind(bplace_3 %>% drop_na(cntr_code)) %>% 
  rbind(bplace_4%>% drop_na(cntr_code)) %>% 
  mutate(country = str_sub(gisco_id, 1, 2)) %>%
  mutate(died_country = case_when(country == cntr_code  ~ "yes",
                                  country != cntr_code  ~ "no")) %>%  
  filter(died_country == "yes") %>% 
  group_by(gisco_id, lau_name) %>% 
  count(name = "died_country") 


born_extra_eu <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id) %>% 
  left_join(all_individuals_data) %>% 
  distinct() %>%  
  drop_na(cntr_code) %>%
  mutate(born_ex_eu = case_when(!cntr_code %in% europe_cntr  ~ "yes",
                                cntr_code %in% europe_cntr  ~ "no")) %>% 
  filter(born_ex_eu == "yes") %>%
  group_by(gisco_id, lau_name) %>% 
  count(name = "born_extra_eu") 


born_extra_eu_labs <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id) %>% 
  left_join(all_individuals_data) %>% 
  distinct() %>%  
  drop_na(cntr_code) %>%
  mutate(born_ex_eu = case_when(!cntr_code %in% europe_cntr  ~ "yes",
                                cntr_code %in% europe_cntr  ~ "no")) %>% 
  filter(born_ex_eu == "yes") %>% 
  select(gisco_id, label, cntr_name) %>% 
  group_by(gisco_id) %>% 
  summarise(across(c(cntr_name, label), ~paste(.x, collapse = " | ")))  



born_extra_eu_countries <- data_humans %>% 
  select(gisco_id, lau_name, label, named_after_id) %>% 
  left_join(all_individuals_data) %>% 
  distinct() %>%  
  drop_na(cntr_code) %>%
  mutate(born_ex_eu = case_when(!cntr_code %in% europe_cntr  ~ "yes",
                                cntr_code %in% europe_cntr  ~ "no")) %>% 
  filter(born_ex_eu == "yes") %>% 
  select(gisco_id, cntr_name) %>% 
  group_by(gisco_id, cntr_name) %>% 
  count() %>%
  arrange(gisco_id, -n) %>%
  mutate(country_n = paste(cntr_name, n, sep =  " ")) %>% 
  group_by(gisco_id) %>% 
  summarise(countries = paste(country_n, collapse = " | "))  


death_extra_eu <- bplace_1 %>% 
  drop_na(cntr_code) %>% 
  rbind(bplace_2 %>% drop_na(cntr_code)) %>% 
  rbind(bplace_3 %>% drop_na(cntr_code)) %>% 
  rbind(bplace_4%>% drop_na(cntr_code)) %>% 
  mutate(country = str_sub(gisco_id, 1, 2)) %>%
  mutate(died_extra_eu =  case_when(!cntr_code %in% europe_cntr  ~ "yes",
                                    cntr_code %in% europe_cntr  ~ "no")) %>% 
  filter(died_extra_eu == "yes") %>% 
  group_by(gisco_id, lau_name) %>% 
  count(name = "died_extra_eu") 


death_birth_places <- coordinate_data %>% 
  left_join(born_city) %>% 
  left_join(died_city) %>%
  left_join(born_country) %>%
  left_join(death_place_cntr) %>%
  left_join(born_extra_eu) %>%
  left_join(death_extra_eu) %>%
  left_join(born_extra_eu_labs)  



## Lifespan


data_lifespan <- data_geo %>% 
  filter(!gisco_id %in% c("ES_28079","PT_Q597")) %>% 
  st_set_geometry(NULL) %>% 
  filter(person == 1) %>% 
  select(gisco_id, lau_name, named_after_id, gender, date_of_birth, date_of_death) %>% 
  mutate(date_of_birth = str_remove_all(date_of_birth, "\\;.*"),
         date_of_death = str_remove_all(date_of_death, "\\;.*")) %>% 
  mutate(date_of_birth = as.numeric(substr(date_of_birth, 1, 5)),
         date_of_death = as.numeric(substr(date_of_death, 1, 5))) %>%  
  filter(!is.na(date_of_birth)) %>%   
  filter(!is.na(date_of_death)) 


dir.create("lifespan_data")

gisco_id <- unique(data_lifespan$gisco_id)

for (i in gisco_id) {
  
  filename <- data_lifespan %>% 
    filter(gisco_id == i) %>% 
    mutate(lau_id_name_combo = gisco_id %>% 
             paste(lau_name, sep = "-") %>% 
             paste0("_lifespan") %>% 
             paste0(".csv")) %>% 
    pull(lau_id_name_combo) %>% 
    unique()
  
  
  data_i <- data_lifespan %>% 
    filter(gisco_id == i) %>% 
    select(gisco_id, gender, date_of_birth, date_of_death, named_after_id) %>%
    distinct() %>%  
    mutate(id = row_number()) %>%  
    group_by(gisco_id, id, gender) %>% 
    mutate(year = list(date_of_birth:date_of_death)) %>% 
    ungroup %>% 
    unnest(cols = c(year)) %>% 
    group_by(gisco_id, gender) %>%
    count(year) %>% 
    pivot_wider(names_from = gender, values_from = n) %>% 
    arrange(year) 
  
  
  write_csv(data_i, here("lifespan_data", filename), na = "0") 
  
}