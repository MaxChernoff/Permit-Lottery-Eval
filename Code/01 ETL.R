source('Code/00 Init.R')




# Import Raw Data ---------------------------------------------------------

tab_rogue_winners_25_raw <- read_excel(file.path(dirs$input, 'rogue-lottery-winners_2025.xlsx'),
                                       skip = 5) %>% 
  clean_names() %>% 
  mutate(source = '2025')

tab_rogue_winners_24_raw <- read_excel(file.path(dirs$input, 'rogue-lottery-winners2024.xlsx'),
                                       skip = 5) %>% 
  clean_names() %>% 
  mutate(source = '2024')

tab_rogue_winners_22_raw <- read_excel(file.path(dirs$input, '2022_Rogue_River_Wild_Section_Awarded.xlsx'),
                                       skip = 4) %>% 
  clean_names() %>% 
  mutate(source = '2022') %>% 
  glimpse

tab_census_top_names <- read_excel(file.path(dirs$input, 'Names_2010Census_Top1000.xlsx'),
                                   skip = 2) %>% 
  clean_names()


tab_ribd_23 <- read_csv(file.path(dirs$input, 'FY23 Historical Reservations Full.csv'))

tab_ribd_22 <- read.csv(file.path(dirs$input, 'FY22 Historical Reservations Full.csv'))

# Clean Data --------------------------------------------------------------

tab_rogue_winners_25_1 <- tab_rogue_winners_25_raw %>% 
  filter(!is.na(results_status),
         results_status != 'Results Status') %>%
  glimpse

tab_rogue_winners_24_1 <- tab_rogue_winners_24_raw %>% 
  rename(entry_date = date) %>% 
  filter(!is.na(results_status),
         results_status != 'Results Status') %>%
  glimpse

tab_rogue_winners_22_1 <- tab_rogue_winners_22_raw %>% 
  filter(!is.na(reservation_number),
         reservation_number != '2022 Rogue River Lottery Awarded') %>%
  rename(entry_date = launch_date, 
         group_size = party_size) %>% 
  mutate(group_size = as.character(group_size))

tab_rogue_winners_comb1 <- bind_rows(tab_rogue_winners_25_1,
                                     tab_rogue_winners_24_1,
                                     tab_rogue_winners_22_1)

tab_rogue_winners_comb2 <- tab_rogue_winners_comb1 %>% 
  transmute(last_name = str_to_upper(last_name),
            initial = str_to_upper(initial),
            entry_date = as_date(mdy(entry_date)),
            group_size = as.numeric(group_size),
            state = str_to_upper(state),
            source)




# Explore -----------------------------------------------------------------


tab_rogue_winners_comb3 <- tab_rogue_winners_comb2 %>% 
  group_by(last_name) %>% 
  mutate(n_ln = n()) %>% 
  ungroup() %>% 
  mutate(ln_fl = str_sub(last_name, 1, 1)) 

tab_rogue_winners_comb3 %>% count(ln_fl) %>% 
  arrange(desc(n))

tab_rogue_winners_comb3 %>% count(state) %>% 
  arrange(desc(n))


# filter out 2022 because we don't have state info
tab_rogue_winners_comb4 <- tab_rogue_winners_comb2 %>% 
  filter(source != '2022')

tab_rogue_winners_comb4 %>% 
  mutate(ln_fl = str_sub(last_name, 1, 1)) %>% 
  filter(!is.na(state)) %>% 
  ggplot()+
  geom_histogram(aes(x = group_size, fill = state))+
  scale_x_continuous(n.breaks = 20)


tab_rogue_winners_comb2 %>%
  filter(source == '2025') %>% 
  group_by(state) %>% 
  mutate(state_n = n()) %>% 
  ungroup() %>% 
  ggplot()+
  geom_histogram(aes(x = state_n, fill = state))

tab_names_analysis <- tab_rogue_winners_comb2 %>% 
  left_join(tab_census_top_names %>% 
              distinct(last_name = surname,
                       proportion_per_100_000_population))


# this is all 3 years available
tab_names_analysis %>% 
  count(last_name) %>% 
  filter(n>1) %>% 
  left_join(tab_census_top_names %>% 
              distinct(last_name = surname,
                       proportion_per_100_000_population)) %>% 
  filter(proportion_per_100_000_population < 50 |
           is.na(proportion_per_100_000_population)) %>% View()


tab_names_analysis %>% 
  filter(!is.na(initial)) %>% 
  mutate(concat_name = paste0(initial, ' ', last_name)) %>% 
  group_by(concat_name) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count>1,
         proportion_per_100_000_population < 50 |
           is.na(proportion_per_100_000_population)) %>% 
  View()


# There are a lot of rows with the same concat name who are winning more than once in the same year or across years
tab_names_analysis %>% 
  filter(!is.na(initial)) %>% 
  mutate(concat_name = paste0(initial, ' ', last_name)) %>% 
  group_by(concat_name) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count>1) %>% View()
# can also possibly use group-size to deduce relationship between years

tab_names_analysis1 <- tab_names_analysis %>% 
  filter(!is.na(initial)) %>% 
  mutate(concat_name = paste0(initial, ' ', last_name)) 

# These are people with the same name and group size across years
multiple_obs <- tab_names_analysis1 %>%  
  group_by(concat_name, group_size) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>% 
  ungroup() %>% 
  mutate(last_name = str_replace(concat_name, "^[^ ]* ", "")) %>% 
  left_join(tab_census_top_names %>% 
              distinct(last_name = surname,
                       proportion_per_100_000_population)) %>% 
  filter(is.na(proportion_per_100_000_population))


multiple_obs %>% 
  distinct(concat_name) %>% 
  left_join(tab_names_analysis1 %>% 
              distinct(concat_name, source)) %>% View()


tab_names_analysis1 %>% 
  group_by(concat_name) %>% 
  mutate(concat_count = n()) %>% 
  filter(concat_count>1) %>% 
  left_join(tab_census_top_names %>% 
              distinct(last_name = surname,
                       proportion_per_100_000_population)) %>% 
  filter(proportion_per_100_000_population < 50 |
           is.na(proportion_per_100_000_population))   %>% 
  distinct(last_name, initial, proportion_per_100_000_population) %>% View()

# Lets look without first initial
tab_names_analysis %>% 
  distinct(last_name, group_size, state, source) %>% 
  group_by(last_name, group_size) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1) %>% 
  left_join(tab_names_analysis %>% 
              distinct(last_name, source)) %>% 
  group_by(last_name, source) %>% 
  filter(n() > 1) %>% 
  View()


tab_rogue_winners_comb2 %>% 
  filter(!is.na(initial)) %>% 
  mutate(concat_name = paste0(initial, ' ', last_name)) %>% 
  count(concat_name) %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% View()




# RIBD --------------------------------------------------------------------

# Exploring Dino/Green to see what we can learn
dino_green_permits <- tab_ribd_23 %>% 
  filter(park == 'Dinosaur Green And Yampa River Permits')


dino_green_permits %>% 
  filter(inventorytype == 'LOTTERY_PERMIT') %>% 
  count()
#18518


dino_green_permits %>% 
  filter(inventorytype == 'LOTTERY_PERMIT') %>% 
  count(startdate) %>% 
  ggplot()+
  geom_bar(aes(x = startdate, y = n), stat = 'identity')


dino_green_permits  %>% 
  filter(inventorytype == 'LOTTERY_PERMIT') %>% 
  count(nights)
# All NA 
# holy fuck, I think rows with LOTTERY_PERMIT are the applications entered
# There are only 300 permits per Yampa and 300 per Greeen 

dino_green_permits %>% 
  filter(inventorytype != 'LOTTERY_PERMIT') %>% 
  nrow()
#988 



dino_green_permits %>% 
  filter(inventorytype != 'LOTTERY_PERMIT') %>% 
  filter(year(startdate) == 2023) %>% 
  count(startdate) %>% 
  ggplot()+
  geom_bar(aes(x = startdate, y = n), stat = 'identity')

#still more than I expected, but more reasonable with cancellations being roughly 30% of the time?

# Working assumption: rows that have LOTTERY_PERMIT




lottery_dates <- 
  dino_green_permits %>% 
  filter(inventorytype == 'LOTTERY_PERMIT') %>% 
  count(startdate) %>% 
  mutate(`Permit Type` = 'lottery')


launch_dates <- dino_green_permits %>% 
  filter(inventorytype != 'LOTTERY_PERMIT') %>% 
  count(startdate) %>% 
  mutate(`Permit Type` = 'launch')

comb_dates <- bind_rows(lottery_dates,
                        launch_dates) %>% 
  filter(startdate > as_date('2023-05-01'),
         startdate < as_date('2023-10-01')) %>% 
  mutate(day_type = if_else(weekdays(startdate) %in% c('Saturday', 'Sunday'),
                            'Weekend',
                            'Weekday'))


ggplot(comb_dates)+
  geom_line(aes(x = startdate, y = n, color = `Permit Type`))+
  geom_smooth(aes(x = startdate, y = n, color = `Permit Type`), method = 'lm')+
  geom_point(aes(x = startdate, y = n, shape = day_type))



all_rivers <- tab_ribd_23 %>% 
  transmute(year = '2023',
            park,
            startdate, 
            inventorytype,
            month = month(startdate),
            day = day(startdate)) %>% 
  arrange(month, day) %>% 
  mutate(month_day = paste(month, '-', day)) %>% 
  filter(startdate > as_date('2023-05-01'),
         startdate < as_date('2023-10-01')) %>% 
  bind_rows(tab_ribd_22 %>% 
              as_tibble() %>% 
              transmute(year = '2022',
                        park,
                        startdate, 
                        inventorytype,
                        startdate = as_datetime(startdate),
                        month = month(startdate),
                        day = day(startdate)) %>% 
              arrange(month, day) %>% 
              mutate(month_day = paste(month, '-', day)) %>% 
  filter(startdate > as_date('2022-05-01'),
         startdate < as_date('2022-10-01'))) %>% 
  filter(str_detect(park, 'River'))
            
            
all_rivers %>% 
  distinct(equipmentdescription)

all_rivers %>% filter(equipmentdescription == 'Boat')



all_rivers %>% distinct(park) %>% View()


main_rivers <- all_rivers %>% 
  filter(park %in% c('Rogue River Wild Section',
                     'Desolation Gray - Green River Permit',
                     'San Juan River Permit Lottery And Reservations',
                     'Westwater Canyon River Permits',
                     'Dolores River Permits (Gateway, CO to Dewey Bridge, UT)',
                     'Dinosaur Green And Yampa River Permits',
                     'Canyonlands National Park Overnight River Permits',
                     'Selway River (4 Rivers)',
                     'Madison River (MT)',
                     # 'Blue River Campground (CO)',
                     # 'Green River Lakes - Bridger-Teton NF (WY)',
                     'Hells Canyon - Snake River (4 Rivers)')) %>% 
         filter(inventorytype != 'CAMPING') %>% 
  mutate(`Permit Type` = if_else(inventorytype == 'PERMIT',
                                 'Launch Day',
                                 'Lottery Day'),
         day_type = if_else(weekdays(startdate) %in% c('Saturday', 'Sunday'),
                            'Weekend',
                            'Weekday'),
         `Day Name` = weekdays(startdate),
         `Day Name` = if_else(`Permit Type` == 'Lottery Day',
                              `Day Name`,
                              NA_character_),
         `Day Name` = factor(`Day Name`, levels = c('Monday',
                                                    'Tuesday',
                                                    'Wednesday',
                                                    'Thursday',
                                                    'Friday',
                                                    'Saturday',
                                                    'Sunday')),
         month_day = as_factor(month_day))



main_rivers_gg <- main_rivers %>% 
  group_by(park, month_day, `Permit Type`, day_type, `Day Name`, year) %>% 
  summarise(`Applications per Day` = n()) %>% 
  ungroup() %>% 
  filter(`Permit Type` == 'Lottery Day') %>% 
  arrange(month_day) %>% 
  split(.$park)


main_rivers_gg_plot <- main_rivers_gg %>% 
  imap(~ggplot(.x)+
         geom_line(aes(x = month_day, y = `Applications per Day`, color = year, group = year))+
         geom_point(aes(x = month_day, y = `Applications per Day`, shape = `Day Name`), color = '#231650')+
         scale_color_manual(values = c('2022' = '#c68646', '2023' = '#a5c5da'))+
         labs(title = paste0(.y))+
         theme_te())

write_rds(main_rivers,
          file = file.path(dirs$output, glue::glue('River Data {Sys.Date()}.rds')))

main_rivers_gg_plot %>% 
  iwalk(~ggsave(plot = .x,
                filename = file.path(dirs$output, glue::glue('{.y} Plot {Sys.Date()}.png')),
                width = 12,
                height = 6))


# Permit-Required Rivers
permitted_rivers <- main_rivers %>% 
  distinct(park, inventorytype) %>% 
  group_by(park) %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  ungroup() %>% 
  left_join(main_rivers)

permitted_rivers %>% 
  count(park, inventorytype)
# Seems like Rogue River has bad permit data, but that's okay because we don't care
# about actual launch dates


permitted_rivers %>% 
  group_by(orderdate, park) %>% 
  filter(n()>1) %>% View(
  )



