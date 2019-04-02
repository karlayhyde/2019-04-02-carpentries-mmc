#David Denton and Tidyr
# Create gap_wide
gap_wide <- gapminder %>%
  gather(key = 'key', value = 'value', c('pop', 'lifeExp', 'gdpPercap')) %>%
  mutate(year_var = paste(key, year, sep = '_')) %>%
  select(country, continent, year_var, value) %>%
  spread(key = 'year_var', value = 'value')


# David also likes 
# skimr
# skim


skimr::skim(gapminder)

gap_long <- gap_wide %>%
  gather(obstype_year, obs_values, starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))

gap_long <- gap_wide %>%
  gather(key = 'obstype_year', value = 'obs_values', starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))

gap_long2 <- gap_long %>%
 separate(obstype_year, into = c('obs_type', 'year'), sep='_') %>%
 mutate(year =  as.numeric(year))



challenge2LifExp <- gap_long2 %>% 
  group_by(continent) %>%
  filter(obs_type=='lifeExp') %>%
  summarize(mean_LifeExp = mean(obs_values))


challenge2All <- gap_long2 %>% 
  group_by(continent, obs_type) %>%
  summarize(mean = mean(obs_values))

skimr::skim(gap_long2)


gap_normal <- gap_long2 %>% 
              spread(obs_type, obs_values)


# look at dimensions
dim(gap_normal)
dim(gapminder)
all.equal(gapminder, gap_normal)

all_equal(gapminder, gap_normal, ignore_col_order = TRUE)

gap_normal <- gap_normal %>% arrange(country,continent,year)
all.equal(gap_normal,gapminder)


gap_temp <- gap_long2 %>% unite(var_ID,continent,country,sep="_")
str(gap_temp)

gap_temp <- gap_long2 %>%
  unite(ID_var,continent,country,sep="_") %>%
  unite(var_names,obs_type,year,sep="_")
str(gap_temp)


gap_wide_new <- gap_long2 %>%
  unite(ID_var,continent,country,sep="_") %>%
  unite(var_names,obs_type,year,sep="_") %>%
  spread(var_names,obs_values)
str(gap_wide_new)

gap_ludicrously_wide <- gap_long2 %>%
  unite(var_names,obs_type,year,country,sep="_") %>%
  spread(var_names,obs_values)

