library(readstata13)
library(wesanderson)
library(tidyverse)
library(reshape2)
library(stringr)


#### County data

# Load data
county_full <- read.dta13('~/Dropbox/county-level-data/data/pop-age-race/county-pop-age-race-v2.dta')

# Keep 18-24 yo population
county <- county_full %>% select(academicyear, stabbr, stfips, cntyfips, stcntyfips, cntypop4_total_1824, cntypop4_hispanic_1824,
                                 # All
                                 cntypop4_white_nh_1824, cntypop4_black_nh_1824, cntypop4_api_nh_1824, cntypop4_aina_nh_1824,
                                 cntypop4_white_h_1824, cntypop4_black_h_1824, cntypop4_api_h_1824, cntypop4_aina_h_1824,
                                 # Males
                                 cntypop4_white_nh_male_1824, cntypop4_black_nh_male_1824, cntypop4_api_nh_male_1824, cntypop4_aina_nh_male_1824,
                                 cntypop4_white_h_male_1824, cntypop4_black_h_male_1824, cntypop4_api_h_male_1824, cntypop4_aina_h_male_1824,
                                 # Females
                                 cntypop4_white_nh_female_1824, cntypop4_black_nh_female_1824, cntypop4_api_nh_female_1824, cntypop4_aina_nh_female_1824,
                                 cntypop4_white_h_female_1824, cntypop4_black_h_female_1824, cntypop4_api_h_female_1824, cntypop4_aina_h_female_1824
                                 )

# Denver–Aurora, CO combined statistical area: https://en.wikipedia.org/wiki/Denver%E2%80%93Aurora_combined_statistical_area
# FIPS code: https://en.wikipedia.org/wiki/List_of_counties_in_Colorado
co_counties <- c('08031',  # City and County of Denver
                 '08005',  # Arapahoe County
                 '08059',  # Jefferson County
                 '08001',  # Adams County
                 '08035',  # Douglas County
                 '08014',  # City and County of Broomfield
                 '08039',  # Elbert County
                 '08093',  # Park County
                 '08019',  # Clear Creek County
                 '08047',  # Gilpin County
                 '08013',  # Boulder County
                 '08123'  # Weld County
                 )

# Keep counties
county <- county %>% filter(stcntyfips %in% co_counties)

# There are NA in some of the counts for males - but are not necessarily 0
# county[is.na(county)] = 0

# Create variables
county <- county %>% mutate(cntypop4_h_1824 = cntypop4_white_h_1824 + cntypop4_black_h_1824 + cntypop4_api_h_1824 + cntypop4_aina_h_1824,
                            cntypop4_h_male_1824 = cntypop4_white_h_male_1824 + cntypop4_black_h_male_1824 + cntypop4_api_h_male_1824 + cntypop4_aina_h_male_1824,
                            cntypop4_h_female_1824 = cntypop4_white_h_female_1824 + cntypop4_black_h_female_1824 + cntypop4_api_h_female_1824 + cntypop4_aina_h_female_1824,
                            cntypop4_1824 = cntypop4_white_nh_1824 + cntypop4_black_nh_1824 + cntypop4_api_nh_1824 + cntypop4_aina_nh_1824 + cntypop4_h_1824,
                            cntypop4_male_1824 = cntypop4_white_nh_male_1824 + cntypop4_black_nh_male_1824 + cntypop4_api_nh_male_1824 + cntypop4_aina_nh_male_1824 + cntypop4_h_male_1824,
                            cntypop4_female_1824 = cntypop4_white_nh_female_1824 + cntypop4_black_nh_female_1824 + cntypop4_api_nh_female_1824 + cntypop4_aina_nh_female_1824 + cntypop4_h_female_1824
                            )

# Sum for combined statistical area
county_combined <- county %>% select(-c(stabbr, stfips, cntyfips, stcntyfips)) %>%
  group_by(academicyear) %>%
  summarise_all(funs(sum))

# Add percent 18-24 yo in combined counties
for (i in c('white_nh', 'black_nh', 'api_nh', 'aina_nh', 'hispanic')) {
  county_combined[str_c('cntypop4_', i, '_1824_pct')] <- county_combined[str_c('cntypop4_', i, '_1824')] / county_combined$cntypop4_total_1824
}

# Checks
nrow(county_combined %>% filter(cntypop4_hispanic_1824 != cntypop4_h_1824)) == 0  # original hispanic var and created hispanic var are equal
nrow(county_combined %>% filter(cntypop4_total_1824 != cntypop4_1824)) == 0  # original total var and created total var are equal
nrow(county_combined %>% filter(cntypop4_total_1824 != cntypop4_male_1824 + cntypop4_female_1824)) == 0  # NA in male_1824 not actually equal to 0?
View(county_combined %>% select(cntypop4_white_nh_1824_pct, cntypop4_black_nh_1824_pct, cntypop4_api_nh_1824_pct, cntypop4_aina_nh_1824_pct, cntypop4_hispanic_1824_pct) %>% mutate(total_pct = cntypop4_white_nh_1824_pct + cntypop4_black_nh_1824_pct + cntypop4_api_nh_1824_pct + cntypop4_aina_nh_1824_pct + cntypop4_hispanic_1824_pct))  # pct adds up to 1

# Keep final variables
county_combined <- county_combined %>% select(academicyear, cntypop4_total_1824,
                                              cntypop4_white_nh_1824, cntypop4_black_nh_1824, cntypop4_api_nh_1824, cntypop4_aina_nh_1824, cntypop4_hispanic_1824,
                                              cntypop4_white_nh_1824_pct, cntypop4_black_nh_1824_pct, cntypop4_api_nh_1824_pct, cntypop4_aina_nh_1824_pct, cntypop4_hispanic_1824_pct
                                              )


#### IPEDS data

# Load data
df_full <- read.dta13('~/Dropbox/out-of-state/data/pub-inst-unitid-state.dta')

# Keep variables
df <- df_full %>% select(endyear, unitid, instname, countynm, state, sector,
                         # Full-time freshmen by race/gender
                         ugftfreshtotm, ugftfreshwhm, ugftfreshblm, ugftfreshhim, ugftfreshapm, ugftfreshasm, ugftfreshnhm, ugftfreshnam, ugftfreshmrm, ugftfreshunm, ugftfreshalm,
                         ugftfreshtotf, ugftfreshwhf, ugftfreshblf, ugftfreshhif, ugftfreshapf, ugftfreshasf, ugftfreshnhf, ugftfreshnaf, ugftfreshmrf, ugftfreshunf, ugftfreshalf,
                         # Part-time freshmen by race/gender
                         ugptfreshtotm, ugptfreshwhm, ugptfreshblm, ugptfreshhim, ugptfreshapm, ugptfreshasm, ugptfreshnhm, ugptfreshnam, ugptfreshmrm, ugptfreshunm, ugptfreshalm,
                         ugptfreshtotf, ugptfreshwhf, ugptfreshblf, ugptfreshhif, ugptfreshapf, ugptfreshasf, ugptfreshnhf, ugptfreshnaf, ugptfreshmrf, ugptfreshunf, ugptfreshalf,
                         # Full-time undergrads by race/gender
                         ugfttotm, ugftwhm, ugftblm, ugfthim, ugftapm, ugftasm, ugftnhm, ugftnam, ugftmrm, ugftunm, ugftalm,
                         ugfttotf, ugftwhf, ugftblf, ugfthif, ugftapf, ugftasf, ugftnhf, ugftnaf, ugftmrf, ugftunf, ugftalf,
                         # Part-time undergrads by race/gender
                         ugpttotm, ugptwhm, ugptblm, ugpthim, ugptapm, ugptasm, ugptnhm, ugptnam, ugptmrm, ugptunm, ugptalm,
                         ugpttotf, ugptwhf, ugptblf, ugpthif, ugptapf, ugptasf, ugptnhf, ugptnaf, ugptmrf, ugptunf, ugptalf,
                         # State population by race
                         state_pop_1824, pop4_white_nh_1824, pop4_black_nh_1824, pop4_api_nh_1824, pop4_aina_nh_1824, pop4_hispanic_1824
                         )

# AAU members: https://www.aau.edu/sites/default/files/AAU-Files/Who-We-Are/AAU-Member-List.pdf
aau <- c('139755',  # Georgia Institute of Technology (2010)
         '151351',  # Indiana University (1909)
         '153603',  # Iowa State University (1958)
         '171100',  # Michigan State University (1964)
         '204796',  # The Ohio State University (1916)
         '214777',  # The Pennsylvania State University (1958)
         '243780',  # Purdue University (1958)
         '186380',  # Rutgers University – New Brunswick (1989)
         '196097',  # Stony Brook University – The State University of New York (2001)
         '228723',  # Texas A&M University (2001)
         '196088',  # University at Buffalo – The State University of New York (1989)
         '104179',  # The University of Arizona (1985)
         '110644',  # University of California, Davis (1996)
         '110635',  # University of California, Berkeley (1900)
         '110653',  # University of California, Irvine (1996)
         '110662',  # University of California, Los Angeles (1974)
         '110680',  # University of California, San Diego (1982)
         '110705',  # University of California, Santa Barbara (1995)
         '110714',  # University of California, Santa Cruz (2019)
         '126614',  # University of Colorado, Boulder (1966)
         '134130',  # University of Florida (1985)
         '145637',  # University of Illinois at Urbana-Champaign (1908)
         '153658',  # The University of Iowa (1909)
         '155317',  # The University of Kansas (1909)
         '163286',  # University of Maryland at College Park (1969)
         '170976',  # University of Michigan (1900)
         '174066',  # University of Minnesota, Twin Cities (1908)
         '178396',  # University of Missouri, Columbia (1908)
         '199120',  # The University of North Carolina at Chapel Hill (1922)
         '209551',  # University of Oregon (1969)
         '215293',  # University of Pittsburgh (1974)
         '228778',  # The University of Texas at Austin (1929)
         '230764',  # The University of Utah (2019)
         '234076',  # University of Virginia (1904)
         '236948',  # University of Washington (1950)
         '240444'  # The University of Wisconsin – Madison (1900)
         )

# CO public universities
# unique(df_full$sector)
co <- unique((df_full %>% filter(state == 'CO', sector %in% c('pub-4yr', 'pub-2yr', 'pub-lt-2yr')))$unitid)

# Keep universities
df <- df %>% filter(unitid %in% c(aau, co))

# Create variables
race <- c('wh', 'bl', 'hi', 'ap', 'as', 'nh', 'na', 'mr', 'un', 'al')

# Number freshmen/undergrads by race
for (i in race) {
  # Full-time freshmen by race
  df[str_c('ugftfresh', i)] <- rowSums(df[, c(str_c('ugftfresh', i, 'm'), str_c('ugftfresh', i, 'f'))], na.rm=TRUE)
  # Part-time freshmen by race
  df[str_c('ugptfresh', i)] <- rowSums(df[, c(str_c('ugptfresh', i, 'm'), str_c('ugptfresh', i, 'f'))], na.rm=TRUE)
  # All freshmen by race
  df[str_c('ugftptfresh', i)] <- rowSums(df[, c(str_c('ugftfresh', i, 'm'), str_c('ugftfresh', i, 'f'), str_c('ugptfresh', i, 'm'), str_c('ugptfresh', i, 'f'))], na.rm=TRUE)
  
  # Full-time undergrads by race
  df[str_c('ugft', i)] <- rowSums(df[, c(str_c('ugft', i, 'm'), str_c('ugft', i, 'f'))], na.rm=TRUE)
  # Part-time undergrads by race
  df[str_c('ugpt', i)] <- rowSums(df[, c(str_c('ugpt', i, 'm'), str_c('ugpt', i, 'f'))], na.rm=TRUE)
  # All undergrads by race
  df[str_c('ugftpt', i)] <- rowSums(df[, c(str_c('ugft', i, 'm'), str_c('ugft', i, 'f'), str_c('ugpt', i, 'm'), str_c('ugpt', i, 'f'))], na.rm=TRUE)
}

# Total freshmen/undergrads
df <- df %>% mutate(ugftfresh = ugftfreshtotm + ugftfreshtotf,
                    ugptfresh = ugptfreshtotm + ugptfreshtotf,
                    ugftptfresh = ugftfreshtotm + ugftfreshtotf + ugptfreshtotm + ugptfreshtotf,
                    ugft = ugfttotm + ugfttotf,
                    ugpt = ugpttotm + ugpttotf,
                    ugftpt = ugfttotm + ugfttotf + ugpttotm + ugpttotf
                    )

# Percent freshmen/undergrads by race (might not add up to 1)
for (i in race) {
  # Full-time freshmen by race
  df[str_c('ugftfresh', i, '_pct')] <- df[str_c('ugftfresh', i)] / df$ugftfresh
  # Part-time freshmen by race
  df[str_c('ugptfresh', i, '_pct')] <- df[str_c('ugptfresh', i)] / df$ugptfresh
  # All freshmen by race
  df[str_c('ugftptfresh', i, '_pct')] <- df[str_c('ugftptfresh', i)] / df$ugftptfresh
  
  # Full-time undergrads by race
  df[str_c('ugft', i, '_pct')] <- df[str_c('ugft', i)] / df$ugft
  # Part-time undergrads by race
  df[str_c('ugpt', i, '_pct')] <- df[str_c('ugpt', i)] / df$ugpt
  # All undergrads by race
  df[str_c('ugftpt', i, '_pct')] <- df[str_c('ugftpt', i)] / df$ugftpt
}

# Percent 18-24 yo in state
for (i in c('white_nh', 'black_nh', 'api_nh', 'aina_nh', 'hispanic')) {
  df[str_c('pop4_', i, '_1824_pct')] <- df[str_c('pop4_', i, '_1824')] / df$state_pop_1824
}

# Checks
nrow(df %>% filter(state_pop_1824 != pop4_white_nh_1824 + pop4_black_nh_1824 + pop4_api_nh_1824 + pop4_aina_nh_1824 + pop4_hispanic_1824)) == 0  # state pop total is the sum of 5 race categories
# compare some original vars w/ created vars
View(df_full %>% filter(unitid == '126614') %>% select(endyear, unitid, ugftptfreshtot, ugftfreshwhmf, ugptfreshwhmf, ugftptfreshwhmf))
View(df %>% filter(unitid == '126614') %>% select(endyear, unitid, ugftptfresh, ugftfreshwh, ugptfreshwh, ugftptfreshwh))
# univ pct might not add up to 1 but state does
View(df %>% filter(unitid == '126614') %>% select(ugftptfreshwh_pct, ugftptfreshbl_pct, ugftptfreshhi_pct, ugftptfreshap_pct, ugftptfreshas_pct, ugftptfreshnh_pct, ugftptfreshna_pct, ugftptfreshmr_pct, ugftptfreshun_pct, ugftptfreshal_pct) %>% mutate(total_pct = ugftptfreshwh_pct + ugftptfreshbl_pct + ugftptfreshhi_pct + ugftptfreshap_pct + ugftptfreshas_pct + ugftptfreshnh_pct + ugftptfreshna_pct + ugftptfreshmr_pct + ugftptfreshun_pct + ugftptfreshal_pct))
View(df %>% filter(unitid == '126614') %>% select(pop4_white_nh_1824_pct, pop4_black_nh_1824_pct, pop4_api_nh_1824_pct, pop4_aina_nh_1824_pct, pop4_hispanic_1824_pct) %>% mutate(total_pct = pop4_white_nh_1824_pct + pop4_black_nh_1824_pct + pop4_api_nh_1824_pct + pop4_aina_nh_1824_pct + pop4_hispanic_1824_pct))

# Keep final variables
df <- df %>% select(endyear, unitid, instname, state, sector,
                    # Full-time freshmen by race
                    ugftfresh, ugftfreshwh, ugftfreshbl, ugftfreshhi, ugftfreshap, ugftfreshas, ugftfreshnh, ugftfreshna, ugftfreshmr, ugftfreshun, ugftfreshal,
                    ugftfreshwh_pct, ugftfreshbl_pct, ugftfreshhi_pct, ugftfreshap_pct, ugftfreshas_pct, ugftfreshnh_pct, ugftfreshna_pct, ugftfreshmr_pct, ugftfreshun_pct, ugftfreshal_pct,
                    # Part-time freshmen by race
                    ugptfresh, ugptfreshwh, ugptfreshbl, ugptfreshhi, ugptfreshap, ugptfreshas, ugptfreshnh, ugptfreshna, ugptfreshmr, ugptfreshun, ugptfreshal,
                    ugptfreshwh_pct, ugptfreshbl_pct, ugptfreshhi_pct, ugptfreshap_pct, ugptfreshas_pct, ugptfreshnh_pct, ugptfreshna_pct, ugptfreshmr_pct, ugptfreshun_pct, ugptfreshal_pct,
                    # All freshmen by race
                    ugftptfresh, ugftptfreshwh, ugftptfreshbl, ugftptfreshhi, ugftptfreshap, ugftptfreshas, ugftptfreshnh, ugftptfreshna, ugftptfreshmr, ugftptfreshun, ugftptfreshal,
                    ugftptfreshwh_pct, ugftptfreshbl_pct, ugftptfreshhi_pct, ugftptfreshap_pct, ugftptfreshas_pct, ugftptfreshnh_pct, ugftptfreshna_pct, ugftptfreshmr_pct, ugftptfreshun_pct, ugftptfreshal_pct,
                    # Full-time undergrads by race
                    ugft, ugftwh, ugftbl, ugfthi, ugftap, ugftas, ugftnh, ugftna, ugftmr, ugftun, ugftal,
                    ugftwh_pct, ugftbl_pct, ugfthi_pct, ugftap_pct, ugftas_pct, ugftnh_pct, ugftna_pct, ugftmr_pct, ugftun_pct, ugftal_pct,
                    # Part-time undergrads by race
                    ugpt, ugptwh, ugptbl, ugpthi, ugptap, ugptas, ugptnh, ugptna, ugptmr, ugptun, ugptal,
                    ugptwh_pct, ugptbl_pct, ugpthi_pct, ugptap_pct, ugptas_pct, ugptnh_pct, ugptna_pct, ugptmr_pct, ugptun_pct, ugptal_pct,
                    # All undergrads by race
                    ugftpt, ugftptwh, ugftptbl, ugftpthi, ugftptap, ugftptas, ugftptnh, ugftptna, ugftptmr, ugftptun, ugftptal,
                    ugftptwh_pct, ugftptbl_pct, ugftpthi_pct, ugftptap_pct, ugftptas_pct, ugftptnh_pct, ugftptna_pct, ugftptmr_pct, ugftptun_pct, ugftptal_pct,
                    # State population by race
                    state_pop_1824, pop4_white_nh_1824, pop4_black_nh_1824, pop4_api_nh_1824, pop4_aina_nh_1824, pop4_hispanic_1824,
                    pop4_white_nh_1824_pct, pop4_black_nh_1824_pct, pop4_api_nh_1824_pct, pop4_aina_nh_1824_pct, pop4_hispanic_1824_pct
                    )


#### Export data

# Subset data
peer_univs <- df %>% subset(unitid != '126614') %>% arrange(unitid, endyear)
cu_boulder <- df %>% subset(unitid == '126614')

# Merge county data w/ CU Boulder
cu_boulder_full <- merge(cu_boulder, county_combined, by.x = 'endyear', by.y = 'academicyear', all = TRUE)

# Export data
data <- bind_rows(cu_boulder_full, peer_univs)
save(data, file = 'data/cu_boulder_data.RData')
write.csv(data, 'data/cu_boulder_data.csv', na = '', row.names = FALSE)


#### Plot data

# Change over time of freshmen racial composition at CU Boulder
data %>% filter(unitid == '126614', ugftptfreshwh != 0) %>% ggplot(aes(x = endyear)) +
  geom_line(aes(y = ugftptfreshwh_pct, color = 'White')) +
  geom_line(aes(y = ugftptfreshbl_pct, color = 'Black')) +
  geom_line(aes(y = ugftptfreshhi_pct, color = 'Hispanic')) +
  geom_line(aes(y = ugftptfreshap_pct, color = 'Asian/pacific islander')) +
  geom_line(aes(y = ugftptfreshas_pct, color = 'Asian')) +
  geom_line(aes(y = ugftptfreshnh_pct, color = 'Native hawaiian')) +
  geom_line(aes(y = ugftptfreshna_pct, color = 'American indian')) +
  geom_line(aes(y = ugftptfreshmr_pct, color = 'Mixed race')) +
  geom_line(aes(y = ugftptfreshun_pct, color = 'Unknown race')) +
  geom_line(aes(y = ugftptfreshal_pct, color = 'Alien')) + 
  xlab('Academic endyear') + ylab('Percent of freshmen by race') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(name = 'Race/Ethnicity') +
  ggtitle('(All) Racial Composition of Freshmen at CU Boulder')

pdf('plots/graphA.pdf', paper = 'letter')
data %>% filter(unitid == '126614', ugftptfreshwh != 0) %>% ggplot(aes(x = endyear)) +
  geom_line(aes(y = ugftptfreshwh_pct, color = 'White')) +
  geom_line(aes(y = ugftptfreshbl_pct, color = 'Black')) +
  geom_line(aes(y = ugftptfreshhi_pct, color = 'Hispanic')) +
  geom_line(aes(y = ugftptfreshap_pct, color = 'Asian/pacific islander')) +
  xlab('Academic endyear') + ylab('Percent of freshmen by race') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(name = 'Race/Ethnicity', values = wes_palette('Moonrise1', n = 4)) +
  ggtitle('The Black and Gold Project: Racial Composition of Freshmen at CU Boulder')
dev.off()

# Compare CU Boulder to state/combined statistical area
pdf('plots/graphB.pdf', paper = 'letter')
data %>% filter(unitid == '126614', !is.na(state_pop_1824)) %>% ggplot(aes(x = endyear)) +
  geom_line(aes(y = ugftptfreshbl_pct, color = 'CU Boulder')) +
  geom_line(aes(y = pop4_black_nh_1824_pct, color = '18-24 yo in Colorado')) + 
  geom_line(aes(y = cntypop4_black_nh_1824_pct, color = '18-24 yo in Denver-Aurora')) +
  xlab('Academic endyear') + ylab('Percent of black population') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(name = 'Population', values = wes_palette('Moonrise1', n = 3)) +
  ggtitle('The Black and Gold Project: CU Boulder vs. CO vs. Denver-Aurora Area')
dev.off()

# Compare CU Boulder to peer universities (other pub-4yr's in CO)
co_4yrs <- data %>% filter(endyear == 2010, unitid %in% co, sector == 'pub-4yr') %>% arrange(unitid)

pdf('plots/graphC.pdf', paper = 'letter')
data %>% filter(unitid %in% co_4yrs$unitid, ugftptfreshwh != 0) %>% ggplot(aes(x = endyear, y = ugftptfreshbl_pct, group = unitid, color = as.factor(unitid))) +
  geom_line(aes(linetype = as.factor(unitid))) +
  scale_linetype_manual(values = c(rep('dotted', 3), 'solid', rep('dotted', 9)), guide = FALSE) +
  xlab('Academic endyear') + ylab('Percent of black freshmen') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(name = 'University', labels = co_4yrs$instname, values = wes_palette('Moonrise1', n = 13, 'continuous')) +
  ggtitle('The Black and Gold Project: CU Boulder vs. Other Public 4yrs in CO')
dev.off()
