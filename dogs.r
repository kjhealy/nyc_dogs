###--------------------------------------------------
### Dogs of New York
###--------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(sf)
library(showtext)
showtext_auto()

library(myriad)
import_myriad()

theme_set(theme_myriad_semi())

## Footer tag
caption_text <-  "@kjhealy http://socviz.co Data are for 2016. Source: DOHMH Dog Licensing System."

map_name <- function(doggo = "Max") {

name_sub <- subset(name_zip, animal_name == doggo)


name_map <- left_join(nyc_zips, name_sub)

p <- ggplot(name_map, aes(fill = pct))
p + geom_sf(color = "gray80", size = 0.1) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Percent of\nLicensed Dogs",
         title = paste0("Where's ", doggo, " ?"),
         caption = caption_text) +
    theme_map() + theme(plot.title = element_text(face = "bold", size = 18),
                        legend.text = element_text(size = 9),
                        legend.text.align = 0,
                        legend.title = element_text(face = "bold"),
                        legend.position = "left",
                        panel.grid.major = element_line(color = "white"),
                        legend.key = element_rect(color = "gray40", size = 0.1))


    ggsave(paste0("figures/", doggo, ".png"), height = 6, width = 6)
    ggsave(paste0("figures/", doggo, ".pdf"), height = 6, width = 6)
}

map_breed <- function(breed = "Pit Bull (or Mix)") {

breed_sub <- subset(breed_zip, breed_rc == breed)

breed_map <- left_join(nyc_zips, breed_sub)

p <- ggplot(breed_map, aes(fill = pct))
p + geom_sf(color = "gray80", size = 0.1) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Percent of\nLicensed Dogs",
         title = breed,
         caption = caption_text) +
#    guides(fill = guide_legend(title.position = "top", label.position = "right", reverse = TRUE)) +
    theme_map() + theme(plot.title = element_text(face = "bold", size = 18),
                        legend.text = element_text(size = 9),
                        legend.text.align = 0,
                        legend.title = element_text(face = "bold"),
                        legend.position = "left",
                        panel.grid.major = element_line(color = "white"),
                        legend.key = element_rect(color = "gray40", size = 0.1))


    ggsave(paste0("figures/", breed, ".png"), height = 6, width = 6)
    ggsave(paste0("figures/", breed, ".pdf"), height = 6, width = 6)
}

### --------------------------------------------------
### NYC Zip code map
###--------------------------------------------------

nyc_zips <- read_sf("geojson/nyc_zip_code_areas.geojson") %>%
    clean_names()

nyc_zips$zip_code <- as.integer(nyc_zips$postal_code)
nyc_zips <- nyc_zips %>% select(-one_of("postal_code"))

save(nyc_zips, file = "export/nyc_zips.rda", compress = "bzip2")


nyc_zips <- nyc_zips %>% select(objectid, zip_code, everything())

month_names <- c("January", "February", "March", "April", "May",
                 "June", "July", "August", "September", "October",
                 "November", "December")

month_nums <- as.character(c(1:12))

names(month_nums) <- month_names

boro_names <- c("Manhattan", "Queens", "Brooklyn", "Bronx", "Staten Island")
missing_names <- c("Unknown", "Name Not Provided")

###--------------------------------------------------
### Bite data
###--------------------------------------------------

bites <- read_csv("data/DOHMH_Dog_Bite_Data.csv") %>%
    clean_names() %>%
    modify_at("date_of_bite", str_replace_all, month_nums) %>%
    modify_at("date_of_bite", str_replace_all, pattern = " ", replacement = "-") %>%
    mutate(date_of_bite = mdy(date_of_bite)) %>%
    mutate(species = str_to_title(species),
           breed = str_to_title(breed),
           year = year(date_of_bite))


bites$breed_rc <- recode(bites$breed, "American Pit Bull Mix - Pit Bull Mix" =
                                        "Pit Bull (or Mix)",
                        "American Pit Bull Mix - Pit Bull Mix" = "Pit Bull (or Mix)",
                        "American Pit Bull Terrier-Pit Bull" = "Pit Bull (or Mix)",
                        "Pit Bull" = "Pit Bull (or Mix)",
                        "American Pit Bull Mix - Pit Bull Mix" = "Pit Bull (or Mix)")


bites %>%
    group_by(breed_rc) %>%
    tally() %>%
    arrange(desc(n))



by_boro <- bites %>%
    group_by(borough, breed_rc) %>%
    tally() %>%
    top_n(n = 10)


p <- ggplot(subset(by_boro, borough %nin% "Other"),
            aes(x = n, y = reorder(breed_rc, n)))
p_out <- p + geom_point() + facet_wrap(~ reorder(borough, n), scales = "free_y", ncol = 1) +
    labs(y = NULL, x = "Raw Number of Incidents",
         title = "Reported Dog Bites by Breed\nin New York, by Borough",
         caption = "@kjhealy / socviz.co. Data are for 2016. Source: DOHMH Dog Licensing System.")

ggsave("figures/biting_incidents.pdf", p_out, height = 10, width = 5)
ggsave("figures/biting_incidents.png", p_out, height = 10, width = 5)


###--------------------------------------------------
### Dog license data
###--------------------------------------------------

license <- read_csv("data/NYC_Dog_Licensing_Dataset-2.csv") %>%
    clean_names() %>%
    mutate(animal_name = str_to_title(str_trim(animal_name)),
           breed_name = str_to_title(str_trim(breed_name)),
           borough = str_to_title(str_trim(borough)))

license$borough  <-  recode(license$borough, "Staten Is" = "Staten Island")

license$breed_rc <- recode(license$breed_name, "American Pit Bull Terrier/Pit Bull" =
                                                   "Pit Bull (or Mix)",
                           "American Pit Bull Mix / Pit Bull Mix" = "Pit Bull (or Mix)",
                           "Labrador Retriever" = "Labrador (or Crossbreed)",
                           "Labrador Retriever Crossbreed" = "Labrador (or Crossbreed)")

license <- license %>%
    select(row_number:animal_birth_month, breed_rc, everything())
license <- license %>%
    select(-one_of("breed_name"))

nyc_license <- license
nyc_license$animal_birth_month <- mdy(nyc_license$animal_birth_month)
nyc_license$license_issued_date <- mdy(nyc_license$license_issued_date)
nyc_license$license_expired_date <- mdy(nyc_license$license__date)

save(nyc_license, file = "export/nyc_license.rda", compress = "bzip2")

pop_names <- license %>%
    filter(borough %in% boro_names) %>%
    filter(animal_name %nin% missing_names) %>%
    filter(animal_gender %nin% NA) %>%
    group_by(borough, animal_gender, animal_name) %>%
    tally() %>%
    top_n(n = 3)


p <- ggplot(pop_names, aes(x = n, y = reorder(animal_name, n), color = animal_gender))
p + geom_point(size = 1.3) + facet_wrap(~ reorder(borough, n), scales = "free_y", ncol = 1) +
    labs(y = NULL, x = "Number of Dogs", color = "Sex of Dog") + theme(legend.position = "top")


license %>%
    filter(borough %in% boro_names) %>%
    filter(animal_name %nin% missing_names) %>%
    filter(animal_gender %nin% NA) %>%
    group_by(animal_name, breed_name) %>%
    tally() %>%
    arrange(desc(n))


lotr <- license %>%
    filter(str_detect(animal_name, "Frodo|Samwise|Gandalf|Legolas|Gimli|Sauron|Merry|Pippin|Boromir|Aragorn|Strider")) %>%
    select(animal_name, breed_name, borough) %>%
    filter(animal_name %nin% "Merryweather") %>%
    group_by(borough, animal_name) %>%
    tally() %>%
    arrange(desc(n))

with(lotr, table(breed_name, animal_name))

swars <- license %>%
    filter(str_detect(animal_name, "Vader|Luke Skywalker|Han Solo|Leia|Artoo")) %>%
    select(animal_name, breed_name, borough) %>%
    group_by(animal_name) %>%
    tally() %>%
    arrange(desc(n))


austen <- license %>%
    filter(str_detect(animal_name, "Darcy|Bingley|Jane|Lizzie")) %>%
    select(animal_name, breed_name, borough) %>%
    group_by(animal_name) %>%
    tally() %>%
    arrange(desc(n))



with(swars, table(breed_name, animal_name))

trek <- license %>%
    filter(str_detect(animal_name, "Spock|Scottie")) %>%
    select(animal_name, breed_name, borough)

with(trek, table(breed_name, animal_name))

license %>%
    filter(str_detect(animal_name, "wiggles|Wiggles")) %>%
    select(animal_name, breed_name, borough,zip_code) %>%
    group_by(animal_name) %>%
    tally()


pop_breeds <- license %>%
    filter(borough %in% boro_names) %>%
    filter(animal_name %nin% missing_names) %>%
    filter(animal_gender %nin% NA) %>%
    group_by(borough, breed_rc) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    arrange(desc(pct)) %>%
    top_n(n = 10, wt = pct)


p <- ggplot(pop_breeds, aes(x = pct, y = reorder(breed_rc, pct)))
p_out  <- p + geom_point(size = 1.5) + facet_wrap(~ reorder(borough, pct), scales = "free_y", ncol = 1) +
    labs(y = NULL, x = "Percent of All Licensed Dogs in Borough",
         title = "Most Popular Dog Breeds\nin New York, by Borough",
         caption = "@kjhealy / socviz.co. Data are for 2016. Source: DOHMH Dog Licensing System.") +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave("figures/breeds.pdf", p_out, height = 10, width = 5)
ggsave("figures/breeds.png", p_out, height = 10, width = 5)


pop_names <- license %>%
    filter(borough %in% boro_names) %>%
    filter(animal_name %nin% missing_names) %>%
    filter(animal_gender %nin% NA) %>%
    group_by(borough, animal_name) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    arrange(desc(pct)) %>%
    top_n(n = 10, wt = pct)


p <- ggplot(pop_names, aes(x = pct, y = reorder(animal_name, pct)))
p_out  <- p + geom_point(size = 1.5) + facet_wrap(~ reorder(borough, pct), scales = "free_y", ncol = 1) +
    labs(y = NULL, x = "Percent of All Licensed Dogs in Borough",
         title = "Most Popular Dog Names\nin New York, by Borough",
         caption = "@kjhealy / socviz.co. Data are for 2016. Source: DOHMH Dog Licensing System.") + theme(legend.position = "top")

ggsave("figures/names.pdf", p_out, height = 10, width = 5)
ggsave("figures/names.png", p_out, height = 10, width = 5)


pop_names <- license %>%
    filter(borough %in% boro_names) %>%
    filter(animal_name %nin% missing_names) %>%
    filter(animal_gender %nin% c(NA, "F")) %>%
    group_by(borough, animal_name) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    arrange(desc(pct)) %>%
    top_n(n = 10, wt = pct)


p <- ggplot(pop_names, aes(x = pct, y = reorder(animal_name, pct)))
p_out  <- p + geom_point(size = 1.5) + facet_wrap(~ reorder(borough, pct), scales = "free_y", ncol = 1) +
    labs(y = NULL, x = "Percent of All Licensed Male Dogs in Borough",
         title = "Most Popular Names for Male Dogs\nin New York, by Borough",
         caption = "@kjhealy / socviz.co. Data are for 2016. Source: DOHMH Dog Licensing System.") + theme(legend.position = "top")

ggsave("figures/male_names.pdf", p_out, height = 10, width = 5)
ggsave("figures/male_names.png", p_out, height = 10, width = 5)




pop_names <- license %>%
    filter(borough %in% boro_names) %>%
    filter(animal_name %nin% missing_names) %>%
    filter(animal_gender %nin% c(NA, "M")) %>%
    group_by(borough, animal_name) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    arrange(desc(pct)) %>%
    top_n(n = 10, wt = pct)


p <- ggplot(pop_names, aes(x = pct, y = reorder(animal_name, pct)))
p_out  <- p + geom_point(size = 1.3) + facet_wrap(~ reorder(borough, pct), scales = "free_y", ncol = 1) +
    labs(y = NULL, x = "Percent of All Licensed Female Dogs in Borough",
         title = "Most Popular Names for Female Dogs\nin New York, by Borough",
         caption = "@kjhealy / socviz.co. Data are for 2016. Source: DOHMH Dog Licensing System.") + theme(legend.position = "top")

ggsave("figures/female_names.pdf", p_out, height = 10, width = 5)
ggsave("figures/female_names.png", p_out, height = 10, width = 5)

license %>% filter(animal_name %in%
                   c("Max", "Karl", "Emile", "Georg")) %>%
    group_by(animal_name) %>% tally() %>%
    arrange(desc(n))


breed_zip <- license %>%
    group_by(zip_code, breed_rc) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    ungroup() %>%
    complete(zip_code, breed_rc, fill = list(n = 0, freq = 0, pct = 0))

name_zip <- license %>%
    group_by(zip_code, animal_name) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    ungroup() %>%
    complete(zip_code, animal_name, fill = list(n = 0, freq = 0, pct = 0))


map_breed("Jack Russell Terrier")
map_breed("Chihuahua")
map_breed("Shih Tzu")
map_breed("Pit Bull (or Mix)")
map_breed("Rottweiler")
map_breed("German Shepherd Dog")
map_breed("Great Dane")
map_breed("Pug")
map_breed("German Pinscher")


map_name("Coco")
map_name("Charlie")
map_name("Princess")
map_name("Buddy")
map_name("Bella")
map_name("Lola")
map_name("Oliver")
map_name("Daisy")
map_name("Shadow")
map_name("Killer")
map_name("Luna")


breed_zip <- nyc_license %>%
    group_by(zip_code, breed_rc) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    ungroup() %>%
    complete(zip_code, breed_rc,
             fill = list(n = 0, freq = 0, pct = 0))

breed_zip <- nyc_license %>%
    group_by(zip_code, breed_rc) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2))

breed_sub <- subset(breed_zip, breed_rc == "Rottweiler")

breed_sub

rott_zip <- nyc_license %>%
    group_by(zip_code, breed_rc) %>%
    tally() %>%
    mutate(freq = n / sum(n),
           pct = round(freq*100, 2)) %>%
    filter(breed_rc == "Rottweiler")


rott_map <- left_join(nyc_zips, rott_zip)

p <- ggplot(rott_map, aes(fill = pct))
p + geom_sf(color = "gray80", size = 0.1) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Percent of\nLicensed Dogs",
         title = "Rottweilers") +
    theme_map() + theme(legend.position = "left",
                        panel.grid.major = element_line(color = "white"),
                        legend.key = element_rect(color = "gray40", size = 0.1))
