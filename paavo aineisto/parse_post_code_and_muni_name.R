
# Mapping from post codes to municipalities 
# 21.11.2020
# Ville Mäkinen 
# Source is the Paavo data from Statistic Finland

setwd("/home/asdf/Desktop/asuntojen hinnat/")

raw_paavo_data <- read.table("./paavo aineisto/paavo_2020.csv", sep = ";", skip = 3, fileEncoding = "iso-8859-1")

paavo_headers <- read.table("./paavo aineisto/paavo_2020.csv", 
                            sep = ";", 
                            skip = 2, 
                            nrows = 1, 
                            stringsAsFactors = F, 
                            fileEncoding = "iso-8859-1")

names(raw_paavo_data) <- paavo_headers

raw_post_codes_and_muni <- as.character(raw_paavo_data$Postinumeroalue)

post_codes_and_muni <- grep("[0-9]{5}", raw_post_codes_and_muni, value=T)


post_code_start_ind <- regexpr("[0-9]{5}", post_codes_and_muni)
post_code <- substr(x = post_codes_and_muni, 
                    start = post_code_start_ind,
                    stop = post_code_start_ind + attributes(post_code_start_ind)$match.length - 1)

muni_name_start_ind <- regexpr("\\(.+\\)", post_codes_and_muni)
muni_name <- substr(x = post_codes_and_muni, 
                    start = muni_name_start_ind,
                    stop = muni_name_start_ind + attributes(muni_name_start_ind)$match.length - 1)
muni_name <- gsub("\\(", "", muni_name)
muni_name <- gsub("\\)", "", muni_name)

post_code_muni_name_df <- data.frame(post_code = post_code, muni_name = muni_name)

write.csv2(post_code_muni_name_df, "./paavo aineisto/post_code_muni_name_mapping.csv")
