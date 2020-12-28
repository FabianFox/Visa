# excluded:
# 490 - Other Asia, not elsewhere specified

# custom dictionary (added)
# 58 - Belgium-Luxembourg
# 251 - France, Monaco
# 381 - ITA
# 579 - NOR
# 699 - IND
# 711 - Southern African Customs Union
# 757 - CHE
# 842 - USA
custom.baci <- c("58" = "BEL","251" = "FRA","381" = "ITA", "579" = "NOR", "699" = "IND", "711" = "ZAF","757" = "CHE", "842" = "USA")

baci.df <- import("./data/independent variables/BACI_HS17_Y2018_V202001.csv") %>%
  rename(year = t, prod_cat = k, exp_iso3 = i, imp_iso3 = j, trade_flow = v, quantity = q) %>%
  mutate(exp_iso3 = countrycode(exp_iso3, "iso3n", "iso3c", custom_match = custom.baci),
         imp_iso3 = countrycode(imp_iso3, "iso3n", "iso3c", custom_match = custom.baci)) %>%
  filter(!is.na(exp_iso3) & !is.na(imp_iso3))

baci_zeros.df <- baci.df <- import("./data/independent variables/zeros_HS17_V202001.csv") %>%
  rename(year = t, exp_iso3 = i, imp_iso3 = j, zero_flow_dummy1 = ztf1, zero_flow_dummy2 = ztf2) %>%
  mutate(exp_iso3 = countrycode(exp_iso3, "iso3n", "iso3c", custom_match = custom.baci),
         imp_iso3 = countrycode(imp_iso3, "iso3n", "iso3c", custom_match = custom.baci))

# Filter BACI to countries in visa.df
baci.df <- baci.df %>%
  filter(exp_iso3 %in% unique(visa.df$destination_iso3), # subset to countries in visa.df
         imp_iso3 %in% unique(visa.df$destination_iso3))

# Missing values
visa.df[which(!unique(visa.df$destination_iso3) %in% unique(baci.df$exp_iso3)),]$destination_iso3
