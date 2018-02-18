#### Old Solutions ####
df_test <-
  df_medpharma %>%
  group_by(DMHID) %>%
  distinct() %>%
  filter(str_detect("OP|Mazda", TRANS_CO)) # distinct(DMHID, FIRST_DATE_SERVICE, DAYS_SUPPLY, DISPENSE_FEE, MAC_PRICE, ACQ_WHOLESALE_PRICE, AVQ_WHOLESALE_PRICE, BATCH_DATE, DATE_PAID)

df_medpharma %>%
  group_by_all() %>%
  n_distinct()

removal_index <-
  df_medpharma %>%
  group_by(DMHID) %>%
  filter(abs(DAYS_SUPPLY) == abs(DAYS_SUPPLY), abs(AVQ_WHOLESALE_PRICE) == abs(), abs(MAC_PRICE) == abs(), TRAN_CODE != TRAN_CODE)

g3 <- mutate(df_medpharma, Sum = MAC_PRICE / AirTime * 60)


## ORIGINAL BORROWED CODE
# https://stackoverflow.com/questions/36843826/select-specific-rows-based-on-previous-row-value-in-the-same-column
# Get indices of rows that meet condition
#ind2 <- which(df$Type==20 & dplyr::lag(df$Type)==40)
# Get indices of rows before the ones that meet condition
#ind1 <- which(df$Type==20 & dplyr::lag(df$Type)==40)-1


# creates a key with the variables that should be the same
# should filter out refunds
df_test <-
  df_test %>%
  select(ID, everything()) %>%
  group_by(DMHID) %>%
  arrange(FIRST_DATE_SERVICE, DRUG_CODE, abs(DAYS_SUPPLY), abs(DISPENSE_FEE), abs(MAC_PRICE), TRAN_CODE) %>%
  #mutate(key = paste(TRAN_CODE, abs(DAYS_SUPPLY), abs(DISPENSE_FEE), abs(MAC_PRICE), sep = "|")) %>%
  filter(TRAN_CODE != lag(TRAN_CODE, default = "0"))# %>%
#select(-key)
write_csv(
  df_test2 %>%
    arrange(DMHID) %>%
    select(-ID, -key),
  "data/df_widetest.csv")
