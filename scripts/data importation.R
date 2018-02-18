#### Load Libraries ####
library(tidyverse)
library(lubridate)

#### Import Data ####
df_medpharma <- read_tsv("data/MedPharma_CY16FY18Q2_19JAN18.txt", col_names = T)
df_dx_hx <- read_tsv("data/Dx_Hx_CY16FY18Q2_19JAN18.txt", col_names = T)
df_pdencs <- read_tsv("data/PdEncs_CY16FY18Q2_19JAN18.txt", col_names = T)
df_teds <- read_tsv("data/TEDS_CY16FY18Q2_19JAN18.txt", col_names = T)
df_teds_mat <- read_tsv("data/TEDS_MAT_CY16FY18Q2_19JAN18.txt", col_names = T)
df_teds_pa <- read_tsv("data/TEDS_PA_CY16FY18Q2_19JAN18.txt", col_names = T)


#### Clean MedPharma ####
# add index row before cleaning starts
df_medpharma$ID <- seq.int(nrow(df_medpharma))

# Convert dates from character type to proper formatted date
df_medpharma <- mutate_at(df_medpharma, vars(contains("DATE")), funs(dmy_hms))
df_dx_hx <- mutate_at(df_dx_hx, vars(contains("Dt")), funs(ymd))
df_pdencs <- mutate_at(df_pdencs, vars(contains("Date")), funs(ymd))

### Commands to make the reversals absolute ###
df_medpharma$DAYS_SUPPLY <- abs(df_medpharma$DAYS_SUPPLY)
df_medpharma$DISPENSE_FEE <- abs(df_medpharma$DISPENSE_FEE)
df_medpharma$MAC_PRICE <- abs(df_medpharma$MAC_PRICE)
df_medpharma$ACQ_WHOLESALE_PRICE <- abs(df_medpharma$ACQ_WHOLESALE_PRICE)
df_medpharma$AVQ_WHOLESALE_PRICE <- abs(df_medpharma$AVQ_WHOLESALE_PRICE)

## create test data set, remove last two characters to prep for NDC matching
## filter dataset for only the relevant drugs
df_medpharma <-
  df_medpharma %>%
  separate(DRUG_CODE, c("DRUG_CODE", "LAST_2"), sep = -2, convert = T) %>%
  select(ID, everything(), -LAST_2, -ACQ_WHOLESALE_PRICE, -BATCH_DATE, -DATE_PAID, -DRUG_NH_IND, -DRUG_QTY_DISPENSE) %>%
  filter(DRUG_CODE %in% c(
    540176, 540177, 540188, 540189, 935378, 935379, 935720, 935721, 2283153, 2283154, 2283155, 2283156, 3780923, 3780924, 4061923, 4061924, 4092012, 5170725, 21695515, 35356556,
    42023179, 42291174, 42291175, 43063667, 43063753, 50268144, 50268145, 50383287, 50383294, 50383924, 50383930, 52125678, 53217138, 53217246, 54123114, 54123907, 54123914, 54123929,
    54123957, 54123986, 55700302, 55700303, 58284100, 59385012, 59385014, 59385016, 60429586, 60429587, 60846970, 60846971, 61786911, 61786912, 62175452, 62175458, 62756459, 62756460,
    62756969, 62756970, 65162415, 65162416, 67046990, 67046991, 67046992, 67046993, 67046994, 67046995, 67046996, 67046997, 67046998, 67046999, 500901571, 500902924, 551544962, 636294092,
    636295074, 636297125, 636297126, 647250930, 647251924, 691890591, 705180442, 705180652, 705180711, 4061170, 16729081, 42291632, 43063591, 47335326, 51224206, 53217261, 65757300,
    68084291, 68094853, 500902866, 548685574, 636295304, 680712156, 691890499, 4091215, 4091219, 4091782, 6416132, 17478041, 17478042, 52584215, 52584369, 52584469, 52584782, 52584978,
    55700457, 63739463, 67457292, 67457299, 67457599, 69547212, 69547353, 70069071, 70069072, 500901836, 500902422, 548682062, 548686259, 551543954, 551544732, 551546980, 551546998,
    647251782, 703852013, 763291469, 763293369, 763293469
  ))
nrow(df_medpharma) # 2064 down from 311590!!

df_medpharma %>%
  group_by(TRAN_CODE) %>%
  summarise(count = n())
# 1690 OP and 374 BP, following reversal should remove 748 rows. Rows expected to be 1316

# There is at least one case where they were charged 29 days and reversed 30 days
# Will later combine this with above filter
df_medpharma <-
  df_medpharma %>%
  group_by(DMHID) %>%
  arrange(FIRST_DATE_SERVICE, DRUG_CODE, abs(DAYS_SUPPLY), TRAN_CODE, desc(DISPENSE_FEE)) %>%
  mutate(key = paste(TRAN_CODE, abs(DISPENSE_FEE), abs(MAC_PRICE), sep = "|"))

# Get indices of rows before the ones that meet condition
ind <- which(df_medpharma$TRAN_CODE=='BP')-1 #original transaction OP
df_medpharma <- df_medpharma[-c(ind),]
ind <- which(df_medpharma$TRAN_CODE=='BP')  #transaction reversal BP
df_medpharma <- df_medpharma[-c(ind),]

# Results in 1364 rows, 48 too many. no BP left
# Create variables for each drug type
df_medpharma <-
  df_medpharma %>%
  mutate(anybup = (DRUG_CODE %in% c(540176, 540177, 540188, 540189, 935378, 935379, 935720, 935721, 2283153, 2283154, 2283155, 2283156, 3780923, 3780924, 4061923, 4061924, 4092012, 5170725, 21695515, 35356556, 42023179, 42291174, 42291175, 43063667, 43063753, 50268144, 50268145, 50383287, 50383294, 50383924, 50383930, 52125678, 53217138, 53217246, 54123114, 54123907, 54123914, 54123929, 54123957, 54123986, 55700302, 55700303, 58284100, 59385012, 59385014, 59385016, 60429586, 60429587, 60846970, 60846971, 61786911, 61786912, 62175452, 62175458, 62756459, 62756460, 62756969, 62756970, 65162415, 65162416, 67046990, 67046991, 67046992, 67046993, 67046994, 67046995, 67046996, 67046997, 67046998, 67046999, 500901571, 500902924, 551544962, 636294092, 636295074, 636297125, 636297126, 647250930, 647251924, 691890591, 705180442, 705180652, 705180711))) %>%
  mutate(anynv =  (DRUG_CODE %in% c(4061170, 16729081, 42291632, 43063591, 47335326, 51224206, 53217261, 68084291, 68094853, 500902866, 548685574, 636295304, 680712156, 691890499))) %>%
  mutate(anynal = (DRUG_CODE %in% c(4091215, 4091219, 4091782, 6416132, 17478041, 17478042, 52584215, 52584369, 52584469, 52584782, 52584978, 55700457, 63739463, 67457292, 67457299, 67457599, 69547212, 69547353, 70069071, 70069072, 500901836, 500902422, 548682062, 548686259, 551543954, 551544732, 551546980, 551546998, 647251782, 703852013, 763291469, 763293369, 763293469))) %>%
  mutate(anyviv = (DRUG_CODE %in% c(65757300)))
  
df_test <-
  df_test2 %>%
  spread(FIRST_DATE_SERVICE, anybup)

df_test <-
  df_test2 %>%
  gather(FIRST_DATE_SERVICE, anybup)

df_test <- Filter(function(df_test) !(all(df_test=="NA")), df)

df_test <- df_test[,colSums(is.na(df_test)) == 0]

df_medpharma <-
  df_medpharma %>%
  select(-TRAN_CODE, -ID)

# now filter rows where collection date is greater or equal to the baseline entry date
# code here for eventual bucketing of treatment windows
df_urine <-
  df_urine %>%
  filter(COLLECT_DT >= Baseline)
# found some duplicates during spread
# Error: Duplicate identifiers for rows (676, 682), (298, 299), (677, 684), (678, 685), (679, 696)
df_urine <- distinct(df_urine)
df_urine <- spread(df_urine, "Test CODE", RESULT)

#### Export Clean Data ####
# export to RDS for speed and compression
saveRDS(df_dx_hx, "data/df_dx_hx.rds")
saveRDS(df_medpharma, "data/df_medpharma.rds")
saveRDS(df_pdencs, "data/df_pdencs.rds")
saveRDS(df_teds, "data/df_teds.rds")
saveRDS(df_teds_mat, "data/df_teds_mat.rds")
saveRDS(df_teds_pa, "data/df_teds_pa.rds")

# export to CSV for interoperability
write_csv(df_dx_hx, "data/df_dx_hx.csv")
write_csv(df_medpharma, "data/df_medpharma.csv")
write_csv(df_pdencs, "data/df_pdencs.csv")
write_csv(df_teds, "data/df_teds.csv")
write_csv(df_teds_mat, "data/df_teds_mat.csv")
write_csv(df_teds_pa, "data/df_teds_pa.csv")

#### Procedure for Medication Tracking Data ####
df_medpharma <-
  df_medpharma %>%
  arrange(DMHID, desc(FIRST_DATE_SERVICE)) # check if oldest to newest is ascending or descending