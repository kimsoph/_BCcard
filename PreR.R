# Loading -----------------------------------------------------------------

source("d:/_R/_Fnc/dsKim_library.R")

File = c("BCcard_1601_2106.csv"); bcd.00 <- read_csv(
  File, skip = 9, col_names = T, locale = locale(
    encoding = guess_encoding(File)[1,1] %>% as.character
    )
  ); bcd.00

# Pre-treatments ----------------------------------------------------------

bcd.01 <- # pre-01
  bcd.00 %>% 
  select( # Delete garbage column
    !contains("X")
    ) %>% 
  mutate( # cash : NA => 0 
    업종코드 = case_when(
      국내해외 == "국내" ~ replace_na(업종코드, "-001"),
      국내해외 == "해외" ~ replace_na(업종코드, "-002"),
      TRUE               ~ "NA"
      )
    ); bcd.01
  
bcd.02 <- # pre-02
  bcd.01 %>% 
  select( # renaming
    세금 = 구분, 일자 = 년월, 회원 = 회원구분, 국내 = 국내해외, 
    코드 = 업종코드, 업종 = 업종명, 금액 = 이용실적
    ) %>% 
  mutate( # variable type setting
    세금 = str_sub(세금, 3, 4),
    일자 = ym(일자) + months(1) - days(1),
    회원 = factor(회원),
    국내 = factor(국내)
    ) %>% 
  filter(세금 == "제외") %>% select(-세금); bcd.02

cde.cls <- # code number and matching class(industries)
  bcd.02 %>% 
  select(코드, 업종, 국내) %>% 
  unique() %>% 
  arrange(코드); cde.cls

# Saving ------------------------------------------------------------------

bcd <- bcd.02
bcd.txt <- c("Made from '_BCcard'")
bcd.des <- c("
bcd          : BC Card Dataset (2016.01 ~ 2021.06)
               구분, 년월, 회원구분, 국내해외, 업종코드, 업종명, 이용실적
cde.cls      : 코드, 업종, 국내
             ")
save(
  list = c("bcd", "cde.cls", "bcd.txt", "bcd.des"), 
  file = 'bcd.rda'
  )

paste(format(Sys.Date(), "%Y년 %m월 %d일 %A"), format(Sys.time(), "%H시 %M분"))











