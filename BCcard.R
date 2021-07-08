
# Loading -----------------------------------------------------------------

source('d:/_R/_Fnc/dsKim_library.R')
load('bcd.rda'); cat(bcd.des); bcd.00 <- bcd; bcd.00

bcd <- bcd.00 %>% drop_na(업종); bcd

# 월간 국내외 카드사용량 추이 ---------------------------------------------

bcd.mon.amt <- # credit card, monthly, amount
  bcd %>% 
  group_by(일자, 국내, 회원) %>% 
  summarise(amt = sum(금액, na.rm = T)) %>% 
  unite("구분", c(회원, 국내), sep = "", remove = T); bcd.mon.amt

bcd.mon.amt %>% # graph
  ggplot(aes(x = 일자, y = amt, color = 구분)) + geom_point() + geom_line() + 
  facet_wrap(~ 구분, scales = "free_y") + theme(legend.position = "None")
  
bcd.mon.amt %>% # graph and check outliers
  filter(구분 == "개인해외") %>% 
  ggplot(aes(x = 일자, y = amt)) + geom_point() + geom_line() + 
  geom_label_repel(
    aes(label = 일자 %>% as.character()), 
    data = bcd.mon.amt %>% filter(구분 == "개인해외", amt > .1e12)
    )

bcd %>% # outlier decomposition
  filter(
    일자 == ymd("2021-04-30"), 
    국내 == "해외", 회원 == "개인"
    ) %>% 
  arrange(desc(금액)) %>% DT::datatable(options = list(pageLength = 5))

# 매년 특정월 사용량 추이 ------------------------------------------------

bcd.mon.amt %>% 
  ungroup() %>% 
  mutate(년 = year(일자), 월 = month(일자, label = T)) %>% 
  select(년, 월, 구분, amt) %>% 
  ggplot(aes(x = 년, y = amt, group = 월, color = 월)) + 
  geom_point() + geom_line() + 
  facet_wrap(~ 구분, scales = "free_y")

# 카드사용금액 : 개인/기업, 국내/해외 -----------------------------------

bcd.psn.dom <- bcd %>% filter(국내 == "국내", 회원 == "개인") # 개인.국내 
bcd.psn.abd <- bcd %>% filter(국내 == "해외", 회원 == "개인") # 개인.해외 
bcd.crp.dom <- bcd %>% filter(국내 == "국내", 회원 == "기업") # 기업.국내
bcd.crp.abd <- bcd %>% filter(국내 == "해외", 회원 == "기업") # 기업.해외 

# 카드사용금액 톱5 업종 추이 --------------------------------------------

bcd.psn.dom %>% 
  group_by(일자) %>% 
  slice_max(order_by = 금액, n = 5) %>% 
  datatable(options = list(pageLength = 5))

dta <- # 인터넷PG, 음식점, 편의점, 슈퍼마켓, 대형할인점 
  bcd.psn.dom %>% filter(코드 %in% c(4076, 8001, 4010, 4020, 4004))

dta %>% # Plot
  ggplot(aes(x = 일자, y = 금액, color = 업종)) + geom_point() + geom_line()

# 코로나 영향을 가장 많이 받은 업종 --------------------------------------

c19.tme.srt <- "2019-11-30" %>% ymd # start c19
c19.tme.mdl <- "2020-12-31" %>% ymd # middle c19
c19.tme.now <- "2021-06-30" %>% ymd # nowadays

dta.01 <- # use person & domestic expanse
  bcd.psn.dom %>% 
  mutate(
    c19.tme = case_when(
      일자 == c19.tme.srt ~ "srt",
      일자 == c19.tme.mdl ~ "mdl",
      일자 == c19.tme.now ~ "now",
      TRUE                ~ "neg"
    )
  ) %>% 
  mutate(
    c19.tme = factor(c19.tme, levels = c("srt", "mdl", "now", "neg"))
  ) %>% 
  filter(c19.tme %ni% "neg") %>% 
  select(c19.tme, 일자, 코드, 업종, 금액) %>% 
  arrange(코드, c19.tme); dta.01

dta.02 <- # shrinking & rebound ratio
  dta.01 %>% 
  group_by(코드, 업종) %>% nest() %>% 
  mutate(
    c19.shk.rto = data %>% map_dbl(~ .x$금액[2]/.x$금액[1] - 1),
    c19.reb.rto = data %>% map_dbl(~ .x$금액[3]/.x$금액[1] - 1)
    ) %>% 
  select(-data) %>% 
  arrange(c19.reb.rto); dta.02

idx <- dta.02$코드 %>% tail(10)

bcd.psn.dom %>% 
  filter(코드 %in% idx) %>% 
  ggplot(aes(x = 일자, y = 금액, color = 업종)) + geom_point() + geom_line() + 
  facet_wrap(~업종, nrow = 2, scales = "free_y") + theme(legend.position = "none")

# 특정업종 국내개인카드 사용금액 -----------------------------------------

bcd.psn.dom %>% 
  #filter(코드 %in% c(1001, 1002, 1003, 1020)) %>% # 호텔(특급, 1급, 2급, 기타) 
  filter(코드 %in% c(2114, 2113, 7120, 2130)) %>% # 헬스장,수영장,사우나,노래방 
  ggplot(aes(x = 일자, y = 금액, color = 업종)) + geom_point() + geom_line() + 
  facet_wrap(~업종, nrow = 2, scales = "free_y") + theme(legend.position = "none")

# 전월대비 & 전년동월 대비 ----------------------------------------------

bcd.psn.dom.chg <- 
  bcd.psn.dom %>% 
  select(-회원, -국내) %>% 
  arrange(코드, 업종, 일자) %>% 
  mutate(
    전월     = lag(금액, n = 1),
    전월대비 = 금액/전월 - 1,
    전년     = lag(금액, n = 12),
    전년대비 = 금액/전년 - 1
  ) %>% 
  drop_na(전년대비) %>% 
  select(-전월, -전년); bcd.psn.dom.chg

bcd.psn.dom.chg %>% 
  filter(일자 > ymd("20170101")) %>% 
  #filter(코드 %in% c(1001, 1002, 1003, 1020)) %>% # 호텔(특급, 1급, 2급, 기타) 
  filter(코드 %in% c(2114, 2113, 7120, 2130)) %>% # 헬스장,수영장,사우나,노래방 
  ggplot(aes(x = 일자, y = 전년대비, color = 업종)) + 
  geom_point() + geom_line() + geom_hline(yintercept = 0, color = "blue") +
  facet_wrap(~업종, nrow = 2, scales = "free_y") + theme(legend.position = "none")







