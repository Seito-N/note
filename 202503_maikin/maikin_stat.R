# package ----
pacman::p_load(
  tidyverse, 
  gtsummary,
  estatapi,
  ggsci,
  scales,
  ggview
)

# fonts ----
windowsFonts(meiryo = "Meiryo UI")

# data ----
## download ----
dir <- "your directory" # directory
url <- "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000040187500&fileKind=4"
download.file(
  url = url,
  destfile = paste0(dir, "/data/概況.xlsx"), 
  mode = "wb") # 作業 directory の下に data directory を作成

paste0(dir, "/data/概況.xlsx")
## 事前準備 ----
### 産業の表示順 ----

industry_levels <- c(
  "鉱業，採石業等",
  "建設業",
  "製造業",
  "電気・ガス業",
  "情報通信業",
  "運輸業，郵便業",
  "卸売業，小売業",
  "金融業，保険業",
  "不動産・物品賃貸業",
  "学術研究等",
  "飲食サービス業等",
  "生活関連サービス等",
  "教育，学習支援業",
  "医療，福祉",
  "複合サービス事業",
  "その他のサービス業"
)

## 一般労働者 ----
### 表 1 ----
dat_hyo1_一般 <- readxl::read_xlsx(
  paste0(dir, "./data/概況.xlsx"), 
  sheet = "給与額", range = "B27:L44"
  )

dat_hyo1_一般2 <- dat_hyo1_一般 %>% 
  rename(
    現金給与総額_円 = 円...2,
    現金給与総額_前年比 = `％...3`,
    きまって支給する給与_円 = 円...4,
    きまって支給する給与_前年比 = `％...5`,
    所定内給与_円 = 円...6,
    所定内給与_前年比 = `％...7`,
    所定外給与_円 = 円...8,
    所定外給与_前年比 = `％...9`,
    特別に支払われた給与_円 = 円...10,
    特別に支払われた給与_前年比 = `％...11`, 
    業種 = 一般労働者
  ) %>% 
  mutate(
    業種 = str_replace_all(string = 業種, pattern = "[ 　]", replacement = "")
  ) %>% 
  mutate(
    業種 = as_factor(業種) %>% 
      fct_relevel(., industry_levels)
  ) %>% 
  filter(業種 != c("調査産業計"))

### 表 2 ----
dat_hyo2_一般 <- readxl::read_xlsx(
  paste0(dir, "./data/概況.xlsx"), 
  sheet = "時間", range = "B27:J44"
)

dat_hyo2_一般2 <- dat_hyo2_一般 %>% 
  rename(
    総実労働時間 = `　時間`,
    総実労働時間_前年比 = `％...3`,
    所定内労働時間 = 時間...4,
    所定内労働時間_前年比 = `％...5`,
    所定外労働時間 = 時間...6,
    所定外労働時間_前年比 = `％...7`,
    出勤日数 = 日...8,
    出勤日数_前年差 = 日...9,
    業種 = `一般労働者　` 
  ) %>% 
  mutate(
    業種 = str_replace_all(string = 業種, pattern = "[ 　]", replacement = "")
  ) %>% 
  mutate(
    業種 = as_factor(業種) %>% 
      fct_relevel(., industry_levels)
  ) %>% 
  filter(業種 != c("調査産業計"))

### 表 3 ----
dat_hyo3_一般 <- readxl::read_xlsx(
  paste0(dir, "./data/概況.xlsx"), 
  sheet = "雇用", range = "B27:J44"
)

dat_hyo3_一般2 <- dat_hyo3_一般 %>% 
  rename(
    労働者総数 = 千人,
    労働者総数_前年比 = `％...3`,
    パートタイム労働者比率 = `％...4`,
    パートタイム労働者比率_前年差 = ﾎﾟｲﾝﾄ...5,
    入職率 = `％...6`,
    入職率_前年差 = ﾎﾟｲﾝﾄ...7,
    離職率 = `％...8`,
    離職率_前年差 = ﾎﾟｲﾝﾄ...9,
    業種 = 一般労働者 
  ) %>% 
  mutate(
    業種 = str_replace_all(string = 業種, pattern = "[ 　]", replacement = "")
  ) %>% 
  mutate(
    業種 = as_factor(業種) %>% 
      fct_relevel(., industry_levels)
  ) %>% 
  filter(業種 != c("調査産業計"))

## パートタイム労働者 ----
### 表 1 ----
dat_hyo1_パート <- readxl::read_xlsx(
  paste0(dir, "./data/概況.xlsx"), 
  sheet = "給与額", range = "B47:L64"
)

dat_hyo1_パート2 <- dat_hyo1_パート %>% 
  rename(
    現金給与総額_円 = 円...2,
    現金給与総額_前年比 = `％...3`,
    きまって支給する給与_円 = 円...4,
    きまって支給する給与_前年比 = `％...5`,
    所定内給与_円 = 円...6,
    所定内給与_前年比 = `％...7`,
    所定外給与_円 = 円...8,
    所定外給与_前年比 = `％...9`,
    特別に支払われた給与_円 = 円...10,
    特別に支払われた給与_前年比 = `％...11`, 
    業種 = パートタイム労働者
  ) %>% 
  mutate(
    業種 = str_replace_all(string = 業種, pattern = "[ 　]", replacement = "")
  ) %>% 
  mutate(
    業種 = as_factor(業種) %>% 
      fct_relevel(., industry_levels)
  ) %>% 
  filter(業種 != c("調査産業計"))

### 表 2 ----
dat_hyo2_パート <- readxl::read_xlsx(
  paste0(dir, "./data/概況.xlsx"), 
  sheet = "時間", range = "B47:J64"
)

dat_hyo2_パート2 <- dat_hyo2_パート %>% 
  rename(
    総実労働時間 = `　時間`,
    総実労働時間_前年比 = `％...3`,
    所定内労働時間 = 時間...4,
    所定内労働時間_前年比 = `％...5`,
    所定外労働時間 = 時間...6,
    所定外労働時間_前年比 = `％...7`,
    出勤日数 = 日...8,
    出勤日数_前年差 = 日...9,
    業種 = パートタイム労働者 
  ) %>% 
  mutate(
    業種 = str_replace_all(string = 業種, pattern = "[ 　]", replacement = "")
  ) %>% 
  mutate(
    業種 = as_factor(業種) %>% 
      fct_relevel(., industry_levels)
  ) %>% 
  filter(業種 != c("調査産業計"))

### 表 3 ----
dat_hyo3_パート <- readxl::read_xlsx(
  paste0(dir, "./data/概況.xlsx"), 
  sheet = "雇用", range = "B47:J64"
)

dat_hyo3_パート2 <- dat_hyo3_パート %>% 
  rename(
    労働者総数 = 千人,
    労働者総数_前年比 = `％...3`,
    パートタイム労働者比率 = `％...4`,
    パートタイム労働者比率_前年差 = ﾎﾟｲﾝﾄ...5,
    入職率 = `％...6`,
    入職率_前年差 = ﾎﾟｲﾝﾄ...7,
    離職率 = `％...8`,
    離職率_前年差 = ﾎﾟｲﾝﾄ...9,
    業種 = パートタイム労働者
  ) %>% 
  mutate(
    業種 = str_replace_all(string = 業種, pattern = "[ 　]", replacement = "")
  ) %>% 
  mutate(
    業種 = as_factor(業種) %>% 
      fct_relevel(., industry_levels)
  ) %>% 
  filter(業種 != c("調査産業計"))

# 表1 所定内給与 ----
## 一般労働者----

fig_hyo1_一般 <- dat_hyo1_一般2 %>% 
  select(業種, 所定内給与_円, 所定内給与_前年比) %>% 
  # mutate(所定内給与_前年比 = 所定内給与_前年比/100) %>% 

  ggplot(data = .,
         mapping = aes(x = 業種)) + 
  theme_classic() +
  
  geom_col(mapping = aes(y = 所定内給与_円), fill = "skyblue") +
  geom_point(mapping = aes(y = 所定内給与_前年比 * 4000), colour = "red") +
  geom_text(mapping = aes(y = 所定内給与_円, 
                          label = paste0(
                            format(所定内給与_円, big.mark = ","),
                            "\n",
                            format(所定内給与_前年比, nsmall = 1),
                            "%")
                          ), 
            size = 3, 
            vjust = -0.5, 
            family = "meiryo"
            ) + 
  
  scale_y_continuous(
    name = "所定内給与 (円)", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-10000, 500000),
    breaks = seq(0, 500000, by = 100000),
    sec.axis = sec_axis(~./4000, name = "所定内給与前年比（%）", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(-10, 100, by = 10)
                        )  # 副軸（2軸目）の設定
  ) +
  
  labs(x = "", title = "一般労働者 所定内給与（円、前年比）") +
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text(size = 11)
        ) 
  
  # canvas(units = "in", height = 8, width = 12)

fig_hyo1_一般

# ggsave(filename = "一般労働者_所定内給与.jpeg", plot = fig_hyo1_一般,
#        path = paste0(dir, "./out"),
#        units = "in", height = 7, width = 10)

## パートタイム労働者 ----
fig_hyo1_パート <- dat_hyo1_パート2 %>% 
  select(業種, 所定内給与_円, 所定内給与_前年比) %>% 
  # mutate(所定内給与_前年比 = 所定内給与_前年比/100) %>% 
  filter(業種 != c("調査産業計")) %>% 
  
  ggplot(data = .,
         mapping = aes(x = 業種)) + 
  theme_classic() +
  
  geom_col(mapping = aes(y = 所定内給与_円), fill = "skyblue") +
  geom_point(mapping = aes(y = 所定内給与_前年比 * 4000), colour = "red") +
  geom_text(mapping = aes(y = 所定内給与_円, 
                          label = paste0(
                            format(所定内給与_円, big.mark = ","),
                            "\n",
                            format(所定内給与_前年比, nsmall = 1),
                            "%")
  ), 
  size = 3, 
  vjust = -0.5, 
  family = "meiryo"
  ) + 
  
  scale_y_continuous(
    name = "所定内給与 (円)", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-100000, 500000),
    breaks = seq(0, 500000, by = 100000),
    sec.axis = sec_axis(~./4000, name = "所定内給与前年比（%）", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(-20, 100, by = 10)
    )  # 副軸（2軸目）の設定
  ) +
  
  labs(x = "", title = "パートタイム労働者 所定内給与（円、前年比）") +
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text(size = 11)
  ) 
fig_hyo1_パート
# canvas(units = "in", height = 8, width = 12)

ggsave(filename = "パートタイム労働者_所定内給与.jpeg", plot = fig_hyo1_パート,
       path = paste0(dir, "./out"),
       units = "in", height = 7, width = 10)

# 表2 ----
## 一般労働者 ----
fig_hyo2_一般 <- dat_hyo2_一般2 %>% 
  select(業種, 総実労働時間, 総実労働時間_前年比) %>% 
  
  ggplot(data = .,
         mapping = aes(x = 業種)) +
  theme_bw() +
  
  geom_col(mapping = aes(y = 総実労働時間), fill = "skyblue") + 
  geom_point(mapping = aes(y = 総実労働時間_前年比 * 2), colour = "red") +
  geom_text(mapping = aes(y = 総実労働時間, 
                          label = paste0(
                            format(総実労働時間, big.mark = ","),
                            "\n",
                            format(総実労働時間_前年比, nsmall = 1),
                            "%")
                          ),
            size = 3,
            vjust = -0.5,
            family = "meiryo", 
            ) + 
  
  scale_y_continuous(
    name = "総実労働時間（時間)", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-50, 200),
    breaks = seq(0, 200, by = 50),
    sec.axis = sec_axis(~./2, name = "総実労働時間前年比 (%)", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(-20, 100, by = 10)
    )  # 副軸（2軸目）の設定
  ) +
  
  labs(x = "", title = "一般労働者 総実労働時間（時間、前年比）") + 
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text(size = 11),
        )
  # canvas(units = "in", height = 8, width = 12)

ggsave(filename = "一般労働者_総実労働時間.jpeg", plot = fig_hyo2_一般,
       path = paste0(dir, "./out"),
       units = "in", height = 7, width = 10
       )

## パートタイム労働者 ----
fig_hyo2_パート <- dat_hyo2_パート2 %>% 
  select(業種, 総実労働時間, 総実労働時間_前年比) %>% 
  
  ggplot(data = .,
         mapping = aes(x = 業種)) +
  theme_bw() +
  
  geom_col(mapping = aes(y = 総実労働時間), fill = "skyblue") + 
  geom_point(mapping = aes(y = 総実労働時間_前年比 * 2), colour = "red") +
  geom_text(mapping = aes(y = 総実労働時間, 
                          label = paste0(
                            format(総実労働時間, big.mark = ","),
                            "\n",
                            format(総実労働時間_前年比, nsmall = 1),
                            "%")
  ),
  size = 3 ,
  vjust = -0.5,
  family = "meiryo", 
  ) + 
  
  scale_y_continuous(
    name = "総実労働時間（時間)", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-50, 200),
    breaks = seq(0, 200, by = 50),
    sec.axis = sec_axis(~./2, name = "総実労働時間前年比 (%)", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(-20, 100, by = 10)
    )  # 副軸（2軸目）の設定
  ) +
  
  labs(x = "", title = "パートタイム労働者 総実労働時間（時間、前年比）") + 
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text(size = 11),
        )

# ggsave(filename = "パートタイム労働者_総実労働時間.jpeg", plot = fig_hyo2_パート,
#        path = paste0(dir, "./out"),
#        units = "in", height = 7, width = 10)

# 表 3 ----
## 一般労働者 ----
fig_hyo3_一般 <- dat_hyo3_一般2 %>% 
  select(業種, 労働者総数, 入職率, 離職率) %>% 
  
  ggplot(data = .,
         mapping = aes(x = 業種)) +
  theme_bw() + 
  
  geom_col(mapping = aes(y = 労働者総数), fill = "skyblue") + 
  geom_point(mapping = aes(y = 入職率 * 1000), colour = "green") +
  geom_point(mapping = aes(y = 離職率 * 1000), colour = "orange") + 
  
  geom_text(mapping = aes(y = 8000,
                          label = paste0(
                            format(労働者総数, big.mark = ","), "千人",
                            "\n",
                            "入 ",
                            format(入職率, nsmall = 1), "%", 
                            "\n",
                            "離 ",
                            format(離職率, nsmall = 1), "%"
                            )
  ),
  size = 2.5,
  vjust = -0.5,
  family = "meiryo",
  ) +
  
  scale_y_continuous(
    name = "労働者総数", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-50, 10000),
    breaks = seq(0, 10000, by = 2500),
    sec.axis = sec_axis(~./1000, name = "入職率、離職率（%）", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(0, 10, by = 1)
    )  # 副軸（2軸目）の設定
  ) + 
  
  labs(x = "", title = "一般労働者 労働者総数、入職率、離職率") + 
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text(size = 11),
        )
# canvas(units = "in", height = 8, width = 12)

ggsave(filename = "一般労働者_労働者総数・入職率・離職率.jpeg", plot = fig_hyo3_一般,
       path = paste0(dir, "./out"),
       units = "in", height = 7, width = 10)

## パートタイム労働者 ----
fig_hyo3_パート <- dat_hyo3_パート2 %>% 
  select(業種, 労働者総数, 入職率, 離職率) %>% 
  
  ggplot(data = .,
         mapping = aes(x = 業種)) +
  theme_bw() + 
  
  geom_col(mapping = aes(y = 労働者総数), fill = "skyblue") + 
  geom_point(mapping = aes(y = 入職率 * 1000), colour = "green") +
  geom_point(mapping = aes(y = 離職率 * 1000), colour = "orange") + 
  
  geom_text(mapping = aes(y = 8000,
                          label = paste0(
                            format(労働者総数, big.mark = ","), "千人",
                            "\n",
                            "入 ",
                            format(入職率, nsmall = 1), "%", 
                            "\n",
                            "離 ",
                            format(離職率, nsmall = 1), "%"
                          )
  ),
  size = 2.5,
  vjust = -0.5,
  family = "meiryo",
  ) +
  
  scale_y_continuous(
    name = "労働者総数", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-50, 10000),
    breaks = seq(0, 10000, by = 2500),
    sec.axis = sec_axis(~./1000, name = "入職率、離職率（%）", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(0, 10, by = 1)
    )  # 副軸（2軸目）の設定
  ) + 
  
  labs(x = "", title = "パートタイム労働者 労働者総数、入職率、離職率") + 
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text( size = 11)
        )
# canvas(units = "in", height = 8, width = 12)

# ggsave(filename = "パートタイム労働者_労働者総数・入職率・離職率.jpeg", plot = fig_hyo3_パート,
#        units = "in", height = 7, width = 10,
#        path = paste0(dir, "./out"))
