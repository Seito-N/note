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

# data set ----
## 一般労働者 ----
dir <- "C:/Users/seito/Documents/note/202503_maikin"

dat_hyo1_一般 <- readxl::read_xlsx(
  paste0(dir, "./data/hon-gaikyo202503p.xlsx"), 
  sheet = "給与額", range = "B27:L44"
  )


# 表示順
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

# データセット ----
## 一般労働者 
dat_hyo12 <- dat_hyo1 %>% 
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
  )

# 表1 所定内給与 ----
## 一般労働者----

fig_hyo1_一般 <- dat_hyo12 %>% 
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
            size = 2.5, 
            vjust = -0.5, 
            family = "meiryo"
            ) + 
  
  scale_y_continuous(
    name = "所定内給与 (円)", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-10000, 500000),
    breaks = seq(0, 500000, by = 100000),
    sec.axis = sec_axis(~./4000, name = "所定内給与前年比 (%)", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(-10, 100, by = 10)
                        )  # 副軸（2軸目）の設定
  ) +
  
  labs(x = "", title = "一般労働者 所定内給与（円、前年比）") +
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90)
        ) 
  
  # canvas(units = "in", height = 8, width = 12)

ggsave(filename = "一般労働者_所定内給与.jpeg", plot = fig_hyo1, 
       path = paste0(dir, "./out"))

## 短時間労働者 ----
fig_hyo1_短時間 <- dat_hyo12 %>% 
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
  size = 2.5, 
  vjust = -0.5, 
  family = "meiryo"
  ) + 
  
  scale_y_continuous(
    name = "所定内給与 (円)", 
    labels = label_comma(), # メイン軸はカンマ区切り
    limits = c(-10000, 500000),
    breaks = seq(0, 500000, by = 100000),
    sec.axis = sec_axis(~./4000, name = "所定内給与前年比 (%)", 
                        labels = scales::percent_format(scale = 1), 
                        breaks = seq(-10, 100, by = 10)
    )  # 副軸（2軸目）の設定
  ) +
  
  labs(x = "", title = "一般労働者 所定内給与（円、前年比）") +
  theme(text = element_text(family = "meiryo"),
        axis.text.x = element_text(angle = 90)
  ) 

# canvas(units = "in", height = 8, width = 12)
