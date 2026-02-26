library(ggplot2)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)

cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")


#ggplot for lm
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x, color = "#8fe388") + 
  ggtitle("LM Method") +
  theme(
    plot.title = element_text(size = 14, color = "#8fe388")
  )

#ggplot for glm
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#fe8d6d") + 
  ggtitle("GLM Method") +
  theme(
    plot.title = element_text(size = 14, color = "#fe8d6d")
  )





load("G:/.shortcut-targets-by-id/1ehWwunuAo7CE1Vk2JYkUnQMmxh5pph3C/DATA/preprint_growth.rda") #please change the path if needed
head(preprint_growth)
preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth
preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
preprints_final <- filter(preprints, date == ymd("2017-01-01"))

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")

#Gets the rows that have no NA values, a count greater than 0, and a date past 2004
preprint_growth %>% drop_na(count, date) %>%
  filter(count > 0, date > ymd("2004-12-31")) -> preprint_full

#Selects the rows that have bioRxiv or F1000Research
preprint_full %>% filter(archive == "bioRxiv" | archive == "F1000Research" ) -> preprint_full

#Draws line graphs
ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("#7c6bea", "#fe8d6d"),
    name = NULL
  ) +
  scale_x_date(
    limits = c(ymd("2014-02-01"), max(preprint_full$date)),
  ) +
  ggtitle("Preprint Counts") +
  theme(legend.position = "right")
