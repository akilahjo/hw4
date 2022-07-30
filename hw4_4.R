library(tidyverse)
library(ggplot2)
install.packages('ggridges')
library(ggridges)
library(lubridate)
library(ggrepel)
library(colorspace)

load("C:/Users/Akila/Documents/visualization/preprint_growth.rda") #please change the path if needed
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

preprint_full=filter(preprint_growth, count>0 & date > ymd("2004-12-31"))
head(preprint_full)
preprint_full1<-preprint_full %>% filter(archive %in%
                                        c("bioRxiv", "F1000Research")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "F1000Research")))
preprints_final1 <- filter(preprint_full1, date == ymd("2017-01-01"))


ggplot(preprint_full1) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final1$count, #and we use the counts to position our labels
      labels = c( "F1000Research", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(ymd("2014-02-01"), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#fe8d6d", "#7c6bea"),
                     name = NULL) +
  theme(legend.position = "right")+
  ggtitle('Preprint Counts')+
  theme(
    plot.title = element_text(color="#fe8d6d", size=14)
  )


