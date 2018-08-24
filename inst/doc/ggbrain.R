## ----setup, include=F----------------------------------------------------
knitr::opts_chunk$set(eval=T)

## ----start---------------------------------------------------------------
library(ggbrain)
library(tidyverse)
ggbrain()

## ----void----------------------------------------------------------------
ggbrain() + theme_void()

## ----stacked-------------------------------------------------------------
ggbrain(position="stacked") +
  theme_void()

## ----fill----------------------------------------------------------------
ggbrain(mapping=aes(fill=area)) +
  theme_void()

## ----fill2---------------------------------------------------------------
ggbrain(colour="black", size=.7, mapping=aes(fill=area)) +
  theme_void()

## ----plot.area-----------------------------------------------------------
ggbrain(plot.area="superior frontal") +
  theme_void()

## ----plot.area2----------------------------------------------------------
ggbrain(plot.area=c("transverse temporal", "insula","pre central","superior parietal")) +
  theme_void()

## ----hemisphere----------------------------------------------------------
ggbrain(hemisphere="right") +
  theme_void()

## ----datasupp------------------------------------------------------------
someData = list(area=c("transverse temporal", "insula","pre central","superior parietal"), 
                p=sample(seq(0,.5,.001), 4)) %>% 
  as.data.frame(stringsAsFactors=F)

ggbrain(data=someData, mapping=aes(fill=p))


## ----datasupp2-----------------------------------------------------------
ggbrain(data=someData, mapping=aes(fill=p)) +
  theme_void() +
  scale_fill_gradient(low="firebrick",high="goldenrod") +
  labs(title="Aparc plots rule")


## ----datasuppX-----------------------------------------------------------
someData$hemi = "right"

ggbrain(data=someData, mapping=aes(fill=p)) +
  theme_void() +
  scale_fill_gradient(low="firebrick",high="goldenrod") +
  labs(title="Aparc plots rule")


## ----datasupp3-----------------------------------------------------------
someData = list(area=rep(c("transversetemporal", "insula","precentral","superiorparietal"),2), 
                p=sample(seq(0,.5,.001), 8),
                AgeG = c(rep("Young",4), rep("Old",4))
                ) %>% 
  as.data.frame(stringsAsFactors=F)

ggbrain(data=someData, mapping=aes(fill=p)) +
  facet_wrap(~AgeG, ncol=1) +
  theme(legend.position = "bottom")


## ----datasupp4-----------------------------------------------------------
ggbrain(data=someData, na.fill = "transparent",mapping=aes(fill=p)) +
  facet_wrap(~AgeG, ncol=1) +
  theme_dark() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.title = element_blank()
        )


## ----atlases-------------------------------------------------------------
ggbrain(atlas="aseg", mapping=aes(fill=area))

