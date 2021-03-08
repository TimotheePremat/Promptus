#Timothée Premat | 08/03/2021
#Script to print plots based on PAM (Poggio & Premat, 2019)* analysis results
    ## Poggio, Enzo & Timothée Premat, "Le PAM, un Programme d'Analyse Métrique pour le français médiéval"[= PAM: a software for analysis of medieval French metrics], in : Actes des Rencontres lyonnaises des jeunes chercheurs en linguistique historique, under the dir. of Timothée Premat & Ariane Pinche, Lyon : Diachronies contemporaines, 2019, pp. 59-70. ⟨https://hal.archives-ouvertes.fr/hal-02320550⟩ ⟨10.5281/zenodo.3464477⟩

#If needed, install packages. Uncomment the ones needed.
    #install.packages(scales)
    #install.packages(tidyverse)
    #install.packages(readxl)
    #install.packages(ggpubr)
    #install.packages(ggrepel)
    #install.packages(Hmisc)

#Load packages
library(scales)
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggrepel)
library(Hmisc)

#Set directory
setwd("~/Documents/GitHub/Promptus/roland/roland_normal")
## SET YOU OWN DIRECTORY FOR THE SCRIPT TO WORK!! Don't let it get lost :)

#Import datasets
##Import global data
df_loc_line <- read.table(
    "line_by_line_meter.txt",
    header=T)
df_epC <- df_loc_line %>%
    mutate(meter=replace(meter, ces_4=="4épC" & meter==11, 10)) %>%
    as.data.frame()
    #This replace meter=11 by meter=10 when ces_4=4épC, in order to display lines with m:11 and 4épC as having only 10 metrified syllable. They are merged with the 'natural' m:10 lines.
df_epC <- df_epC %>%
    mutate(meter=replace(meter, ces_6=="6épC" & meter==11, 10)) %>%
    as.data.frame()
    #This replace meter=11 by meter=10 when ces_6=6épC, in order to display lines with m:11 and 6épC as having only 10 metrified syllable. They are merged with the 'natural' m:10 lines.
    #We don't do that for line m:>11, because location of metrical break is not reliable for them.

##Import global data for schwa/no schwa
        PAM_raw_xlsx <- read_excel("all.xlsx")
    
    #First, let's apply some transformations to clean up and properly reorganize data!
    ##Moove columns
        PAM_raw_xlsx <- PAM_raw_xlsx %>% relocate(meter, ces_3, ces_4, ces_5, ces_6, ces_7)
    
    ##Delete uneven lines (which contains syllables and not tags)
        toDelete <- seq(1, nrow(PAM_raw_xlsx), 2)
        PAM_tag <- PAM_raw_xlsx[ toDelete ,]

    ##Delete empty columns (xlsx export from PAM creates extra columns to be sure to be large enough for big too-long lines) (would also delete unused -1 (elided schwa) columns, but it shouldn't have any consequences and should be very rare, if existing)
        PAM_tag <- PAM_tag[,colSums(is.na(PAM_tag))<nrow(PAM_tag)]

        #Reduce meter==11 to meter==10 when 4épC or 6épC detected
        PAM_tag_epC <- PAM_tag %>%
            mutate(meter=replace(meter, ces_4=="4épC" & meter==11, 10)) %>%
            as.data.frame()
        PAM_tag_epC <- PAM_tag_epC %>%
            mutate(meter=replace(meter, ces_6=="6épC" & meter==11, 10)) %>%
            as.data.frame()

#PLOT IT!
##LOC = dotplot that shows location of each type of line
LOC <- ggplot(
    df_epC,
    aes(
    x=num_l,
    y=meter)) +

geom_jitter(
  width=0,
  height=0.05) +

    ###Good looking parameters
    labs(
      x="Line number",
      y="Line-type (num. of metrified syllables)",
      title = "Line lenght* per line",
      caption = "*metrified syllables only") +
        #Very bad method to set the breaks, but still works. To be changed in the future.
      scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) +
      theme_classic()

##DISTRIB_lines = barplot of each type of m:n[x]
DISTRIB_lines <- ggplot(PAM_tag_epC, aes(meter)) + geom_bar() +
    geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
    ###Good looking parameters
    labs(
      x="Number of metrified syllables",
      y="Number of lines (squared log.)",
      title = "Distribution of line lenght*",
      caption = "*metrified syllables only") +
    theme_classic() +
    coord_trans(y='sqrt') +
    scale_x_continuous(breaks=c(7,8,9,10,11,12,13,14,15)) +
    scale_y_continuous(breaks=c(5,10,50,150,1000,4000))

##Apply transformations (needed for ploting) to the df
PAM_tag_epC <- PAM_tag_epC %>%
    mutate(meter_cont = as_factor(meter))

##STATS = calculate and print general results as the PAM but with comprehension of 4épC and 6épC
PAM_md <- PAM_tag_epC %>%
    group_by(meter) %>%
    summarise(count = n()) %>%
    mutate(rate=sprintf("%0.2f", count/sum(count)*100))

knitr::kable(PAM_md, "pipe", align = "lrr")
knitr::kable(PAM_md, format = "latex", align = "lrr")



#PRINT IT!
##Uncomment the plot you want to print. Leave all uncommented to print all plots.
LOC
    ggsave(LOC, filename = "loc_meter.png", width=25,height=10, units="cm", scale=1, dpi="retina")

DISTRIB_lines
    ggsave(DISTRIB_lines, filename = "distrib_meter.png", width=25, height=20.13, units="cm", scale=1, dpi="retina")
