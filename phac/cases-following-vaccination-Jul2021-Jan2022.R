source("R/00_common.R")


library(lubridate); library(magrittr); library(data.table); library(ggplot2); library(stringr)
library(dplyr); library(forcats)




# 1. Read raw data from pdf Table 2 ----

dtCases <- data.table()


dt <- data.table(   
  Date="July 10, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(552262, 32684, 28011, 2936),
  Hospitalizations = c(27889, 2443, 2303, 226),
  Deaths = c(5863, 633, 555, 85)
)
dtCases <- dtCases %>% rbind(dt)
dt <- data.table(   
  Date="September 04, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(629774, 36579, 39764, 19174),
  Hospitalizations = c(32432 , 2636, 2677, 844),
  Deaths = c(6587, 696, 632, 246)
)

dtCases <- dtCases %>% rbind(dt)
dt <- data.table(   
  Date="September 11, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(642016, 37321, 41562, 24717),
  Hospitalizations = c(33303, 2668, 2712, 1059),
  Deaths = c(6700, 699, 639, 312)
)

dtCases <- dtCases %>% rbind(dt)
dt <- data.table(   
  Date="October 02, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(671339, 40590, 46083, 43266),
  Hospitalizations = c(35848, 2823, 2919, 1813),
  Deaths = c(7120, 719, 681, 520)
)

dtCases <- dtCases %>% rbind(dt)
dt <- data.table(   
  Date="October 30, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(691361, 42321, 49670, 61825),
  Hospitalizations = c(37742, 2883, 3015, 2610),
  Deaths = c(7540, 741, 710, 776)
)

dtCases <- dtCases %>% rbind(dt)

dt <- data.table(   
  Date="November 27, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(705725, 43022, 51728, 82513),
  Hospitalizations = c(39967, 2988, 3258, 3514),
  Deaths = c(7861, 752, 731, 981)
)

dtCases <- dtCases %>% rbind(dt)

dt <- data.table(   
  Date="December 04, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(709123, 43114, 52116, 88742),
  Hospitalizations = c(40287, 3000, 3277, 3705),
  Deaths = c(7917, 753, 734, 1017)
)



dtCases <- dtCases %>% rbind(dt)

dt <- data.table(   
  Date="December 18, 2021",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(727925, 43471, 53171, 122843),
  Hospitalizations = c(40788, 3062, 3374, 4099),
  Deaths = c(8013, 759, 744, 1077)
)

dtCases <- dtCases %>% rbind(dt)


dt <- data.table(   
  Date="Jan 15, 2022",  
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases=c(771095, 44494, 61209, 581636),
  Hospitalizations = c(43540, 3118, 3717, 10387),
  Deaths = c(8479, 770, 788, 2032)
)

dtCases <- dtCases %>% rbind(dt)

dt <- data.table(  
  Date = "January 22, 2022",
  Status = c("Unvaccinated", "Cases not yet protected", "Partially vaccinated", "Fully vaccinated", "Total Cases"),
  Cases = c(892033, 50695, 79683, 648271),
  Hospitalizations = c(44907, 3154, 3925, 13043),
  Deaths = c(8693, 775, 808, 2490)
)

dtCases <- dtCases %>% rbind(dt)



# 2. process data ###############


# dt[Status=="Total Cases", Cases:= sum(dt[Status!="Total Cases"]$Cases)]


dtCases[, Date:=mdy(Date)]



setnames(dtCases, "Cases", "Tested Positive")
colsMetric <- c("Tested Positive", "Hospitalizations" ,"Deaths")
colsStatus <- c("Unvaccinated", "Cases not yet protected", "Partially vaccinated" , "Fully vaccinated",   "Total Cases" )

dtCases[, Status:=as.ordered(Status)]
dtCases[, Status:=fct_relevel(Status, colsStatus)]

dtTotals <- dtCases[Status!="Total Cases",  lapply(.SD, sum) , by = .(Date), .SDcols=colsMetric];
# dtTotals$Status = "Total Cases"

dtCases[Status=="Total Cases"]$`Tested Positive` = dtTotals$`Tested Positive`
dtCases[Status=="Total Cases"]$Hospitalizations = dtTotals$Hospitalizations
dtCases[Status=="Total Cases"]$Deaths = dtTotals$Deaths

fwrite(dtCases, "PHAC_Cases_following_vaccination_Table2-raw-data-log.csv", sep="\t")


# 2.2 dtCasesHor -----

dtCasesHor <- dtCases %>% dcast(Date ~ Status, value.var=colsMetric)

colsAll <- names(dtCasesHor)[-1]
colsAllDiff <- paste0(colsAll, " diff")
colsAllDiffPerWeek <- paste0(colsAll, " weekly")
# colsAllRaw <- paste0(colsAll, "_raw")


dtCasesHor[ ,  (colsAllDiff) := .SD - shift(.SD,1),  .SDcols = colsAll]
dtCasesHor[ ,  weeks := as.integer(Date - shift(Date,1)) / 7]
dtCasesHor[ , (colsAllDiffPerWeek) := round(.SD / weeks, 0),  .SDcols = colsAllDiff]

setnames (dtCasesHor, colsAll, paste0(colsAll, " raw"))

View(dtCasesHor)
dtCasesHor %>% datatable.fixedCols(1,2)

fwrite(dtCasesHor, "PHAC_Cases_following_vaccination_Table2-recalculated-wo-bias.csv", sep="\t")



dtCasesWeekly <- dtCasesHor %>% select (contains(c ( "Date","weekly")) )
# dtCasesWeekly <- dtCasesHor %>% select (contains(c ( "Date"," Diff")) )
dtCasesWeekly <- dtCasesHor %>% select (contains(c ( "Date","_raw")) )

dtCasesWeekly %>%  datatable.fixedCols(1,0)

# 3. Plot ######

metric=3


g <- list()


for (metric in 1:3) {
  
  dtCasesWeeklyMetric <- dtCasesWeekly %>% select(contains( c("Date",  colsMetric[metric])) )
  
  
  colTotals <- names(dtCasesWeeklyMetric)[6]
  
  if (F) { # percentages
    for ( i in 4:8) {
      col=names(dtCasesWeeklyMetric)[i]
      dtCasesWeeklyMetric[ , (col) := round( get(col) / get(colTotals) * 100, 1) ]
    }
  }
  
  dtRes <- dtCasesWeeklyMetric %>% melt(id=1)
  setnames(dtRes, "variable", "Status")
  dtRes[ , Status:=gsub(paste0(colsMetric[metric],"_"), "",Status)]
  dtRes[ , Status:=gsub(" weekly", "",Status)]
  dtRes[ , Status:=gsub(" raw", "",Status)]
  dtRes[ , Status:=gsub("_diff", "",Status)]
  
  colsStatus2 <- c(     "Cases not yet protected", "Partially vaccinated"   , "Unvaccinated"    , "Total Cases" )
  
  colsStatus3 <- c(  "Unvaccinated"    ,     "Partially vaccinated"   , "Cases not yet protected", "Fully vaccinated"  ,   "Total Cases" )
  
  dtRes[, Status:=fct_relevel(Status, colsStatus3)]
  
  g[[metric]] <- 
    ggplot(dtRes[Status!= "Total Cases" & 
                   # Date<ymd("2022-01-20") &                
                   Date>ymd("2021-07-20") 
                 ] ) +     theme_bw() +
    # guides(label="none") +
    # guides(label="col") +
    theme(legend.position = "bottom") +
# 
#     coord_flip() +
    # geom_line(aes(Date, value, col=Status)) +
    # geom_point(aes(Date, value, col=Status)) +
    # geom_label(aes(Date, value, col=Status, label=value), alpha(0.5)) +
    
    # geom_col(aes(Date, value, fill=Status), position = "stack") +
    geom_col(aes(Date, value, fill=Status), position = "fill") +
    

    # geom_label(aes(Date, 0, , label=value), alpha(1)) +   
    
    labs(
      # title=NULL, # 
      title=colsMetric[metric],
      # subtitle="Recomputed (average per week)",
      subtitle="Average per week",
      # subtitle="Reported (totals since 2020/12/14)",
      # subtitle="Reported (totals since 2020/12/14)",
      # caption="Source: Public Health Agency of Canada\n Full analysis: www.IVIM.ca",
      y=NULL,
      # y="Cases per week",
      x=NULL
    ) 

}




ggpubr::ggarrange(g[[1]], g[[2]], g[[3]], ncol = 3, common.legend = TRUE)
                  # title = "Cases counting since December 14, 2020",
                  # subtitle = "wert", caption = "rtyu")
# ggpubr::ggarrange(g[[1]], g[[2]], g[[3]], nrow= 3, common.legend = TRUE)



ggsave("PHAC-cases-by-vaccination-recomputed-per-week-rel.png", width=10, height=5)
ggsave("PHAC-cases-by-vaccination-recomputed-per-week-abs.png", width=9, height=6)


ggsave("PHAC-cases-by-vaccination-reported-since2020_12_14-rel.png", width=9, height=6)
ggsave("PHAC-cases-by-vaccination-reported-since2020_12_14-abs.png", width=9, height=6)



