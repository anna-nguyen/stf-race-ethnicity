################################################################################
# Shoo the Flu Evaluation
# Surveillance analysis

# load cdph data with surveillance data on ILI
# use to determine flu season and peak week
################################################################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load cdph data  
################################################################################

cdphpre17 = read_csv(file = cdph_pre17)
cdph1718  = read_csv(file = cdph_1718)

cdph      = rbind(cdphpre17, cdph1718)

################################################################################
# Clean dates
################################################################################

cdph$weekending=as.character(cdph$weekending)
slash=gregexpr("/",as.character(cdph$weekending))
slash1=matrix(NA,length(slash),1)
slash2=matrix(NA,length(slash),1)
for(i in 1:length(slash)){
  slash1[i,]=unlist(slash[i])[1]
  slash2[i,]=unlist(slash[i])[2]
}
cdph$slash1=slash1
cdph$slash2=slash2
cdph$mon=substr(cdph$weekending,1,cdph$slash1)
cdph$mon=as.numeric(gsub("/","",cdph$mon))
cdph$day=as.numeric(substr(cdph$weekending,cdph$slash1+1,cdph$slash2-1))
cdph$yr=as.numeric(substr(cdph$weekending,cdph$slash2+1,slash2+3))+2000
cdph$slash1=NULL
cdph$slash2=NULL

cdph$date1=paste0(cdph$day,"-",cdph$mon,"-",cdph$yr)
cdph$date=as.Date(cdph$date1,"%d-%m-%Y")
cdph$date1=NULL

################################################################################
# Define flu season
# Start of flu season: at least 2 consecutive weeks in which % of medical visits for ILI is > 2%
# End of flu season: at least 2 consecutive weeks in which % of medical visits for ILI is < 2%
################################################################################

# Function used to define flu season using CDPH ILI data and peak week of flu season
define_cdph_seas = function(data, cutoff){
  data = data %>% 
    mutate(above_cutoff = ifelse(ILIper > cutoff, 1, 0)) %>% 
    arrange(date) %>%
    mutate(fluseasCDPH = NA)
  
  # create indicator for flu season
  data$fluseasCDPH[1]=1
  for(i in 2:nrow(data)){
    data$fluseasCDPH[i-1][data$above_cutoff[i]==1 & data$above_cutoff[i-1]==1]=1
    data$fluseasCDPH[i-1][data$above_cutoff[i]==0 & data$above_cutoff[i-1]==0]=0
    data$fluseasCDPH[i-1][data$above_cutoff[i]==0 & data$above_cutoff[i-1]==1]=data$fluseasCDPH[i-2]
    data$fluseasCDPH[i-1][data$above_cutoff[i]==1 & data$above_cutoff[i-1]==0]=data$fluseasCDPH[i-2]
  }
  
  # create peak week of flu season variable
  data$seasno[(data$yr>=2011 & data$mon<=6)]=1011
  data$seasno[(data$yr>=2011 & data$mon>6)]=1112
  data$seasno[(data$yr>=2012 & data$mon<=6)]=1112
  data$seasno[(data$yr>=2012 & data$mon>6)]=1213
  data$seasno[(data$yr>=2013 & data$mon<=6)]=1213
  data$seasno[(data$yr>=2013 & data$mon>6)]=1314
  data$seasno[(data$yr>=2014 & data$mon<=6)]=1314
  data$seasno[(data$yr>=2014 & data$mon>6)]=1415
  data$seasno[(data$yr>=2015 & data$mon<=6)]=1415
  data$seasno[(data$yr>=2015 & data$mon>6)]=1516
  data$seasno[(data$yr>=2016 & data$mon<=6)]=1516
  data$seasno[(data$yr>=2016 & data$mon>6)]=1617
  data$seasno[(data$yr>=2017 & data$mon<=6)]=1617
  data$seasno[(data$yr>=2017 & data$mon>6)]=1718
  data$seasno[(data$yr>=2018 & data$mon<=6)]=1718
  
  maxili = data %>% 
    group_by(seasno) %>%
    summarise(ILIper = max(ILIper)) %>%
    mutate(peakwk = 1)
  
  data_m = full_join(data, maxili, by=c("seasno","ILIper"))
  data_m = data_m %>% mutate(peakwk=ifelse(is.na(peakwk),0,peakwk))
  
  data_m = data_m %>% mutate(week = week(date))
  
  # rename fluseas and peakwk based on cutoff
  colnames(data_m)[which(colnames(data_m)=="fluseasCDPH")] = paste0("fluseasCDPH_", cutoff)
  colnames(data_m)[which(colnames(data_m)=="peakwk")] = paste0("peakwk_", cutoff)
  
  return(data_m)
}

# Use 2% ILI as the cutoff
cdph = define_cdph_seas(data = cdph, cutoff = 2)

# Use 2.5% ILI as the cutoff (Lower Bound, Sensitivity Analysis)
cdph = define_cdph_seas(data = cdph, cutoff = 2.5)

# Use 3% ILI as the cutoff (Upper Bound, Sensitivity Analysis)
cdph = define_cdph_seas(data = cdph, cutoff = 3)

cdph = cdph %>% select(seasno, date, yr, mon, day, week, ILIper, 
                       fluseasCDPH_2, peakwk_2,
                       fluseasCDPH_2.5, peakwk_2.5,
                       fluseasCDPH_3, peakwk_3)

################################################################################
# Plot ILI by date and indicate flu season definitions and peak week 
################################################################################
cdph_plot = cdph %>% 
  filter(seasno>1011) %>%
  mutate(season = case_when(
    seasno == 1011 ~ "2010-11",
    seasno == 1112 ~ "2011-12",
    seasno == 1213 ~ "2012-13",
    seasno == 1314 ~ "2013-14",
    seasno == 1415 ~ "2014-15",
    seasno == 1516 ~ "2015-16",
    seasno == 1617 ~ "2016-17",
    seasno == 1718 ~ "2017-18"
  ),
  label = case_when(
    seasno<=1314 ~ "Pre-Intervention period",
    season>1314 ~ "Intervention period"
  ),
  cdc = ifelse(week>=40 | week<=20, 1, 0)) %>%
  mutate(cdc = factor(cdc))

plot_2 = ggplot(cdph_plot , aes(y=ILIper, x=date)) + 
  geom_point(aes(col = as.factor(fluseasCDPH_2)))  + 
  geom_line(aes(col = as.factor(fluseasCDPH_2))) +
  facet_wrap(~seasno, ncol = 1, scales = "free") + 
  geom_hline(yintercept=2) +
  theme_complete_bw()

ggsave(plot_2, file = paste0(fig_dir, "cdph_fluseas_cutoff_2.pdf"),
       width = 14, height = 8)

plot = ggplot(cdph_plot, aes(x=date, y=ILIper))+
  geom_line(aes(x=date, y=ifelse(cdph_plot$cdc==1,ILIper, NA)), size=1.8) +
  geom_line() +
  scale_y_continuous(limits=c(0,7.5),breaks=seq(0,7,1), labels=seq(0,7,1)) +
  scale_x_date(date_breaks="months"  , labels = date_format("%b"))+
  # xlab("Month")+
  ylab("Percentage of Influenza-Like Illness Visits")+
  facet_wrap( ~ season, scales = "free") +
  geom_ribbon(aes(ymin = 0, ymax = ILIper), fill = "#FC5400")  +
  geom_area(aes(y = ifelse(ILIper >= 3,3,ILIper)), fill="#FE9866") +
  geom_area(aes(y = ifelse(ILIper >= 2.5,2.5,ILIper)), fill="#FDBE9F") +
  geom_area(aes(y = ifelse(ILIper >= 2,2,ILIper)), fill="#F3F3F3") +
  # geom_area(aes(y = ifelse(ILIper >= 2,2,ILIper), fill=cdc), show.legend=FALSE) +
  # scale_fill_manual(values=c("#F3F3F3", "#FAD0BB"), name="fill") +
  geom_segment(data = cdph_plot %>% filter(date %in% c(cdph$date[cdph$week==40 & cdph$yr<=2017])),
               aes(x = date, xend = date, y = 0, yend = ILIper + 0.05),
               size = 1) +
  geom_segment(data = cdph_plot %>% filter(date %in% c(cdph$date[cdph$week==20 & cdph$yr>2011])),
               aes(x = date, xend = date, y = 0, yend = ILIper + 0.05),
               size = 1) +
  geom_segment(data = cdph_plot %>% filter(date %in% c(cdph$date[cdph$week==40 & cdph$yr<=2017])),
               aes(x = cdph$date[cdph$week==40 & cdph$yr<=2017],
                   xend = cdph$date[cdph$week==20 & cdph$yr > 2011],
                   y = 0,
                   yend = 0),
               size = 1) +
  theme_bw() + 
  theme(strip.text = element_text(size = 16),
        axis.text.y = element_text(size = 14))

ggsave(plot, file = paste0(fig_dir, "cdph_fluseas_cutoff_all.pdf"),
       width = 14, height = 12)



################################################################################
# Cleaning to prepare to merge into other datasets
################################################################################

cdph = cdph %>% select(week, yr, fluseasCDPH_2, fluseasCDPH_2.5, fluseasCDPH_3, 
                       peakwk_2, peakwk_2.5, peakwk_3)

################################################################################
# Impute season for missing weeks at tail ends
################################################################################

before = data.frame(
  week = 1,
  yr = 2011,
  fluseasCDPH_2 = 0,
  fluseasCDPH_2.5 = 0,
  fluseasCDPH_3 = 0,
  peakwk_2 = 0,
  peakwk_2.5 = 0,
  peakwk_3 = 0
)

make_after = function(week){
  df = data.frame(
    week = week,
    yr = 2018,
    fluseasCDPH_2 = 0,
    fluseasCDPH_2.5 = 0,
    fluseasCDPH_3 = 0,
    peakwk_2 = 0,
    peakwk_2.5 = 0,
    peakwk_3 = 0
  )
  
  return(df)
} 

week_list = as.list(seq(31, 52, 1))

after_list = lapply(week_list, function(x) make_after(week = x))
after_df = bind_rows(after_list)

cdph_all = bind_rows(before, cdph, after_df)

################################################################################
# Save Data
################################################################################ 

write_rds(x = cdph_all, path = paste0(data_dir, "cdph_season.RDS"))
