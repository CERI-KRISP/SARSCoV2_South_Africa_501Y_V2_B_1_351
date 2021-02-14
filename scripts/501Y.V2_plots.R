library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
#library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")


############# Figure 1


data3<-read_excel('SA_cummulative_cases_16Jan.xlsx')

data3$days<-as.Date(cut(data3$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data3$date<-as.Date(cut(data3$date,
                        breaks = "week",
                        start.on.monday = FALSE))

R_estimates<-read_excel('ZAF_Re_estimates_16Jan.xlsx')
R_estimates$date<-as.Date(cut(R_estimates$date,
                              breaks = "day",
                              start.on.monday = FALSE))

R_estimates_SA=subset(subset(subset(R_estimates, region=='ZAF'),data_type=="Confirmed cases"),estimate_type=="Cori_slidingWindow")
R_estimates_EC=subset(subset(subset(R_estimates, region=='Eastern Cape'),data_type=="Confirmed cases"),estimate_type=="Cori_slidingWindow")
R_estimates_KZN=subset(subset(subset(R_estimates, region=='KwaZulu-Natal'),data_type=="Confirmed cases"),estimate_type=="Cori_slidingWindow")
R_estimates_WC=subset(subset(subset(R_estimates, region=='Western Cape'),data_type=="Confirmed cases"),estimate_type=="Cori_slidingWindow")
R_estimates_NC=subset(subset(subset(R_estimates, region=='Northern Cape'),data_type=="Confirmed cases"),estimate_type=="Cori_slidingWindow")

excess_deaths<-read_excel('ExcessDeaths_16Jan_v2.xlsx')
excess_deaths$date<-as.Date(cut(excess_deaths$date,
                                breaks = "week",
                                start.on.monday = FALSE))

pEpi_SA<-ggplot(data3,aes(x = days,y=SA_daily))+
  #geom_bar(position='fill',width=1, alpha=0.5,color='black',fill='gold2')+
  #geom_bar(width=1,color='black',fill='gold2')+
  theme_classic()+
  #geom_line()+
  geom_bar(width=1,stat='identity',color='bisque3',fill='bisque3')+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=12))+
  #scale_x_date(date_labels = "%U",date_breaks = "1 week")+
  theme(axis.text.x = element_text(color="black", size=10,angle = 90))+
  scale_x_date(date_labels = "%d-%b",date_breaks = "2 week")+
  geom_line(data = R_estimates_SA, aes(x = date, y = median_R_mean*7000), color = "red", size=0.5) +
  geom_line(data = R_estimates_SA,aes(x=date, y=median_R_lowHPD*7000), color="red",size=0.2) +
  geom_line(data = R_estimates_SA,aes(x=date, y=median_R_highHPD*7000), color="red",size=0.2) +
  geom_hline(yintercept=7000, color='red3', linetype=2) +
  
  geom_line(data = excess_deaths, aes(x = date, y = RSA), linetype='F1',color = "black",size=1) +
  
  #geom_ribbon(data = R_estimates_SA,aes(x=date, ymin=median_R_lowHPD, ymax=median_R_highHPD), fill='tomato', alpha=0.2) +
  xlab(' ')+
  ylab(' ')+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases / Weekly Excess Deaths",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./7000, name="Re"),limits=c(-0.1*7000,3.5*7000)
    
  )+
  ggtitle('SA')


#theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.x = element_line(margin=unit(c(1,0.5,0.5,0.5), "cm"))) 
pEpi_SA

pEpi_EC<-ggplot(data3,aes(x = days,y=EC_daily))+
  #geom_bar(position='fill',width=1, alpha=0.5,color='black',fill='gold2')+
  #geom_bar(width=1,color='black',fill='gold2')+
  theme_classic()+
  geom_bar(width=1,stat='identity',color='orange2',fill='orange2')+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=9))+
  #scale_x_date(date_labels = "%U",date_breaks = "1 week")+
  theme(axis.text.x = element_text(color="black", size=7,angle = 90))+
  scale_x_date(date_labels = "%d-%b",date_breaks = "1 month")+
  geom_line(data = R_estimates_EC, aes(x = date, y = median_R_mean*1100), color = "red", size=0.3) +
  geom_line(data = R_estimates_EC,aes(x=date, y=median_R_lowHPD*1100), color="red",size=0.08) +
  geom_line(data = R_estimates_EC,aes(x=date, y=median_R_highHPD*1100), color="red",size=0.08) +
  geom_hline(yintercept=1100, color='red3', linetype=2) +
  
  geom_line(data = excess_deaths, aes(x = date, y = EC), linetype='F1',color = "black",size=0.5) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1100, name="")
  )+
  xlab(' ')+
  ylab(' ')+
  ggtitle('EC')


#theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.x = element_line(margin=unit(c(1,0.5,0.5,0.5), "cm"))) 
pEpi_EC


pEpi_WC<-ggplot(data3,aes(x = days,y=WC_daily))+
  #geom_bar(position='fill',width=1, alpha=0.5,color='black',fill='gold2')+
  #geom_bar(width=1,color='black',fill='gold2')+
  theme_classic()+
  geom_bar(width=1,stat='identity',color='darkseagreen3',fill='darkseagreen3')+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=9))+
  #scale_x_date(date_labels = "%U",date_breaks = "1 week")+
  theme(axis.text.x = element_text(color="black", size=7,angle = 90))+
  scale_x_date(date_labels = "%d-%b",date_breaks = "1 month")+
  geom_line(data = R_estimates_WC, aes(x = date, y = median_R_mean*1200), color = "red", size=0.3) +
  geom_line(data = R_estimates_WC,aes(x=date, y=median_R_lowHPD*1200), color="red",size=0.08) +
  geom_line(data = R_estimates_WC,aes(x=date, y=median_R_highHPD*1200), color="red",size=0.08) +
  geom_hline(yintercept=1200, color='red3', linetype=2) +
  
  geom_line(data = excess_deaths, aes(x = date, y = WC), linetype='F1',color = "black",size=0.5) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1200, name="")
  )+
  xlab(' ')+
  ylab(' ')+
  ggtitle('WC')


#theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.x = element_line(margin=unit(c(1,0.5,0.5,0.5), "cm"))) 
pEpi_WC


pEpi_NC<-ggplot(data3,aes(x = days,y=NC_daily))+
  #geom_bar(position='fill',width=1, alpha=0.5,color='black',fill='gold2')+
  #geom_bar(width=1,color='black',fill='gold2')+
  theme_classic()+
  geom_bar(width=1,stat='identity',color='lightpink3',fill='lightpink3')+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=9))+
  #scale_x_date(date_labels = "%U",date_breaks = "1 week")+
  theme(axis.text.x = element_text(color="black", size=7,angle = 90))+
  scale_x_date(date_labels = "%d-%b",date_breaks = "1 month")+
  geom_line(data = R_estimates_NC, aes(x = date, y = median_R_mean*75), color = "red", size=0.3) +
  geom_line(data = R_estimates_NC,aes(x=date, y=median_R_lowHPD*75), color="red",size=0.08) +
  geom_line(data = R_estimates_NC,aes(x=date, y=median_R_highHPD*75), color="red",size=0.08) +
  geom_hline(yintercept=75, color='red3', linetype=2) +
  
  geom_line(data = excess_deaths, aes(x = date, y = NC), linetype='F1',color = "black",size=0.5) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./75, name="")
  )+
  xlab(' ')+
  ylab(' ')+
  ggtitle('NC')


#theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.x = element_line(margin=unit(c(1,0.5,0.5,0.5), "cm"))) 
pEpi_NC


pEpi_KZN<-ggplot(data3,aes(x = days,y=KZN_daily))+
  #geom_bar(position='fill',width=1, alpha=0.5,color='black',fill='gold2')+
  #geom_bar(width=1,color='black',fill='gold2')+
  theme_classic()+
  geom_bar(width=1,stat='identity',color='steelblue2',fill='steelblue2')+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=9))+
  #scale_x_date(date_labels = "%U",date_breaks = "1 week")+
  theme(axis.text.x = element_text(color="black", size=7,angle = 90))+
  scale_x_date(date_labels = "%d-%b",date_breaks = "1 month")+
  geom_line(data = R_estimates_KZN, aes(x = date, y = median_R_mean*2000), color = "red", size=0.3) +
  geom_line(data = R_estimates_KZN,aes(x=date, y=median_R_lowHPD*2000), color="red",size=0.08) +
  geom_line(data = R_estimates_KZN,aes(x=date, y=median_R_highHPD*2000), color="red",size=0.08) +
  geom_hline(yintercept=2000, color='red3', linetype=2) +
  
  geom_line(data = excess_deaths, aes(x = date, y = KZN), linetype='F1',color = "black",size=0.5) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./2000, name=""), limits=c(-0.1*2000,3.7*2000)
  )+
  xlab(' ')+
  ylab(' ')+
  ggtitle('KZN')


#theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.x = element_line(margin=unit(c(1,0.5,0.5,0.5), "cm"))) 
pEpi_KZN

grid.arrange(pEpi_SA, arrangeGrob(pEpi_EC, pEpi_WC,pEpi_KZN,pEpi_NC), ncol = 2, widths=c(0.5,0.5))




############ Figure 2

####2A
# load pacakges
library(ggtree)
library(tidyverse)
library(tidytree)
library(ape)
library(treeio)

tree<-read.newick('nextstrain_groups_ngs-sa_COVID19-ZA-2021.01.18_timetree.nwk')
metadata_df <- read_excel("SA_dataset_12Jan2021_n=2882_lineages_annotated.xlsx")

tree <- groupClade(tree,.node=c(7448))
p<-ggtree(tree, mrsd="2020-12-10", aes(color=group),as.Date=TRUE,size=0.2) + theme_tree2()+
  #scale_colour_manual(values=c("gray40", "springgreen4"))+
  scale_colour_manual(values=c("gray50", "goldenrod2"))+
  #geom_range(range='height_0.95_HPD', color='black', alpha=0.8, size=2)+
  #geom_tiplab(aes(
  #subset=(grepl('Tygerberg',label,fixed=TRUE)==TRUE)),linesize=2,color='black',align=F)+
  #scale_colour_manual(values=c("salmon1", "gold2", "palegreen3",'cadetblue2','mediumpurple2','plum2'))+
  geom_tippoint(size=1, align=F, fill='gray90', color='gray40',shape=21)+
  #geom_tippoint(aes(
  #  subset=(!grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=3, align=F, fill='cadetblue', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=1.5, align=F, fill='red', color='black',shape=21)+
  scale_x_date(date_labels = "%b-%y",date_breaks = "1 month")+
  theme(axis.text=element_text(size=10))+
  expand_limits(y = 5500)+
  theme(axis.text.x = element_text(size=8,angle=90))+
  theme(legend.position = 'none')
  #geom_text(aes(label=node), hjust=-.3, size=2)

p

#ggsave('SA_n2882_ML_tree_main_nodes_v2.pdf', width = 30, height = 400, units = "cm",limitsize = FALSE)

pangolin_modified_main

p1 <- p %<+% metadata_df + 
  geom_tippoint(aes(
   subset=(pangolin_modified_main=='S.501Y.V2')),size=1.5, align=F, fill='gold', color='black',shape=21)+
 # geom_tiplab(aes(
  #  subset=(pangolin_modified_main=='S.501Y.V2')), size=0.5)
  geom_tippoint(aes(
    subset=(pangolin_lineage_automatic=='B.1.1.206')),size=2, align=F, fill='#440154FF', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(pangolin_modified_main=='B.1.1.56')),size=2, align=F, fill='#39568CFF', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(pangolin_modified_main=='C.1')),size=2, align=F, fill='#29AF7FFF', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(Nextstrain_clade=='20H/501Y.V2')),size=2, align=F, fill='gold', color='black',shape=21)
#geom_text(aes(label=node), hjust=-.3)
#scale_color_manual(values = myColor)
p1

#ggsave('SA_n2882_ML_tree4_labels.pdf', width = 30, height = 150, units = "cm",limitsize = FALSE)


######2B


tree <- read.beast("501_sequences_n341_EditSTEXP100ML_Combined.tree") #Obtained from BEAST run as described in the methods
df <- read_excel('list_501Yv2_n341_PhyloData.xlsx')

#tree <- groupClade(tree,.node=c(205))

p <- ggtree(tree, mrsd="2020-12-10", as.Date=TRUE,color='goldenrod2',size=0.8) + theme_tree2()
  #scale_color_manual(values=c('grey50','goldenrod2'))

#yellow_colors_dark1<-randomcoloR::randomColor(count=3, hue="yellow", luminosity = "random")
#yellow_colors_dark<-randomcoloR::randomColor(count=4, hue="yellow", luminosity = "random")
#yellow_colors_dark1<-randomcoloR::randomColor(count=4, hue="yellow", luminosity = "random")

p <- p %<+% df + 
  #geom_tippoint(size=3, shape=21,fill='red',align=F,color='black')+
  geom_tippoint(aes(shape=Province, fill=Province),size=3.5, align=F,color='black')+
  scale_shape_manual(values=c(21, 22, 24,23))+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  scale_x_date(date_labels = "%d-%b",date_breaks = "2 week")+
  scale_fill_manual(values=c('lemonchiffon2', 'darkgoldenrod3', 'yellow', 'lightgoldenrod4'))+
  #scale_fill_manual(values=yellow_colors_dark)+
  #geom_range("length_range", color='grey', size=200, alpha=.5)+
  theme(axis.text=element_text(size=12, angle=90))+
  expand_limits(y =350)
  #geom_tiplab()
#geom_text(aes(label=node), hjust=-.3, size=2)
p

ggsave('cluster_n341_tree_labels.pdf', width = 50, height = 120, units = "cm",limitsize = FALSE)



###2C
data2<-read_excel('SA_dataset_12Jan2021_n=2882_lineages_annotated.xlsx')

data2$days<-as.Date(cut(data2$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data2$date<-as.Date(cut(data2$date,
                        breaks = "week",
                        start.on.monday = FALSE))


#myColor3 <- randomcoloR::distinctColorPalette(k = 60)

#myColor4 <- randomcoloR::distinctColorPalette(k = 60)
P_pangolin_main<-data2 %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=pangolin_modified_main))+
  geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  #geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%d-%b",date_breaks = "2 week")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=12))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","501Y.V2 (B.1.1.351)","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = myColor4)+
  #scale_fill_manual(values=c('red','gold2','cadetblue3','chartreuse4',"gray"),labels = c("B.1.1.54", "B.1.1.56","B.1.106","C.1","Others"))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=12))+
  #theme(legend.position = "bottom")+
  #theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  #theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  #ylab('Genome Count')
  ylab('Proportion of Genomes')

P_pangolin_main


############ Figure 3


####3A

cluster_mutations <- read.csv(file='501Y.V2_n344_spike_mutations_table.csv', sep=",", header=T) #table obtained after passing a nextclade file (501Y.V2_nextclade_n344.xlsx) to the python code spike_mutations.py

n=344
p_cluster_S_mutations <- ggplot(cluster_mutations, aes(x = location, x1=mutation))+
  geom_histogram(binwidth=1,fill="black", col="black") + theme_bw() +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text=element_text(size=10))+
  #geom_bar(position="fill")+
  xlab("Spike protein location")+
  ylab(" ")+
  scale_x_continuous(limits = c(0,1274),breaks = seq(0, 1274, by = 100))+
  #scale_y_continuous(labels = percent_format()) +
  #scale_y_continuous(breaks = seq(0, 1600/5, by = 200/5))+
  #geom_hline(yintercept=100, linetype="dashed", color = "red")+
  geom_hline(yintercept=.1*n, linetype="dashed", color = "grey", alpha=0.7)+
  geom_label(x=-20, y=.1*n, label="10%", color='grey')
#ggtitle("South Africa - Humans new cluster Oct/Nov 2020")+
  #stat_bin(binwidth=1, geom="text_repel",size=2.5,srt=90,color='red3',position=position_stack(vjust = 0.4), aes(label=ifelse(..count..>.1*n, x1, "")))

p_cluster_S_mutations


####3B

data2<-read_excel('dataset_V3/501Y.V2_cluster_metadata_n344.xlsx')

data2$days<-as.Date(cut(data2$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data2$date<-as.Date(cut(data2$date,
                        breaks = "week",
                        start.on.monday = FALSE))

# plot

myColor_yellow<-randomcoloR::randomColor(count=8, hue="orange", luminosity = "dark")
pie(rep(1, 8), col = myColor_yellow)

p <- ggplot( data2, aes(x=date)) +
  geom_histogram(data=data2,aes(y=..count..),alpha=0.6, binwidth = 3.5, alpha=0.3,fill='grey80') +
  #geom_histogram(data=subset(data2, D80A=='A'),aes(y=..density..),alpha=0.6, binwidth = 5, color='indianred') +
  geom_line(data=subset(data2, L18F=='F'),size=1.2,stat='count', aes(color="L18F"))+
  geom_line(data=subset(data2, D80A=='A'),size=1.2,stat='count', aes(color="D80A"))+
  geom_line(data=subset(data2, D215G=='G'),size=1.2,stat='count', aes(color="D215G"))+
  geom_line(data=subset(data2, R246I=='I'),size=1.2,stat='count', aes(color="R246I"))+
  geom_line(data=subset(data2, K417N=='N'),size=1.2,stat='count', aes(color="K417N"))+
  geom_line(data=subset(data2, E484K=='K'),size=1.2,stat='count', aes(color="E484K"))+
  geom_line(data=subset(data2, N501Y=='Y'),size=1.2,stat='count', aes(color="N501Y"))+
  geom_line(data=subset(data2, A701V=='V'),size=1.2,stat='count', aes(color="A701V"))+
  geom_line(data=subset(data2, del_L242_244L=='---'),size=1.2,stat='count', aes(color="del_L242_244L"))+
  scale_color_manual(values=c('darkseagreen3','darkorange3', 'red2','cadetblue3','goldenrod2','tan4','snow4','hotpink3','mediumorchid4'), name='Mutations') +
  #scale_color_simpsons(name="Mutations")+
  #scale_color_viridis(discrete=TRUE,option="magma", name="Mutations")+
  scale_x_date(date_labels = "%d-%b",date_breaks = "1 week")+
  theme_classic() +
  theme(axis.text.x = element_text(color="black", size=10,angle = 90))+
  xlab("") +
  ylab("Number of Genomes with mutation")

p


###3C

#following tables obtained after passing the relevant nextclade file for each cluster to the python code spike_mutations_quantify.py
dataC.1<-read_excel('C.1_spike_mutations_quantification.xlsx')
dataB.1.1.56<-read_excel('B.1.1.56_spike_mutations_quantification.xlsx')
dataB.1.1.54<-read_excel('B.1.1.54_spike_mutations_quantification.xlsx')
dataZA.cluster<-read_excel('501Y.V2_n344_spike_mutations_quantification.xlsx')

data_all<-rbind(dataC.1,dataB.1.1.56,dataB.1.1.54,dataZA.cluster)

p_nt_whole <- ggplot(data=data_all, aes(Lineage, totalMutations, fill=Lineage))+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1),geom = "pointrange", color = "black" )+
  scale_fill_viridis(discrete=TRUE, name='Lineages')+
  xlab('')+ ylab('number of mutations')+ggtitle("Nucleotide")+
  scale_x_discrete(labels= c('B.1.1.54', 'B.1.1.56', 'C.1', '501Y.V2'))+
  theme_classic()+
  ylim(0,35)+
  theme(legend.position="none", axis.text.x = element_text(size=10,angle=45, hjust=1),axis.text.y = element_text(size=10))

p_nt_whole

p_ac_whole <- ggplot(data=data_all, aes(Lineage, totalAminoacidSubstitutions, fill=Lineage))+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1),geom = "pointrange", color = "black" )+
  scale_fill_viridis(discrete=TRUE, name='Lineages')+
  xlab('')+ ylab('')+ggtitle("Amino Acid")+
  scale_x_discrete(labels= c('B.1.1.54', 'B.1.1.56', 'C.1', '501Y.V2'))+
  ylim(0,35)+
  theme_classic()+
  theme(legend.position="none", axis.text.x = element_text(size=10,angle=45, hjust=1),axis.text.y = element_text(size=10))

p_ac_whole

p_nt_spike <- ggplot(data=data_all, aes(Lineage, Num_spike_nt_changes, fill=Lineage))+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1),geom = "pointrange", color = "black" )+
  scale_fill_viridis(discrete=TRUE, name='Lineages')+
  xlab('')+ ylab('')+ggtitle("Nucleotide")+
  scale_x_discrete(labels= c('B.1.1.54', 'B.1.1.56', 'C.1', '501Y.V2'))+
  theme_classic()+
  ylim(0,35)+
  theme(legend.position="none", axis.text.x = element_text(size=10,angle=45, hjust=1),axis.text.y = element_text(size=10))

p_nt_spike

p_ac_spike <- ggplot(data=data_all, aes(Lineage, Num_spike_mutations, fill=Lineage))+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1),geom = "pointrange", color = "black" )+
  scale_fill_viridis(discrete=TRUE, name='Lineages')+
  xlab('')+ ylab('')+ggtitle("Amino Acid")+
  scale_x_discrete(labels= c('B.1.1.54', 'B.1.1.56', 'C.1', '501Y.V2'))+
  theme_classic()+
  ylim(0,35)+
  theme(legend.position="none", axis.text.x = element_text(size=10,angle=45, hjust=1),axis.text.y = element_text(size=10))

p_ac_spike

p_ac_RBD <- ggplot(data=data_all, aes(Lineage, Num_RBD_mutations, fill=Lineage))+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1),geom = "pointrange", color = "black" )+
  scale_fill_viridis(discrete=TRUE, name='Lineages')+
  xlab('')+ ylab('number of mutations')+ggtitle("RBD-Amino Acid")+
  scale_x_discrete(labels= c('B.1.1.54', 'B.1.1.56', 'C.1', '501Y.V2'))+
  theme_classic()+
  theme(legend.position="none", axis.text.x = element_text(size=10,angle=45, hjust=1),axis.text.y = element_text(size=10))

p_ac_RBD


plot_grid(p_nt_whole,p_ac_whole,p_nt_spike,p_ac_spike,p_ac_RBD, ncol=5)

plot_grid(p_nt_whole,p_ac_whole)
plot_grid(p_nt_spike,p_ac_spike)


plot_grid(p_nt_whole,p_ac_whole,p_nt_spike,p_ac_spike, ncol=4)



################# Supplementary Figures
#_______________________________________________________
#_______________________________________________________
#_______________________________________________________
#_______________________________________________________
#_______________________________________________________


#Fig S2 - Temporal signal plot with TempEst data

tempest_data_501Yv2<-read_excel('list_501Yv2_n341_PhyloData.xlsx')
tempest_data_501Yv2$date2=lubridate::date_decimal(tempest_data_501Yv2$date)
tempest_data_501Yv2$date2<-as.Date(cut(tempest_data_501Yv2$date2,
                        breaks = "days",
                        start.on.monday = FALSE))

p_tempest_501Yv2<-ggplot(tempest_data_501Yv2, aes(date2,distance))+
  geom_point(fill='gold2',size=2,color='black',shape=21)+
  #scale_fill_manual(values=c('goldenrod1', 'red', 'darkorange'))+
  geom_smooth(method=lm,se=T, color='black')+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=15))+
  theme(axis.text.x =element_text(size=15, angle=90))+
  theme(axis.title=element_text(size=15))+
  scale_x_date(date_labels = "%d-%b",date_breaks = "2 weeks")+
  ylab("Distance")+
  xlab("Date")+
  #ggtitle("Tempest")+
  scale_y_continuous(labels = scientific)+
  ggtitle('501Y.V2')

p_tempest_501Yv2


tempest_data_B1154<-read_excel('B.1.1.54_PhyloData.xlsx')
tempest_data_B1154$date2=lubridate::date_decimal(tempest_data_B1154$date)
tempest_data_B1154$date2<-as.Date(cut(tempest_data_B1154$date2,
                                       breaks = "days",
                                       start.on.monday = FALSE))

p_tempest_B1154<-ggplot(tempest_data_B1154, aes(date2,distance))+
  geom_point(fill='#440154FF',size=1,color='black',shape=21)+
  #scale_fill_manual(values=c('goldenrod1', 'red', 'darkorange'))+
  geom_smooth(method=lm,se=T, color='black')+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=10))+
  theme(axis.text.x =element_text(size=10, angle=90))+
  theme(axis.title=element_text(size=10))+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  ylab("Distance")+
  xlab("Date")+
  #ggtitle("Tempest")+
  scale_y_continuous(labels = scientific)+
  ggtitle('B.1.1.54')

p_tempest_B1154

tempest_data_B1156<-read_excel('B.1.1.56_PhyloData.xlsx')
tempest_data_B1156$date2=lubridate::date_decimal(tempest_data_B1156$date)
tempest_data_B1156$date2<-as.Date(cut(tempest_data_B1156$date2,
                                      breaks = "days",
                                      start.on.monday = FALSE))

p_tempest_B1156<-ggplot(tempest_data_B1156, aes(date2,distance))+
  geom_point(fill='#39568CFF',size=1,color='black',shape=21)+
  #scale_fill_manual(values=c('goldenrod1', 'red', 'darkorange'))+
  geom_smooth(method=lm,se=T, color='black')+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=10))+
  theme(axis.text.x =element_text(size=10, angle=90))+
  theme(axis.title=element_text(size=10))+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  ylab("Distance")+
  xlab("Date")+
  #ggtitle("Tempest")+
  scale_y_continuous(labels = scientific)+
  ggtitle('B.1.1.56')

p_tempest_B1156


tempest_data_C1<-read_excel('C.1_PhyloData.xlsx')
tempest_data_C1$date2=lubridate::date_decimal(tempest_data_C1$date)
tempest_data_C1$date2<-as.Date(cut(tempest_data_C1$date2,
                                      breaks = "days",
                                      start.on.monday = FALSE))



p_tempest_C1<-ggplot(tempest_data_C1, aes(date2,distance))+
  geom_point(fill='#20A387FF',size=1,color='black',shape=21)+
  #scale_fill_manual(values=c('goldenrod1', 'red', 'darkorange'))+
  geom_smooth(method=lm,se=T, color='black')+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=10))+
  theme(axis.text.x =element_text(size=10, angle=90))+
  theme(axis.title=element_text(size=10))+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  ylab("Distance")+
  xlab("Date")+
  #ggtitle("Tempest")+
  scale_y_continuous(labels = scientific)+
  ggtitle('C.1')

p_tempest_C1

grid.arrange(p_tempest_501Yv2, arrangeGrob(p_tempest_B1154, p_tempest_B1156,p_tempest_C1), ncol = 2, widths=c(0.6,0.3))


ggplotRegression <- function(fit){
  
  
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(lm(distance ~ date, data = tempest_data_501Yv2))+scale_x_continuous(labels = scientific)

ggplotRegression(lm(distance ~ date, data = tempest_data_B1154))+scale_x_continuous(labels = scientific)

ggplotRegression(lm(distance ~ date, data = tempest_data_B1156))+scale_x_continuous(labels = scientific)

ggplotRegression(lm(distance ~ date, data = tempest_data_C1))+scale_x_continuous(labels = scientific)



#Fig S6 - Progression of all lineages in SA

#myColor3 <- randomcoloR::distinctColorPalette(k = 65)

#myColor4 <- randomcoloR::distinctColorPalette(k = 65)
#myColor_dark<-randomcoloR::randomColor(count=65, hue='random',luminosity = "random")
P_pangolin_all<-data2 %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=pangolin_modified))+
  geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  #geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%d-%b",date_breaks = "2 week")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=12))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","501Y.V2 (B.1.1.351)","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor4, name='PANGOLIN lineages')+
  #scale_fill_manual(values=c('red','gold2','cadetblue3','chartreuse4',"gray"),labels = c("B.1.1.54", "B.1.1.56","B.1.106","C.1","Others"))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=11))+
  theme(legend.title = element_text(size=12))+
  #theme(legend.position = "bottom")+
  #theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  #theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  #ylab('Genome Count')
  ylab('Proportion of Genomes')

P_pangolin_all


#Fig S8 - mutations map (all nucleotides)

#following table obtained after passing the relevant nextclade file to the python code mutation_list.py
variant_mutations <- read.csv(file='501_sequences_n341_nucleotide__mutations_table.csv', sep=",", header=T)


p_mutations_1 <- ggplot(variant_mutations, aes(x = location, x1=mutation))+
  geom_histogram(binwidth=1,fill="black", col="black") + theme_bw() +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text=element_text(size=10))+
  #geom_bar(position="fill")+
  xlab("Genome Location")+
  ylab(" ")+
  scale_x_continuous(limits = c(0,29903),breaks = seq(0, 29903, by = 5000))+
  #scale_y_continuous(labels = percent_format()) +
  #scale_y_continuous(breaks = seq(0, 1600/5, by = 200/5))+
  #geom_hline(yintercept=100, linetype="dashed", color = "red")+
  geom_hline(yintercept=34, linetype="dashed", color = "blue")+
  geom_label(x=-50, y=34, label="10%", color='grey')+
  ggtitle("S501Y.V2 Nucleotide Subtitutions")+
  ylab('Number of Genomes')
  #stat_bin(binwidth=1, geom="text_repel",srt=90,color='blue',position=position_stack(vjust=0.5), aes(label=ifelse(..count..>34, x1, "")))

p_mutations_1
ggsave(
  filename = "plots.pdf", 
  plot = p_mutations_1, 
  width = 10, height = 2,
  limitsize = FALSE
)

#Fig S9 - Nexus tree


tree1<-read.newick('tree_raw_n2882.nwk')

p<-ggtree(tree1, color="gray40",size=0.35) + theme_tree2()+
  #scale_colour_manual(values=c("gray40", "springgreen4"))+
  scale_colour_manual(values=c("gray50", "goldenrod2"))+
  geom_tippoint(size=1.5, align=F, fill='gray90', color='gray40',shape=21)+
  geom_tippoint(aes(
    subset=(grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='red', color='black',shape=21)+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.text=element_text(size=10))+
  expand_limits(y = 5500)+
  theme(axis.text.x = element_text(size=10,angle=90))
#theme(legend.position = 'none')
#geom_text(aes(label=node), hjust=-.3, size=2)

p

p1 <- p %<+% metadata_df + 
  #geom_tippoint(aes(fill=Lineage),size=2, align=F, color='black',shape=21)+
  geom_tippoint(aes(subset=(pangolin_lineage_automatic=='B.1.351')),size=2, align=F, fill='gold', color='black',shape=21)+
  xlab('Distance')
  #geom_tippoint(aes(
  #   subset=(grepl('UCT',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='gold', color='black',shape=21)
  #geom_text(aes(label=node), hjust=-.3)
  #scale_fill_manual(values = Dark_Colors, name='Lineage')
  #scale_fill_viridis(discrete=TRUE, name='Lineage',option="magma" )
p1



# Fig S7 - Regional trees

EC_tree<-read.newick('EC.fasta.treefile_timetree_tree.nwk')
EC_df <- read_excel("EC_tree_metadata.xlsx")

p<-ggtree(EC_tree, mrsd="2020-12-10", color="gray40",as.Date=TRUE,size=0.35) + theme_tree2()+
  #p<-ggtree(tree, mrsd="2020-11-25", aes(color=group),as.Date=TRUE,size=0.35) + theme_tree2()+
  #scale_colour_manual(values=c("gray40", "springgreen4"))+
  scale_colour_manual(values=c("gray50", "goldenrod2"))+
  #geom_range(range='height_0.95_HPD', color='black', alpha=0.8, size=2)+
  #geom_tiplab(aes(
  #subset=(grepl('Tygerberg',label,fixed=TRUE)==TRUE)),linesize=2,color='black',align=F)+
  #scale_colour_manual(values=c("salmon1", "gold2", "palegreen3",'cadetblue2','mediumpurple2','plum2'))+
  geom_tippoint(size=1.5, align=F, fill='gray90', color='gray40',shape=21)+
  #geom_tippoint(aes(
  #  subset=(!grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=3, align=F, fill='cadetblue', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='red', color='black',shape=21)+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.text=element_text(size=10))+
  #expand_limits(y = 1600)+
  theme(axis.text.x = element_text(size=10,angle=90))
#theme(legend.position = 'none')
#geom_text(aes(label=node), hjust=-.3, size=2)

p

p1 <- p %<+% EC_df + 
  geom_tippoint(aes(fill=pangolin_lineage_modified),size=2, align=F, color='black',shape=21)+
  geom_tippoint(aes(subset=(Nextstrain_clade=='20H/501Y.V2')),size=2, align=F, fill='gold', color='black',shape=21)+
  #geom_tippoint(aes(
  #   subset=(grepl('UCT',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='gold', color='black',shape=21)
  #geom_text(aes(label=node), hjust=-.3)
  #scale_fill_manual(values = Dark_Colors, name='Lineage')
  scale_fill_viridis(discrete=TRUE, name='Lineage',option="magma" )
p1


KZN_tree<-read.nexus("KZN_timetree_tree.nwk.nwk")
KZN_df <- read_excel("KZN_tree_metadata.xlsx")
p2<-ggtree(KZN_tree, mrsd="2020-12-10", color="gray40",as.Date=TRUE,size=0.35) + theme_tree2()+
  #p<-ggtree(tree, mrsd="2020-11-25", aes(color=group),as.Date=TRUE,size=0.35) + theme_tree2()+
  #scale_colour_manual(values=c("gray40", "springgreen4"))+
  scale_colour_manual(values=c("gray50", "goldenrod2"))+
  #geom_range(range='height_0.95_HPD', color='black', alpha=0.8, size=2)+
  #geom_tiplab(aes(
  #subset=(grepl('Tygerberg',label,fixed=TRUE)==TRUE)),linesize=2,color='black',align=F)+
  #scale_colour_manual(values=c("salmon1", "gold2", "palegreen3",'cadetblue2','mediumpurple2','plum2'))+
  geom_tippoint(size=1.5, align=F, fill='gray90', color='gray40',shape=21)+
  #geom_tippoint(aes(
  #  subset=(!grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=3, align=F, fill='cadetblue', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='red', color='black',shape=21)+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.text=element_text(size=10))+
  #expand_limits(y = 1600)+
  theme(axis.text.x = element_text(size=10,angle=90))
#theme(legend.position = 'none')
#geom_text(aes(label=node), hjust=-.3, size=2)

p2

p3 <- p2 %<+% KZN_df + 
  geom_tippoint(aes(fill=pangolin_lineage_modified),size=2, align=F, color='black',shape=21)+
  geom_tippoint(aes(subset=(Nextstrain_clade=='20H/501Y.V2')),size=2, align=F, fill='gold', color='black',shape=21)+
  #geom_tippoint(aes(
  #   subset=(grepl('UCT',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='gold', color='black',shape=21)
  #geom_text(aes(label=node), hjust=-.3)
  #scale_fill_manual(values = Dark_Colors, name='Lineage')
  scale_fill_viridis(discrete=TRUE, name='Lineage',option="magma" )
p3


WC_tree<-read.newick('WC_timetree_tree.nwk')
WC_df <- read_excel("WC_tree_metadata.xlsx")


p4<-ggtree(WC_tree, mrsd="2020-12-08", color="gray40",as.Date=TRUE,size=0.35) + theme_tree2()+
  #p<-ggtree(tree, mrsd="2020-11-25", aes(color=group),as.Date=TRUE,size=0.35) + theme_tree2()+
  #scale_colour_manual(values=c("gray40", "springgreen4"))+
  scale_colour_manual(values=c("gray50", "goldenrod2"))+
  #geom_range(range='height_0.95_HPD', color='black', alpha=0.8, size=2)+
  #geom_tiplab(aes(
  #subset=(grepl('Tygerberg',label,fixed=TRUE)==TRUE)),linesize=2,color='black',align=F)+
  #scale_colour_manual(values=c("salmon1", "gold2", "palegreen3",'cadetblue2','mediumpurple2','plum2'))+
  geom_tippoint(size=1.5, align=F, fill='gray90', color='gray40',shape=21)+
  #geom_tippoint(aes(
  #  subset=(!grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=3, align=F, fill='cadetblue', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='red', color='black',shape=21)+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.text=element_text(size=10))+
  expand_limits(y = 600)+
  theme(axis.text.x = element_text(size=10,angle=90))
#theme(legend.position = 'none')
#geom_text(aes(label=node), hjust=-.3, size=2)

p4

p5 <- p4 %<+% WC_df + 
  geom_tippoint(aes(fill=pangolin_lineage_modified),size=2, align=F, color='black',shape=21)+
  geom_tippoint(aes(subset=(Nextstrain_clade=='20H/501Y.V2')),size=2, align=F, fill='gold', color='black',shape=21)+
  #geom_tippoint(aes(
  #   subset=(grepl('UCT',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='gold', color='black',shape=21)
  #geom_text(aes(label=node), hjust=-.3)
  #scale_fill_manual(values = Dark_Colors, name='Lineage')
  scale_fill_viridis(discrete=TRUE, name='Lineage',option="magma" )
p5



NC_tree<-read.newick('NC_timetree_tree.nwk')
NC_df <- read_excel("NC_metadata.xlsx")

p6<-ggtree(NC_tree, mrsd="2020-12-08", color="gray40",as.Date=TRUE,size=0.35) + theme_tree2()+
  #p<-ggtree(tree, mrsd="2020-11-25", aes(color=group),as.Date=TRUE,size=0.35) + theme_tree2()+
  #scale_colour_manual(values=c("gray40", "springgreen4"))+
  scale_colour_manual(values=c("gray50", "goldenrod2"))+
  #geom_range(range='height_0.95_HPD', color='black', alpha=0.8, size=2)+
  #geom_tiplab(aes(
  #subset=(grepl('Tygerberg',label,fixed=TRUE)==TRUE)),linesize=2,color='black',align=F)+
  #scale_colour_manual(values=c("salmon1", "gold2", "palegreen3",'cadetblue2','mediumpurple2','plum2'))+
  geom_tippoint(size=1.5, align=F, fill='gray90', color='gray40',shape=21)+
  #geom_tippoint(aes(
  #  subset=(!grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=3, align=F, fill='cadetblue', color='black',shape=21)+
  geom_tippoint(aes(
    subset=(grepl('SouthAfrica',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='red', color='black',shape=21)+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.text=element_text(size=6))+
  #expand_limits(y = 600)+
  theme(axis.text.x = element_text(size=6,angle=90))
#theme(legend.position = 'none')
#geom_text(aes(label=node), hjust=-.3, size=2)

p6

p7 <- p6 %<+% NC_df + 
  geom_tippoint(aes(fill=pangolin_lineage_modified),size=2, align=F, color='black',shape=21)+
  geom_tippoint(aes(subset=(Nextstrain_clade=='20H_501Y.V2')),size=2, align=F, fill='gold', color='black',shape=21)+
  #geom_tippoint(aes(
  #   subset=(grepl('UCT',label,fixed=TRUE)==TRUE)),size=2, align=F, fill='gold', color='black',shape=21)
  #geom_text(aes(label=node), hjust=-.3)
  #scale_fill_manual(values = Dark_Colors, name='Lineage')
  scale_fill_viridis(discrete=TRUE, name='Lineage',option="magma" )
p7

plot_grid(p1,p3,p5,p7,ncol=4, rel_widths = c(0.25,0.35,0.25,0.15))


#Fig S1 - Clinic count


myColor_EC<-randomcoloR::randomColor(count=41, hue="orange", luminosity = "dark")
#randomColor(count = 1, hue = c(" ", "random", "red", "orange", "yellow",
#                               "green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ",
#                                                                                             "random", "light", "bright", "dark"))

myColor_KZN<-randomcoloR::randomColor(count=22, hue="blue", luminosity = "dark")

myColor_WC<-randomcoloR::randomColor(count=36, hue="green", luminosity = "dark")

myColor_NC<-randomcoloR::randomColor(count=3, hue="pink", luminosity = "dark")


all_colors=c(myColor_EC,myColor_KZN,myColor_NC,myColor_WC)


data3<-read_excel('spike_hypermutation_locations_clinics_updated_16Jan.xlsx')

P1<-ggplot(data=subset(data3, !is.na(strain)), mapping = aes(x = Province, fill=Province_District_HealthFacility))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=0.5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  #xlab("Sampling Date")+ 
  #scale_x_date(date_labels = "%d-%b",date_breaks = "2 week")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_simpsons()+
  #scale_fill_viridis(discrete=TRUE) +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = all_colors)+
  #scale_fill_manual(values=c('red','gold2','cadetblue3','chartreuse4',"gray"),labels = c("B.1.1.54", "B.1.1.56","B.1.106","C.1","Others"))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.position = 'none')+
  xlab('Province')+
  ylab('Clinic Count')+
  #ylab('Proportion of Genomes')
  theme(plot.margin = unit(c(0, 0, 3, 0), "cm"))

P1


P2<-ggplot(data=subset(data3, !is.na(strain)), mapping = aes(x = Province_District, fill=Province_District_HealthFacility))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=0.5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  #xlab("Sampling Date")+ 
  #scale_x_date(date_labels = "%d-%b",date_breaks = "2 week")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_simpsons()+
  #scale_fill_viridis(discrete=TRUE) +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = all_colors)+
  #scale_fill_manual(values=c('red','gold2','cadetblue3','chartreuse4',"gray"),labels = c("B.1.1.54", "B.1.1.56","B.1.106","C.1","Others"))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = 'under')+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('District/Sub-District')+
  ylab('Clinic Count')
#ylab('Proportion of Genomes')

P2

plot_grid(P1,P2, rel_widths = c(0.2,0.8))

#Supp Fig - minority variants 

minority_data<-read_excel('run50_51_55_snvs_isnvs_lofreq.xlsx')


minority_1059_pos<-subset(minority_data, POS=='1059')

p_1059<-ggplot(minority_1059_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('1059 T')
p_1059

minority_5230_pos<-subset(minority_data, POS=='5230')

p_5230<-ggplot(minority_5230_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('5230 T')
p_5230


minority_8660_pos<-subset(minority_data, POS=='8660')

p_8660<-ggplot(minority_8660_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('8660 T')
p_8660

minority_8964_pos<-subset(minority_data, POS=='8964')

p_8964<-ggplot(minority_8964_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('8964 T')
p_8964


minority_10323_pos<-subset(minority_data, POS=='10323')

p_10323<-ggplot(minority_10323_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('10323 G')
p_10323

minority_13843_pos<-subset(minority_data, POS=='13843')

p_13843<-ggplot(minority_13843_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('13843 T')
p_13843

minority_17999_pos<-subset(minority_data, POS=='17999')

p_17999<-ggplot(minority_17999_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('17999 T')
p_17999



minority_21614_pos<-subset(minority_data, POS=='21614')

p_21614<-ggplot(minority_21614_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('21614 T')
p_21614

minority_21801_pos<-subset(minority_data, POS=='21801')

p_21801<-ggplot(minority_21801_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('21801 C')
p_21801

minority_22206_pos<-subset(minority_data, POS=='22206')

p_22206<-ggplot(minority_22206_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('22206 G')
p_22206

minority_22299_pos<-subset(minority_data, POS=='22299')

p_22299<-ggplot(minority_22299_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('22299 T')
p_22299


minority_23012_pos<-subset(minority_data, POS=='23012')

p_23012<-ggplot(minority_23012_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('23012 A')
p_23012


minority_22813_pos<-subset(minority_data, POS=='22813')

p_22813<-ggplot(minority_22813_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('22813 T')
p_22813

minority_23063_pos<-subset(minority_data, POS=='23063')

p_23063<-ggplot(minority_23063_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('23063 T')
p_23063

minority_23664_pos<-subset(minority_data, POS=='23664')

p_23664<-ggplot(minority_23664_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('23664 T')
p_23664

minority_25563_pos<-subset(minority_data, POS=='25563')

p_25563<-ggplot(minority_25563_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('25563 T')
p_25563


minority_25904_pos<-subset(minority_data, POS=='25904')

p_25904<-ggplot(minority_25904_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('25904 T')
p_25904


minority_26456_pos<-subset(minority_data, POS=='26456')

p_26456<-ggplot(minority_26456_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('26456 T')
p_26456


minority_28887_pos<-subset(minority_data, POS=='28887')

p_28887<-ggplot(minority_28887_pos, aes(x=reorder(Sample, AF), y=AF, group=1))+
  geom_point(size=0.5)+
  geom_line()+
  geom_point(size=0.5,aes(x=reorder(Sample, AF), y=1-AF, group=1),color='grey')+
  geom_line(aes(x=reorder(Sample, AF), y=1-AF, group=1), color='grey')+
  theme_classic()+
  ylim(0,1)+
  xlab(" ")+
  ylab(' ')+
  #scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  #theme(axis.text.x = element_text(color="black", size=4))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))+
  ggtitle('28887 T')
p_28887


plot_grid(p_1059,p_5230,p_8660,p_8964,p_10323,p_17999,p_21614,p_21801,p_22206,p_22299,p_22813,p_23012,p_23063,p_23664,p_25563,p_25904,p_26456, p_28887, ncol=3)



###old

data2<-read_excel('world_metadata_2020-12-06.xlsx')

data2$days<-as.Date(cut(data2$date,
                        breaks = "day",
                        start.on.monday = FALSE))

data2$date<-as.Date(cut(data2$date,
                        breaks = "week",
                        start.on.monday = FALSE))

data2<-subset(data2, division=='Eastern Cape')

pRug<-data2 %>%
  mutate(Collection_date=as.POSIXct(days)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  #ggplot(data=subset(data2, !is.na(pangolin_lineage)), mapping = aes(x = date))+
  ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = days))+
  #geom_bar(position='fill',width=1, alpha=0.5,color='black',fill='gold2')+
  #geom_bar(width=1,color='black',fill='gold2')+
  #geom_bar(width=1)+
  scale_x_date(date_labels = "%d-%b",date_breaks = "2 weeks")+
  geom_rug(data=subset(data2, !is.na(strain)),aes(x = days),color='dodgerblue3',alpha=0.2,outside = TRUE,length = unit(0.5, "npc")) +
  coord_cartesian(clip = "off")+
  theme_classic()+
  scale_y_continuous(limits = c(0, 1))+
  theme(plot.margin = margin(1, 1, 5, 1, "cm"))+
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  theme(axis.text.x = element_text(vjust=10))+
  #theme(axis.ticks.x = element_line(vjust=-30))+
  theme(axis.ticks.length=unit(-0.25, "cm"))
#theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.x = element_line(margin=unit(c(1,0.5,0.5,0.5), "cm"))) 
pRug




myColor3 <- randomcoloR::distinctColorPalette(k = 60)

myColor4 <- randomcoloR::distinctColorPalette(k = 60)

custom1 <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
             "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352", "gray80","slategrey",
             "burlywood4","thistle4","lightsalmon2","coral2","lightpink3","indianred3","orange3","yellow3","yellowgreen",
             "khaki4","springgreen4","aquamarine3","lightblue2","lightblue4", "slateblue", "purple4", "slateblue4",
             "maroon4","indian","black","black","black","black","black","black","black","black","black","black","black","black")



world_mutation_data<-read_excel('world_sequences_12Jan_spike_translation.xlsx')
world_metadata<-read_excel('dataset_v3/world_sequences_12Jan_metadata.xlsx')
world_metadata<-subset(world_metadata,host='Human')

world_metadata<-world_metadata%>% select(strain, date,country)

world_merged_data<-merge(world_mutation_data,world_metadata)

world_merged_data$days<-as.Date(cut(world_merged_data$date,
                        breaks = "day",
                        start.on.monday = FALSE))

world_merged_data$date<-as.Date(cut(world_merged_data$date,
                        breaks = "week",
                        start.on.monday = FALSE))

#myColor_dark<-randomcoloR::randomColor(count=47, hue='random',luminosity = "random")

library(randomcoloR)
nColor <- 63
#myColor_dark <- randomcoloR::distinctColorPalette(k = 63)

pie(rep(1, 63), col = myColor_dark)

countries=unique(c(as.vector(subset(world_merged_data, L18F=='F')$country),
  as.vector(subset(world_merged_data, D80A=='A')$country),
  as.vector(subset(world_merged_data, D215G=='G')$country),
  as.vector(subset(world_merged_data, R246I=='I')$country),
  as.vector(subset(world_merged_data, K417N=='N')$country),
  as.vector(subset(world_merged_data, E484K=='K')$country),
  as.vector(subset(world_merged_data, N501Y=='Y')$country),
  as.vector(subset(world_merged_data, A701V=='V')$country)))


#palette1 = scales::hue_pal()(length(countries))
#print(palette1)
names(myColor_dark) = countries
print(myColor_dark)



P_L18F<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, L18F=='F'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('red','gold2'))+
  #scale_fill_viridis(discrete=TRUE,option='magma')+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle("L18F")

P_L18F


P_D80A<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, D80A=='A'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('red','gold2'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('D80A')
#ylab('Proportion of Genomes')

P_D80A



P_D215G<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, D215G=='G'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('gold2', 'red','grey'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('D215G')
#ylab('Proportion of Genomes')

P_D215G

P_LAL242_244XXX<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, LAL242_244XXX=='G'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('gold2', 'red','grey'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('242-244_del')
#ylab('Proportion of Genomes')


P_R246I<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, R246I=='I'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('red', 'gold2','grey'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('R246I')
#ylab('Proportion of Genomes')

P_R246I


P_K417N<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, K417N=='N'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('gold2','red','grey', 'white'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('K417N')
#ylab('Proportion of Genomes')

P_K417N


P_E484K<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, E484K=='K'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('gold2','red','grey', 'white'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('E484K')
#ylab('Proportion of Genomes')

P_E484K


P_N501Y<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, N501Y=='Y'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('gold2','grey','red', 'white'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle('N501Y')
#ylab('Proportion of Genomes')

P_N501Y


P_A701V<-world_merged_data %>%
  mutate(date=as.POSIXct(date)) %>% #convert date to date
  #group_by(D614G_variant, Date_received_by_Sender) %>% #group
  #summarise(prop = sum(D614G_variant=="G")/n()) %>% #calculate proportion 
  ggplot(data=subset(world_merged_data, A701V=='V'), mapping = aes(x = date, fill=country))+
  #ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date,fill=District))+
  #geom_bar(position='fill',width=5,color='black')+
  #geom_bar(width=5)+
  
  geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16,angle = 90))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_viridis(discrete=TRUE,labels = c("B.1.1.54", "B.1.1.56","C.1","ZA.N501Y.new.lineage","Others"), name='Lineages') +
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values = getPalette(24))+
  #scale_fill_manual(values = getPalette(24))+
  scale_fill_manual(values = myColor_dark)+
  #scale_fill_manual(values=c('gold2','red', 'grey','white'))+
  #scale_fill_manual(labels = c("B.1.1.54", "B.1.1.56","C.1","Others"))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = "bottom",legend.direction="vertical")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(axis.title.y = element_blank())+
  #theme(axis.title.x = element_blank())+
  theme(legend.key.size = unit(0.2, "cm"))+
  xlab('Date')+
  ylab(' ')+
  ggtitle("A701V")
#ylab('Proportion of Genomes')

P_A701V


plot_grid(P_L18F,P_D80A, P_D215G,P_R246I,P_K417N,P_E484K,P_N501Y,P_A701V,ncol=2)


ggplot(data=subset(data2, L18F=='F'), aes(date)) + geom_density(adjust=2)+
  theme_classic()

ggplot(data2, aes(x=date, y=Freq)) + geom_line(aes(group=(L18F=='F'), color=(L18F=='F')))

ggplot(data2, aes(x=date))+geom_freqpoly(aes(colour=L18F), size=2, bins=2)




