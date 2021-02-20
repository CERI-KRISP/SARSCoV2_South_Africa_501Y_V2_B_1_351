install.packages("ggplot2")
library("ggplot2")
library(RColorBrewer)

ProvincesPanel = c("EC"="#8DD3C7","FS"= "#FFFFB3","NC"="#D9D9D9","WC"="#FDB462","GP"="#Fb8072","KZN"="#80B1D3","NW"="#FCCDE5","MP"="#BEBADA","LP"="#B3De69")
provinces<-data.frame(province=c("EC","FS","WC","GP","NC","KZN","MP","NW","LP"), value=c(1510, 1155, 852, 766, 719, 636, 483, 473, 261))

p1<-ggplot(provinces, aes(x=reorder(province, value), y=value, fill=province)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=value), size=5, hjust=-0.1) 

p1+theme_classic() +
  coord_flip() +
  ggtitle("Excess natural deaths (ages 1yr +) per million population \n by province, up to 8 September 2020") +
  theme(plot.title=element_text(size=13,face="bold", hjust=0.5)) +
  scale_fill_manual(values=ProvincesPanel) +
  xlab("Province") +
  ylab("Excess natural deaths (1 year+) per million population") +
  theme(axis.text.x = element_text(color="black", size=12)) +
  theme(axis.text.y = element_text(color="black", size=12)) +
  theme(axis.title.x = element_text(color = "black", size=12)) +
  theme(axis.title.y = element_text(color = "black", size=12)) +
  scale_y_continuous(breaks=seq(0,2000,500), limits=c(0,2000), expand=c(0,0))

metros<-data.frame(metro=c("NMA","BUF","CPT","MAN","EKU","JHB","TSH","ETH"), province=c("EC","EC","WC","FS","GP","GP","GP","KZN"), value=c(1558,1465,989,881,873,662,555,417))

p2<-ggplot(metros, aes(x=reorder(metro, value), y=value, fill=province)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=value), hjust=-0.1) 

p2+theme_classic() +
  coord_flip() +
  ggtitle("Excess natural deaths (ages 1yr +) per million population \n by metro, up to 8 September 2020") +
  theme(plot.title=element_text(size=13, face="bold", hjust=0.5)) +
  scale_fill_manual(values=ProvincesPanel) +
  xlab("Metro") +
  ylab("Excess natural deaths (1 year+) per million population") +
  theme(axis.text.x = element_text(color="black", size=12)) +
  theme(axis.text.y = element_text(color="black", size=12)) +
  theme(axis.title.x = element_text(color = "black", size=12)) +
  theme(axis.title.y = element_text(color = "black", size=12)) +
  scale_y_continuous(breaks=seq(0,2000,500), limits=c(0,2000), expand=c(0,0)) 
