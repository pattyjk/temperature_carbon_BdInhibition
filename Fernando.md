### Frog fungi & bacteria project codes

## libraries
library(ggplot2)
library(ggpubr)
library(readxl)

## loading data
frog <- read_excel("~/Downloads/UMB Frog Bd Project/fernando_collated.xlsx")
frog
names(frog)

### Codes by Pat

# splitting data by strain
frog_sp <- split(frog, frog$Strain)
frog_sp

# growth over temp
ggplot(frog, aes(Temp, OD600, color=Strain))+
  facet_wrap(~Carbon_source, nrow = 4, scales='free')+
  geom_point()+
  geom_line()+
  ylab("OD600")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("Growth vs Temp")

#look at biofilm over temperature
ggplot(frog, aes(Temp, OD550, color=Strain))+
  facet_wrap(~Carbon_source, nrow = 4, scales='free')+
  geom_point()+
  geom_line()+
  ylab("OD550")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("Biofilm Prod vs Temp")

#Cell growth vs biofilm production "OD600 vs OD550"
ggplot(frog, aes(OD600, OD550, color=Genus))+
  geom_point()+
  facet_wrap(~Strain, scales='free_y')+
  stat_cor(method='spearman')+
  xlab("OD600")+
  ylab("OD500")+
  ggtitle("Growth vs biofilm")

#OD600 vs perinhib
ggplot(frog, aes(OD600, Per_inhib, color=Genus))+
  geom_point()+
  facet_wrap(~Strain, scales='free_y')+
  stat_cor(method='spearman')

#perinhib vs OD550
ggplot(frog, aes(Per_inhib, OD550, color=Genus))+
  geom_point()+
  facet_wrap(~Strain, scales='free_y')+
  stat_cor(method='spearman')

# Plot set 1: percent inhibition vs temperature of each species
ggplot(frog_sp$Panama13O, aes(Temp, Per_inhib, color=BdStrain))+
  facet_grid(~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Percent Inhibition of Bd")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("% Inhibition vs Tempture by PAN130")


ggplot(frog_sp$Panama68B, aes(Temp, Per_inhib, color=BdStrain))+
  facet_grid(~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Percent Inhibition of Bd")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("% Inhibition vs Temperature by PAN68B")

ggplot(frog_sp$RSM3.2, aes(Temp, Per_inhib, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Percent Inhibition of Bd")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("% Inhibition vs Temperature by RSM3.2")

ggplot(frog_sp$THA3.2, aes(Temp, Per_inhib, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Percent Inhibition of Bd")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("% Inhibition vs Temperature by THA3.2")

## My Code
# Plot set 2: Cell Growth (OD600) vs temperature of each species
ggplot(frog_sp$THA3.2, aes(Temp, OD600, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Cell Growth (OD600)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD600 vs Temperature by THA3.2")

ggplot(frog_sp$Panama13O, aes(Temp, OD600, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Cell Growth (OD600)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD600 vs Temperature by PAN130")

ggplot(frog_sp$Panama68B, aes(Temp, OD600, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Cell Growth (OD600)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD600 vs Temperature by PAN68B")

ggplot(frog_sp$RSM3.2, aes(Temp, OD600, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Cell Growth (OD600)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD600 vs Temperature by RSM3.2")

# Plot set 3: Biofilm production (OD550) vs temp by each species
ggplot(frog_sp$Panama13O, aes(Temp, OD550, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Biofilm Production (OD500)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD550 vs Temperature by PAN130")

ggplot(frog_sp$Panama68B, aes(Temp, OD550, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Biofilm Production (OD500)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD550 vs Temperature by PAN68B")

ggplot(frog_sp$RSM3.2, aes(Temp, OD550, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Biofilm Production (OD500)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD550 vs Temperature by RSM3.2")

ggplot(frog_sp$THA3.2, aes(Temp, OD550, color=BdStrain))+
  facet_grid(BdStrain~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("Biofilm Production (OD500)")+
  xlab("Temperature")+
  xlim(c(10,30))+
  ggtitle("OD550 vs Temperature by THA3.2")

# percent inhibition vs OD600 per species
ggplot(frog_sp$Panama13O, aes(OD600, Per_inhib, color=BdStrain))+
  facet_grid(~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Cell growth (OD600)")+
  ggtitle("% Inhibition vs Cell Growth by PAN130")

ggplot(frog_sp$Panama68B, aes(OD600, Per_inhib, color=BdStrain))+
  facet_grid(~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Cell growth (OD600)")+
  ggtitle("% Inhibition vs Cell Growth by PAN68B")

ggplot(frog_sp$RSM3.2, aes(OD600, Per_inhib, color=BdStrain))+
  facet_wrap(~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Cell growth (OD600)")+
  ggtitle("% Inhibition vs Cell Growth by RSM3.2"

ggplot(frog_sp$THA3.2, aes(OD600, Per_inhib, color=BdStrain))+
  facet_wrap(~Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Cell growth (OD600)")+
  ggtitle("% Inhibition vs Cell Growth by THA3.2")

# percent inhibition vs OD550 by each species
ggplot(frog_sp$Panama68B, aes(OD550, Per_inhib, color=BdStrain))+
  facet_wrap(~ Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Biofilm (OD550)")+
  ggtitle("% Inhibition vs Biofilm by PAN68B")

ggplot(frog_sp$RSM3.2, aes(OD550, Per_inhib, color=BdStrain))+
  facet_wrap(~ Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Biofilm (OD550)")+
  ggtitle("% Inhibition vs Biofilm by RSM3.2")

ggplot(frog_sp$THA3.2, aes(OD550, Per_inhib, color=BdStrain))+
  facet_wrap(~ Carbon_source)+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Biofilm (OD550)")+
  ggtitle("% Inhibition vs Biofilm by THA3.2")

###

ggplot(frog_sp$Panama13O, aes(OD550, Per_inhib, color=BdStrain))+
  facet_wrap(~Carbon_source, scales="free_x")+
  geom_point()+
  geom_smooth(method='lm')+
  ylab("% Inhibition")+
  xlab("Biofilm (OD550)")+
  ggtitle("% Inhibition vs Biofilm by PAN130")+
  cor_stat(method='spearman')

str(frog_sp$Panama130)

unique(frog$Strain)
