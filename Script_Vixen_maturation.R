library(readr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(forcats)
library(readxl)
library(ggpubr)
library(grid)

Reprod <- read_excel('Reproduction0.xlsx', sheet = 'Reproduction0')
View (Reprod)
names(Reprod)
# Bar plot

Reprod$Mois<-factor(Reprod$Mois, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
levels(Reprod$Mois)

AD <- ggplot(Reprod, aes(x = Mois, y = A_dig, fill = Stade))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(title = 'Acropora digitifera')+
  theme_classic()+
  scale_fill_manual(values = c('dimgray', 'coral2', 'grey'))+
  scale_y_continuous(labels = c(0,25,50,75,100))+
  facet_grid(. ~ AnnÈe)+
  theme(plot.title = element_text(size = 12, face = 'italic'),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank(),
        strip.text = element_text(size = 8))
AD

AN <- ggplot(Reprod, aes(x = Mois, y = A_nas, fill = Stade))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(title = 'Acropora nasuta')+
  theme_classic()+
  scale_fill_manual(values = c('dimgray', 'coral2', 'grey'))+
  scale_y_continuous(labels = c(0,25,50,75,100))+
  theme(plot.title = element_text(size = 12, face = 'italic'),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank(),
        strip.text.x = element_blank())+ facet_grid(. ~ AnnÈe)
AN

GF <- ggplot(Reprod, aes(x = Mois, y = G_fas, fill = Stade))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(title = 'Galaxea fascicularis',y = 'Pourcentage (%)')+
  theme_classic()+
  scale_fill_manual(values = c('dimgray', 'coral2', 'grey'))+
  scale_y_continuous(labels = c(0,25,50,75,100))+
  theme(plot.title = element_text(size = 12, face = 'italic'),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank(),
        strip.text.x = element_blank())+ facet_grid(. ~ AnnÈe)
GF

PD <- ggplot(Reprod, aes(x = Mois, y = P_dae, fill = Stade))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(title = 'Platygyra daedalea')+
  theme_classic()+
  scale_fill_manual(values = c('dimgray', 'coral2', 'grey'))+
  scale_y_continuous(labels = c(0,25,50,75,100))+
  theme(plot.title = element_text(size = 12, face = 'italic'),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank(),
        strip.text.x = element_blank())+ facet_grid(. ~ AnnÈe)
PD

LF <- ggplot(Reprod, aes(x = Mois, y = L_phr, fill = Stade))+
  geom_bar(stat = 'identity', position = 'fill')+
  labs(title = 'Leptoria phrygia', caption = 'Author: NY AINA MISANDRATRA E. S., 2022')+
  theme_classic()+
  scale_fill_manual(values = c('dimgray', 'coral2', 'grey'))+
  scale_y_continuous(labels = c(0,25,50,75,100))+
  theme(plot.title = element_text(size = 12, face = 'italic'),
        axis.title.y = element_blank(),
        legend.position = 'none',
        strip.text.x = element_blank())+ facet_grid(. ~ AnnÈe)
LF

?check_legend

fig <- ggarrange(AD,AN,GF,PD,LF, nrow = 5, ncol = 1,
          common.legend = T,
          legend = 'right')
annotate_figure(fig, left = text_grob('Pourcentage (%)',size = 12, face = 'bold',
                                      rot = 90))
fig



### mampiasà 'ggsave()' refa hi'exporter sary @R satria flou io rh atao export directement
  # filename = Anarana zetina ***TSY MAINTSY APETRAKA NY EXTENSION AN'ILAY SARY oh: .png, .jpg, sns***
  # width = en fonction ny largeur tinao
  # heigth = en fonction ny hauteur tinao
  # dpi = ny pixel (d'habitude 1000)
ggsave(filename = 'Plot.maturation_Pierrot.png',dpi = 1000, width = 20,height = 11)








