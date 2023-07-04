## Piramide porcentaje del total y con imagen ----

Total_edades_sexo2 %>% 
  ggplot(aes(x=Edad4, y=Total, fill=Sexo))+
  geom_col(data = subset(Total_edades_sexo2,
                         Sexo=="Hombre") %>% 
             mutate(Total = -Total),
           width=0.9,
           col="black")+
  geom_col(data = subset(Total_edades_sexo2,
                         Sexo=="Mujer") %>% 
             mutate(Total = Total),
           width=0.9,
           col="black")+
  geom_text(data = subset(Total_edades_sexo2,
                          Sexo=="Mujer"),
            aes(y=Total, label= Porcentaje),
            position = position_stack(vjust=0.5),
            size=2.4)+
  geom_text(data = subset(Total_edades_sexo2,
                          Sexo=="Hombre"),
            aes(y=-Total, label=Porcentaje),
            position = position_stack(vjust=0.5),
            size=2.4)+
  ggimage::geom_image(aes(x="De 75 a 79 años",
                          y=-5000000,
                          image = "Formas/Hombre.png"))+
  ggimage::geom_image(aes(x="De 75 a 79 años",
                          y=5000000,
                          image = "Formas/Mujer.png"))+
  coord_flip()+
  scale_y_continuous(breaks = c(seq(-60000000,0,by=500000),
                                seq(0, 40000000, by=500000)),
                     labels = comma(c(seq(-60000000,0,by=500000)*-1,
                                      seq(0, 40000000, by=500000))))+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Piramide poblacional en México para el año 2020",
       subtitle = "División por sexo según la edad por cada 4 años",
       caption = "Elaboración propia con datos del CENSO 2020",
       x="",
       y="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="black"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"),
        axis.text.x=element_text(angle=90, hjust=1))
