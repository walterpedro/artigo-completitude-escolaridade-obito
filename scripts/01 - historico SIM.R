
##### Percentuais de dados faltantes na variável escolaridade do óbito - 1979 a 2018
##### Seção 3.2 Resultados (Figura 3)

library(tidyverse)
library(lubridate)
library(svglite)
theme_set(theme_bw())

df <- readxl::read_excel("arquivos_auxiliares/dados_faltantes_SIM_1979a2018.xlsX")
df$Year <- as_date(df$Year)
glimpse(df)

df <- df %>%
  gather(regiao,percent_miss, -c(CID, Year)) %>%
  arrange(regiao,Year)
 
# plot

brks <- df$Year[seq(1, length(df$Year), 1)]
lbls <- lubridate::year(brks)

ggplot(df, aes(x=Year, y=percent_miss, group=regiao)) + 
  geom_line(aes(shape=regiao)) +
  geom_point(aes(shape=regiao)) +
  scale_x_date(labels = lbls, breaks = brks)+
  geom_vline(xintercept = df$Year[18],
             linetype=4, colour="black") +
  ylim(0,100)+
  labs(y="% of missing data") +
  theme(legend.position = "bottom", legend.title=element_blank(), axis.title=element_text(size=8), 
        axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank()) +   # turn off minor grid
  annotate("text", x = df$Year[9], y = 90, label = "CID9", angle = 0, size = 3, family = "Garamond") +
  annotate("text", x = df$Year[28], y = 90, label = "CID10", angle = 0, size = 3, family = "Garamond") +
  ggsave("arquivos-resultados/figura3-submissao.jpeg", width=6, height=4, dpi=300)
