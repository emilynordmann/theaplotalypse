# theaplotalypse

```{r}
library(extrafont)
library(tidyverse)
library(readxl)
library(patchwork)
library(gridExtra)
library(waffle)
library(magick)
library(png)
library(grid)

tsuro <- read_excel("games.xlsx", sheet = "Tsuro") %>%
  drop_na()

patchwork_game <- read_excel("games.xlsx", sheet = "Patchwork")%>%
  drop_na() 
dual <- read_excel("games.xlsx", sheet = "Dual")%>%
  drop_na()
seven_wonders <- read_excel("games.xlsx", sheet = "7Wonders")%>%
  drop_na()
kittens <- read_excel("games.xlsx", sheet = "Exploding Kittens")%>%
  drop_na()
scrabble <- read_excel("games.xlsx", sheet = "Scrabble")%>%
  drop_na()
splendor <- read_excel("games.xlsx", sheet = "Splendor")%>%
  drop_na()

```

# Set up colours for the graphs

```{r}
patchwork_colours <- c("#67A400", "#F1BF00")
```

# Theme stuff

```{r}

bgcol <- '#5995ab'
textcol <- "grey95"
titlefont <- "Georgia"
subfont <- "Georgia"
subfont2 <- "Georgia"

theme_set(theme_classic(base_family = "Times New Roman"))
theme_update(plot.background = element_rect(color=bgcol,fill=bgcol),
              panel.background = element_blank(),
              axis.line = element_line(color=textcol),
              text = element_text(color=textcol,family=subfont2),
              axis.ticks = element_line(color=textcol),
              axis.text = element_text(color=textcol,family=subfont2),
             axis.title = element_text(family=subfont),
              plot.title = element_text(family=titlefont, size = 20, 
                                        margin = margin(20,0,0,0)),
             plot.subtitle = element_text(family=subfont, margin=margin(0,10,30,0),
                                          size = 15),
             plot.caption = element_text(family=subfont2),
             legend.background = element_rect(color=bgcol,fill=bgcol))
             

```

# Patchwork

```{r}
# calculate percent wins for each

patchwork_wins <- patchwork_game %>%
  group_by(Winner) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n/sum(n),
         percent = prop * 100,
         ymax = cumsum(prop),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) /2,
         label = paste0(Winner, "\n ", round(percent,2), "%"))
```

Waffle plot

```{r}
waf <-patchwork_game %>%
  count(Winner) %>%
  ggplot(aes(fill = Winner, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  guides(fill = NULL, colour = NULL, x = NULL, y = NULL) +
  theme_enhance_waffle()

p1 <- waf +
  geom_waffle(n_rows = 2, size = 0.6, 
              colour = "black", flip = F, radius = unit(4, "pt"), show.legend = F) +
  scale_fill_manual(values = patchwork_colours) +
  geom_label(family = "Georgia", x=c(1.5, 4.5), aes(y=1.5, label=patchwork_wins$label, size=5), 
             colour = c("black"), show.legend = F) +
  labs(title = "Who wins?",
       subtitle = "Kathleen kicks Emily's ass") +
    theme(plot.subtitle = element_text(margin=margin(0,0,30,0)),
          axis.line = element_blank(), 
          axis.ticks = element_blank())
p1


```

```{r}
p2 <- patchwork_game %>%
  summarise(emily_buttons = mean(`E-buttons`),
            kathleen_buttons = mean(`K-buttons`),
            emily_spaces = mean(`E-spaces`),
            kathleen_spaces = mean(`K-spaces`)) %>%
  pivot_longer(cols = emily_buttons:kathleen_spaces, 
               names_to = "player",
               values_to = "avg_number") %>%
  separate(col = player, into = c("player", "component")) %>%
  ggplot(aes(x = component, y = avg_number, fill = player)) +
  geom_col(position = "dodge", colour = "black") +
  labs(x = NULL, y = NULL) + 
  scale_x_discrete(labels = c("Buttons", "Spaces"))+
  scale_fill_manual(values = patchwork_colours, name = "Player", 
                    labels = c("Emily", "Kathleen"))+
  labs(title= "Strategy: buttons or spaces?",
       subtitle = "Kathleen prioritises fewer spaces, Emily more buttons") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.9, 0.8))

p2
```



```{r}
patchwork_game <- patchwork_game %>%
  mutate(special = recode(`E-special`, "7" = "Emily",
                          "0" = "Kathleen"))

waf2 <-patchwork_game %>%
  count(special) %>%
  ggplot(aes(fill = special, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  guides(fill = NULL, colour = NULL) +
  theme_enhance_waffle()

p3 <- waf2 +
  geom_waffle(n_rows = 2, size = 0.6, 
              colour = "black", flip = F, radius = unit(4, "pt"), show.legend = F) +
  scale_fill_manual(values = patchwork_colours) +
    theme(legend.position = "bottom") +
  geom_label(family = "Georgia", x=c(1.5, 4.5), aes(y=1.5, 
                                label=
                                  paste0(special, 
                                         "\n ", round((n/sum(n)*100),2), "%"), 
                                size=4), 
             colour = c("black"), show.legend = F)+
  labs(title = "Who gets the special tile?",
       subtitle = "Kathleen is special") +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1))

p3


```

Difference score

```{r}

mean_diff <- group_by(patchwork_game, Winner) %>%
                                  summarise(mean_diff = abs(mean(Difference))) %>%
  mutate(label = paste0("Average \n ", round(mean_diff,2)))

p4 <- ggplot(patchwork_game, aes(x = reorder(Game, Difference), y = Difference, fill = Winner))+
  geom_col(colour = "black") +
  scale_fill_manual(values = patchwork_colours) +
  labs(x = NULL, y = NULL) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c(50,40,30,20,10,0,10,20,30,40,50)) +
  labs(title = "What's the win margin?",
       subtitle = "Emily goes big or goes home (normally home)") + 
  geom_text(family = "Georgia",data = mean_diff, 
            x=c(4.5,11), aes(y= c(10, -10), label = label, size=5), 
             colour = "white", show.legend = F) +
  coord_flip()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.9, 0.5),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1))
p4
```

Put it all together


```{r fig.width=10, fig.height=8}

# stich together plots using patchwork

fullplot <- 
(p1 + p4) / (p2 + p3) +
  plot_layout(widths = c(1.3, 1))+
  plot_annotation(title = 'Boardgames of the Aplotalypse',
                  subtitle = "Round 1: Patchwork by Uwe Rosenberg",
                  caption = "Vis: @emilynordmann",
                 theme= theme(plot.title = element_text(size=28,hjust=.5,family="Georgia"),
                        plot.subtitle = element_text(size=18,hjust=.5),
                        plot.caption = element_text(size=10,hjust=.5)))

# add logo and save plot

logo <- image_read("patchwork.png")

png("rplot.png", width = 1600, height = 1200, res = 150)

fullplot
grid.raster(logo, x = .9, y = .98, 
                  just = c('right', 'top'), width = unit(1, 'inches'), interpolate = F)
grid.raster(logo, x = .2, y = .98, 
                  just = c('right', 'top'), width = unit(1, 'inches'), interpolate = F)
dev.off() 

image_read("rplot.png")

```

