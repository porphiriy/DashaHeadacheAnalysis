require(openxlsx)        
require(data.table)        
require(corrplot)        
require(dplyr)        
require(broom)        
require(sjPlot)        
require(ggplot2)        

dt <- read.xlsx("HeadacheStudy.xlsx", sheet = 2, na.strings = 0)

dt <- as.data.table(dt)

dt <- 
dt %>%
  select(!c(date, `Krov./.stomatoloh.i.t.p.`, Antymihren, Zolm..sprei, `Mahne.V6./.vit.D`, Bifren, Prypys.vid.nevroloha, Inshi.liky)) 

corDT <- dt %>% 
  cor(method = "spearman")

colnames(corDT) %>% as.data.table() %>% View

corrplot(corDT, tl.cex = .6, method = "color", addCoef.col = "black", type = "lower", 
         number.cex = 0.5, diag = F)
plot.new()
dev.off()


# GLM
headAcheCauseModel <- glm(Holovnyi.bil ~., family = "binomial", 
                          data = dt %>% select(!c(Peredchuttia, Perepad.temperatury)))
summary(headAcheCauseModel)

# broom
tidy(headAcheCauseModel)
glance(headAcheCauseModel)

# sjPlot
tab_model(headAcheCauseModel)

# ggplot
# Melt the correlation matrix
upper_triangle <- melted_cor_matrix[upper.tri(corDT), ]

# Create the correlation plot
ggplot(data = upper_triangle, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, 
                                   margin=margin(t=-30, r=0, b=20, l=0)), # Further decrease the top margin
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  geom_text(aes(label=sprintf("%.2f", value)), color="black", size=3, check_overlap = TRUE) +
  coord_fixed() +
  theme(legend.key.width = unit(1.5, "cm"))
