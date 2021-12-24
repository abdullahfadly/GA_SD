plotgab <- function(data){
  colnames(data) <- c("Variabel", "IV", "IG", "MDA", "MDG", "GA")
  iv <- data[,1:2]
  ig <- data[,c(1,3)]
  mda <- data[,c(1,4)]
  mdg <- data[,c(1,5)]
  
  iv$piv <- as.integer(rank(iv$IV, ties.method ='average'))
  ig$pig <- as.integer(rank(ig$IG, ties.method ='average'))
  mda$pmda <- as.integer(rank(mda$MDA, ties.method ='average'))
  mdg$pmdg <- as.integer(rank(mdg$MDG, ties.method ='average'))
  
  
  drank <- as.data.frame(cbind(iv$Variabel,iv$piv,ig$pig,mda$pmda,mdg$pmdg,data$GA))
  colnames(drank) <- c('Variabel','IV','IG','MDA','MDG','GA')
  
  long <- melt(setDT(drank), id.vars = c('Variabel','GA'), variable.name = 'Metode')
  long$value <- as.numeric(long$value)
  long$GA <- as.numeric(as.character(long$GA))
  
  
  ggplot(long, aes(x = reorder(Variabel,GA), fill = Metode, weight = value)) +
    geom_bar(position = "dodge") +
    scale_fill_viridis_d(option = "plasma") +
    theme_minimal() +
    labs(x='Variabel',y='Peringkat Kepentingan', title = 'Diagram Kepentingan Variabel')+
    theme(axis.text.x = element_text(angle = 45, size = 12),
          axis.text.y = element_blank())
}

plotiv <- function(data){
  IV <- data[,2]
  Variable <- data[,1]
  GA <- data[,6]
  ggplot(data, aes(x=reorder(Variable,GA), y=IV)) +
    geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA), y=0, yend=IV), color="grey") +
    geom_point( color="blue", size=6) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x='Variabel',y='IV', title = 'Diagram Information Value')+
    theme(axis.text.x = element_text(angle = 45, size = 12)) 
}

plotig <- function(data){
  IG <- data[,3]
  IG <- as.numeric(IG)
  Variable <- data[,1] 
  GA <- data[,6]
  ggplot(data, aes(x=reorder(Variable,GA), y=IG)) +
    geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA), y=0, yend=IG), color="grey") +
    geom_point( color="purple", size=6) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x='Variabel',y='Information Gain', title = 'Diagram Information Gain')+
    theme(axis.text.x = element_text(angle = 45, size = 12)) 
}

plotmda <- function(data){
  
  MDA <- data[,4]
  Variable <- data[,1] 
  GA <- data[,6]
  ggplot(data, aes(x=reorder(Variable,GA), y=MDA)) +
    geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA), y=0, yend=MDA), color="grey") +
    geom_point( color="orange", size=6) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x='Variabel',y='Mean Decrease Accuracy', title = 'Diagram Mean Decrease Accuracy')+
    theme(axis.text.x = element_text(angle = 45, size = 12)) 
}

plotmdg <- function(data){
  
  MDG <- data[,5]
  Variable <- data[,1] 
  GA <- data[,6]
  ggplot(data, aes(x=reorder(Variable,GA), y=MDG)) +
    geom_segment( aes(x=reorder(Variable,GA), xend=reorder(Variable,GA), y=0, yend=MDG), color="grey") +
    geom_point( color="yellow", size=6) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x='Variabel',y='Mean Decrease Gini', title = 'Diagram Mean Decrease Gini')+
    theme(axis.text.x = element_text(angle = 45, size = 12)) 
  
}

