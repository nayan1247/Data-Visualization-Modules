##### --------------------------------------------------------------------------------------------------------------------
### Load packages-------------------------

library(data.table)
library(ggplot2)

#####-----------------------------------------------------------
#####----------------------------   Univariate
#####-----------------------------------------------------------

####------------------ Histogram
plot_histogram<-function(data,x){
  p <- ggplot(data,aes_string(x= x))+
    geom_histogram(binwidth = 1, color = md_blue, fill=lt_blue)+
    noodleTheme(base_size = 28)+
    labs(x=x,y = "Frequency",title=paste0('Distribution of ',x))
  return(p)
}

####------------------ Boxplot
plot_boxplot<-function(data,x){
  data<-as.data.table(data)
  feat_ind <- data[,x,with = FALSE]
  p <- ggplot(stack(feat_ind),aes(x= ind, y = values))+
    geom_boxplot(color = dk_green,fill=lt_grey)+
    noodleTheme(base_size = 28)+
    labs(x="",y=x,title=paste0('Distribution of ',x))
  return(p)
}


####------------------ Frequency Plot

plot_frequency<-function(data,x){
  # Sort by the feature
  data<-as.data.table(data)

  #Plot
  p <- ggplot(data,aes_string(x= x))+
    geom_bar(size = 1, color = md_blue,fill=lt_blue)+
    noodleTheme(base_size = 28)+
    labs(x=x,y='Frequency',title=paste0('Distribution of ',x))+
    theme(axis.text.x = element_text(angle=90, vjust = 0.5))
  return(p)
}


#####-----------------------------------------------------------
#####----------------------------   Bivariate
#####-----------------------------------------------------------


####------------------ Scatterplot

plot_scatter<-function(data,x,y, bothAxisSameLength=FALSE){
  data<-as.data.table(data)

  #Plot
  p <- ggplot(data,aes_string(x= x, y= y))+
    geom_point(size = 1, color = dk_blue)+
    noodleTheme(base_size = 28)+
    labs(x=x,y=y,title= paste0(y,' distribution with ',x))
  #Check if flag to have both axis same length is true
  if (bothAxisSameLength){
    minim <- min(c(min(data[,get(x)]),min(data[,get(y)])))
    maxim <- max(c(max(data[,get(x)]),max(data[,get(y)])))
    p<-p+xlim(limits = c(minim, maxim))+
      ylim(limits = c(minim, maxim))
  }
  return(p)
}

####------------------ Grouped Box Plot

plot_categorical_boxplot<-function(data,numVar,catVar){
  p <- ggplot(data,aes_string(x= catVar, y= numVar))+
    geom_boxplot(size = 1, color = dk_green)+
    noodleTheme(base_size = 28)+
    theme(axis.text.x = element_text(angle=90, vjust = 0.5))+
    labs(x=catVar,y=numVar,title= paste0(numVar, ' distribution with ',catVar))
  return(p)
}

####---------------------

stacked_percent_barplot <- function(data,catVarX,catVarY){
  colnames <- c(catVarX,catVarY)
  cd_data <- data[,colnames, with = FALSE]
  cat_count <- as.data.table(count(cd_data, c(catVarX,catVarY)))
  cat_count[,percentage := freq/sum(freq)*100, by = catVarX]
  p <- ggplot(cat_count,aes_string(x= catVarX, y = "freq",fill = catVarY ))+
    geom_bar(position = "fill",stat = "identity", color= md_blue)+
    noodleTheme(base_size = 28)+
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    # scale_fill_noodle1()+
    labs(x=catVarX,y= catVarY,title= paste0(catVarY,' percentage distribution with ', catVarX))
  return(p)
}


#####-----------------------------------------------------------
#####----------------------------   Multivariate
#####-----------------------------------------------------------

####------------------ Heatmap

plot_heatmap<-function(data,catVarX,catVarY,numVar){
  #Plot
  p <- ggplot(data, aes_string(catVarX, catVarY)) +
    geom_tile(aes_string(fill = numVar), color = "white") +
    scale_fill_gradient(low = lt_grey, high = dk_red) +
    labs(x= paste0("List of ", catVarX),y= paste0("List of ",catVarY),fill = numVar)+
    noodleTheme(base_size = 28)+
    theme(axis.text.x = element_text(angle=90, vjust = 0.5))
  return(p)
}

plot_grouped_barplot<- function(data,catVar,groupVar){
  # Sort by the feature
  data <- as.data.table(data)
  data<- data[order(get(catVar)),]

  #Plot
  p <- ggplot(data,aes_string(x= catVar))+
    geom_bar(size = 1, color = md_blue,aes_string(fill=groupVar),position = "dodge")+
    noodleTheme(base_size = 28)+
    labs(x=catVar,y='Frequency',title=paste0('Distribution of ',catVar,' grouped by',groupVar))+
    theme(axis.text.x = element_text(angle=90, vjust = 0.5))+
    scale_fill_noodle1()
  return(p)
}

plot_categorical_grouped_boxplot<- function(data,catVar,numVar,groupVar){
  p <- ggplot(data,aes_string(x= catVar, y= numVar))+
    geom_boxplot(size = 1, color = dk_green,aes_string(fill=groupVar),position = "dodge")+
    noodleTheme(base_size = 28)+
    theme(axis.text.x = element_text(angle=90, vjust = 0.5))+
    labs(x=catVar,y=numVar,title= paste0('Distribution of ',catVar,' by ',groupVar))+
    scale_fill_noodle1()
  return(p)
}

plot_grouped_scatter<- function(data,x,y,groupVar){
  p <- plot_scatter(data,x,y, bothAxisSameLength=FALSE)
  p <- p+geom_point(mapping = aes_string(color = groupVar))+
       scale_color_noodle1()+
       labs(title= paste0(y,' distribution with ',x," grouped by ",groupVar))
  return(p)
}


plot_grouped_lineplot<- function(data,dateVar,y,groupVar){
  data<-as.data.table(data)

  #Plot
  p <- ggplot(data,aes_string(x= dateVar, y= y,group = groupVar, colour = groupVar ))+
       geom_line(size = 1)+
       noodleTheme(base_size = 28)+
       scale_color_noodle1()+
       labs(x=dateVar,y= y,title= paste0(y,' distribution with ',dateVar,' by ',groupVar))
  return(p)
}

### Correlation matrix-----

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


correlation_matrix <- function(data,numerical_features,round_to_decimal,lower_triangle = TRUE, upper_triangle = TRUE,output_path = '', saveFile = TRUE){
  numerical_data <- data[,numerical_features,with = FALSE]
  cormat <- round(cor(numerical_data),round_to_decimal)
  melted_cormat <- melt(cormat)

  if(lower_triangle){
    lower_tri <- get_lower_tri(cormat)
    melted_cormat <- melt(lower_tri, na.rm = TRUE)
  }

  if(upper_triangle){
    upper_tri <- get_upper_tri(cormat)
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
  }

  p <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 1,size = 12, hjust = 1))+
    coord_fixed()+
    noodleTheme(base_size = 28)
  #for huge data- geom_raster works faster


  if(saveFile){
    ggsave(p, filename = paste0(output_path,paste0("correlationmatrix.png")),width=12, height=8, dpi=96, type="cairo-png")
  }
  else{
    return(p)
  }
}



