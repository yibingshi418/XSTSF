######### Initial inspection #########
draw_by <- function(dataframe, x, y){
  p <- dataframe %>% 
    ggplot(aes(x=time, y=norm_f0, group=interaction(syllable_no, ind_no), color=citation_no, linetype=citation_no, text = paste('speaker: ', speaker, '\ncitation tone: ', citation_tone, '\ncitation no:', citation_no, '\ntoken: ', token)))+
    geom_line()+
    {if (missing(y)) {facet_wrap(as.formula(paste("~", x)), ncol = 2, labeller = label_both)}
      else {facet_grid(as.formula(paste(y, "~", x)), labeller = label_value)}}+
    theme_bw()+
    theme(panel.spacing.y = unit(0.02, "cm", data = NULL),
          text = element_text(size = 10))+
    ylim(-4, 4)
  
  p
}


######### Cluster analysis #########

# run k-means analysis
k_means_clustering <- function(df){
  # set up parameters for clustering
  start <- which(names(df)==1)
  end <- which(names(df)== 20)
  
  # create object for clustering
  group_cluster_model <- cld(df, idAll=df$ind_no, timeInData=start:end, time=c(start:end))
  
  # run k-means clustering
  kml(group_cluster_model, nbClusters = 2:10) 
  
  return(group_cluster_model)
}

# convert wide to long dataframe
wide_to_long <- function(df, x){
  column_names <- colnames(df)
  numeric_column_names <- column_names[grepl("^\\d+$", column_names)]
  start <- max(as.integer(numeric_column_names))
  end <- min(as.integer(numeric_column_names))
  
  df <- df %>% 
    gather("time","norm_f0",as.character(start):as.character(end)) %>% 
    mutate(syllable_no = case_when(time >= 1 & time <= 10 ~ '1', 
                                   time >= 11 & time <= 20 ~ '2', 
                                   time >= 21 & time <= 30 ~ '3', 
                                   TRUE ~ NA_character_)) 
  
  return(df)
}

# visualise cluster results
p_cluster <- function(df, x, y = NULL){
  p_cluster <- df %>% 
    ggplot(aes(x = time, y = norm_f0, group = interaction(ind_no, syllable_no), color = {{x}}, text = paste('ind_no: ', ind_no))) +
    geom_line(alpha = 0.2) +
    scale_color_ptol() +
    stat_summary(fun = mean, geom = "line", lwd = 2.5, aes(group = interaction({{x}}, syllable_no)), lty = 1) +
    ylim(-3, 3)+
    xlab("Normalised time") +
    ylab("z-scores of log-f0") + 
    labs(color = "cluster") +
    scale_color_manual(values = c("#4477AA", "#CC6677", "#DDCC77", "#117733"))+
    theme_minimal() +
    theme(legend.position = "right",
          text = element_text(family = 'Times New Roman', size = 20),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 20)))
  
  if (!is.null(y)) {
    p_cluster <- p_cluster + facet_wrap(as.formula(paste("~", y)), ncol = 4, labeller = label_both)
  }
  
  return(p_cluster)
}


# compare k-means and human inspection cluster results
heatmap_data <- function(df, x){
  heatmap_df <- df %>% filter(time == 1) %>% 
    group_by(sandhi_tone, {{x}}) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(sandhi_tone) %>% 
    mutate(count = sum(n),
           prop = n/count) %>% 
    ungroup()

  return(heatmap_df)
}

compare_cluster <- function(df, x){
  heatmap_plot<-ggplot(df,aes_string(x=x,y='sandhi_tone',fill='prop'))+
    # scale_fill_continuous(breaks=c(0,0.5,1))+
    geom_tile()+xlab("cluster")+labs(fill="Frequency")+
    geom_text(aes(label = sprintf("%.2f", prop)),size=5, color = 'white')+
    scale_fill_viridis()+
    scale_color_viridis(direction = -1)+
    scale_fill_gradient(low = "white", high = "#09005F", breaks = c(0, 0.5, 1))+
    theme_minimal()+
    theme(text = element_text(family = 'Times New Roman', size = 20),
          axis.title.x = element_text(margin = margin(t = 15)),
          axis.text.x = element_text(color = c("#4477AA", "#CC6677", "#DDCC77", "#117733"), face = "bold"),
          axis.text.y = element_text(color = c("#4477AA", "#CC6677", "#DDCC77", "#117733"), face = "bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab('k-means cluster')+
    ylab('perceptual cluster')
  
  return(heatmap_plot)
}



######### Distribution analysis #########
distri_count <- function(df, x, y, z=NULL){
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  z <- rlang::enquo(z)
  
  # Group by x, y, and optionally z
  if(is.null(z)) {
    df1 <- df %>% 
      filter(time == 1) %>% 
      group_by(!!x, !!y) %>% 
      count() %>% 
      ungroup()
  } else {
    df1 <- df %>% 
      filter(time == 1) %>% 
      group_by(!!x, !!y, !!z) %>% 
      count() %>% 
      ungroup()
  }
  
  p <- ggplot(df1, aes_string(x = quo_name(x), y = "n", fill = quo_name(y), label = "n")) + 
    geom_bar(position="stack", stat="identity")+
    geom_text(size = 4, position = position_stack(vjust = 0.5))+
    scale_fill_manual(values = c("#4477AA", "#CC6677", "#DDCC77", "#117733"))+
    theme_minimal()+
    ylab('count')+
    labs(fill = 'cluster')+
    theme(text=element_text(size=20, family = 'Times New Roman'))
  
  if (!is.null(z)) {
    p <- p + facet_wrap(as.formula(paste("~", quo_name(z))))
  }
  
  return(p)
}

distri_prop <- function(df, x, y){
  df1 <- df %>% filter(time == 1) %>% 
    count({{x}}, {{y}}) %>% 
    group_by({{x}}) %>% 
    mutate(count = sum(n),
           prop = n/count) %>% 
    ungroup()
  
  p <- ggplot(df1, aes(x={{x}},y=prop, fill={{y}}, label=sprintf("%.2f", prop))) + 
    geom_bar(position="stack", stat="identity")+
    geom_text(size = 4, position = position_stack(vjust = 0.5))+
    scale_fill_manual(values = c("#4477AA", "#CC6677", "#DDCC77", "#117733"))+
    theme_minimal()+
    ylab('count')+
    labs(fill = 'cluster')+
    theme(text=element_text(size=20, family = 'Times New Roman'))
  
  return(p)
}

######### Gradience visualisation #########
std <- function(x) sd(x)/sqrt(length(x))

p_sub_cluster <- function(df, x, y){
  df_mean <- df %>% 
    group_by({{x}}, {{y}}, syllable_no, time) %>% 
    summarise(meanf0 = mean(norm_f0),
              se = std(norm_f0),
              count = n()) %>% 
    mutate(count_text = case_when(time == 20 ~ paste('n=', count, sep = ''),
                                  .default = NA))
  
  p <- ggplot(df_mean, aes(x = time, y = meanf0, color = {{x}}, fill = {{x}}, group = interaction({{x}}, syllable_no)))+
    geom_line(size = 1)+
    geom_ribbon(aes(ymin = meanf0 - se, ymax = meanf0 + se), alpha = .2, linetype = 0)+
    geom_text_repel(data = subset(df_mean, is.na(count_text) == FALSE), 
                    aes(label = count_text), size = 5, family = 'Times New Roman')+
    facet_wrap(vars({{y}}))+
    theme_classic()+
    ylim(-3, 3)
  
  return(p)
}


