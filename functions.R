######### Data processing #########

# a function to recursively merge monosyllables labels with target data frame
# This is to add monosyllabic citation tones to the target data frame
recursive_left_join <- function(df) {
  merge_con <- sub('^[^_]+_([^_]+)_.*$', '\\1', deparse(substitute(df)))
  join_count <- switch(merge_con, mono = 1, di = 2, tri = 3)
  merged_df <- df
  
  for (i in 1:join_count) {
    by_con <-  paste0('mono_no_', as.character(i))
    
    by <- if (i == 1) {
      join_by({{ by_con }} == token_no)
    } else {
      join_by(speaker, {{ by_con }} == token_no)
    }
    
    merged_df <- left_join(merged_df, label_mono_ct[, -c(1, 2)], by = by) %>% 
      rename_with(~paste0('mono_tone_', i), tone)
  }
  
  merged_df <- merged_df %>% unite(citation_tone, matches('^mono_tone_'), remove=FALSE)
  
  return(merged_df)
}

# A function to merge labels and raw data
data_merge <- function(df) {
  # get the df for merging according to the name of the input df
  merge_con <- sub('^[^_]+_([^_]+)_.*$', '\\1', deparse(substitute(df)))
  df_merge <- get(paste('label', merge_con, 'ct', sep = '_'))
  
  # select relevant columns
  selected_cols <- intersect(c('token', 'syntax', 'token_no', 'speaker', 'citation_tone', 'tone', 'mono_tone_1', 'mono_tone_2', 'mono_tone_3'), colnames(df_merge))
  
  # prepare time values
  time_values <- list(mono = rep(1:10, times = nrow(df) / 10),
                      di = rep(1:20, times = nrow(df) / 20),
                      tri = rep(1:30, times = nrow(df) / 30))
  
  # merge data frames
  merged_df <- left_join(df, select(df_merge, all_of(selected_cols)), by = c('token_no', 'speaker')) %>% 
    mutate(time = time_values[[merge_con]]) %>% 
    dplyr::rename(citation_no = token_no)
  
  return(merged_df)
}


######### Initial inspection #########
draw_by <- function(dataframe, x, y){
  p <- dataframe %>% 
    ggplot(aes(x=normtime, y=norm_f0, group=interaction(syllable_no, ind_no), color=citation_no, linetype=citation_no, text = paste('speaker: ', speaker, '\ncitation tone: ', citation_tone, '\ncitation no:', citation_no, '\ntoken: ', token)))+
    geom_line()+
    ylim(-3, 3)+
    {if (missing(y)) {facet_wrap(as.formula(paste("~", x)), ncol = 2, labeller = label_both)}
      else {facet_grid(as.formula(paste(y, "~", x)), labeller = label_value)}}+
    theme_bw()+
    theme(panel.spacing.y = unit(0.02, "cm", data = NULL),
          text = element_text(size = 10)) 
  p
}


######### Cluster analysis #########

# get desired dataset for k-means clustering
get_df <- function(df, x, y){
  df <- df %>% 
    # select conditions 
    filter(syntax_iniTone %in% x & focus_condition == y & is.na(sandhi_tone) == FALSE) %>% 
    # re-normalise f0 
    group_by(speaker) %>%
    mutate(f0ref = mean(f0, na.rm = T),
           norm_f0 = scale(log(f0))) %>% 
    ungroup() %>% 
    # select relevant variables & convert to wide dataframe
    select(-diortri, -syllable_no, -focus_no, -f0) %>% 
    spread(time, norm_f0)
  
  return(df)
}

# run k-means analysis
k_means <- function(df){
  # set up parameters for clustering
  start <- which(names(df)=="1")
  end <- which(names(df)=="20")
  
  # create object for clustering
  group_cluster_model <- cld(df, idAll=df$ind_no, timeInData=start:end, time=c(start:end))
  
  # run clustering algorithm
  kml(group_cluster_model, nbClusters = 2:10) 
  
  return(group_cluster_model)
}

# get cluster solution
get_cluster_solution <- function(df){
  column_names <- colnames(df)
  numeric_column_names <- column_names[grepl("^\\d+$", column_names)]
  start <- max(as.integer(numeric_column_names))
  end <- min(as.integer(numeric_column_names))
  
  cluster_solution <- df %>% 
    gather("normtime","norm_f0",as.character(start):as.character(end)) %>% 
    mutate(normtime = as.integer(normtime),
           propdur = normtime/20,
           syllable_no = case_when(normtime >= 1 & normtime <= 10 ~ '1', 
                                   normtime >= 11 & normtime <= 20 ~ '2', 
                                   normtime >= 21 & normtime <= 30 ~ '3', 
                                   TRUE ~ NA_character_)) %>% 
    unite('groupvar', ind_no, syllable_no, sep = '_', remove = FALSE)
  
  return(cluster_solution)
}

# visualise cluster results
p_cluster <- function(df_cluster, x){
  p_cluster <- df_cluster %>% 
    ggplot(aes(x = propdur, y = norm_f0, group = groupvar, color = {{x}}, text = paste('ind_no: ', ind_no))) +
    geom_line(alpha = 0.2) +
    scale_color_ptol() +
    stat_summary(fun = mean, geom = "line", lwd = 2.5, aes(group = interaction({{x}}, syllable_no)), lty = 1) +
    xlab("proportion compound duration") +
    ylab("z-scores of log-f0") + 
    labs(color = "cluster") +
    scale_color_manual(values = c("#4477AA", "#CC6677", "#DDCC77", "#117733"))+
    theme_minimal() +
    theme(legend.position = "right",
          text = element_text(family = 'Times New Roman', size = 20),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 20)))
  
  return(p_cluster)
}


# compare k-means and human inspection cluster results
compare_cluster <- function(df, x){
  df_1 <- df %>% filter(normtime == 1)
  
  heatmap_table <- xtabs(as.formula(paste("~sandhi_tone+", x)), df_1)
  heatmap_table <- data.frame(heatmap_table) %>% 
    dplyr::group_by(sandhi_tone) %>% 
    mutate(count = sum(Freq),
           proptune = Freq/count)
  
  heatmap_table$sandhi_tone <- factor(heatmap_table$sandhi_tone, levels = rev(levels(heatmap_table$sandhi_tone)))
  
  heatmap_plot<-ggplot(heatmap_table,aes_string(x=x,y='sandhi_tone',fill='proptune'))+
    # scale_fill_continuous(breaks=c(0,0.5,1))+
    geom_tile()+xlab("cluster")+labs(fill="Frequency")+
    # ggtitle("proportion tunes per cluster")+
    geom_text(aes(label = sprintf("%.2f", proptune)),size=5, color = 'white')+
    scale_fill_viridis()+
    scale_color_viridis(direction = -1)+
    scale_fill_gradient(low = "white", high = "#09005F", breaks = c(0, 0.5, 1))+
    theme_minimal()+
    theme(text = element_text(family = 'Times New Roman', size = 20),
          axis.title.x = element_text(margin = margin(t = 15)),
          axis.text.x = element_text(color = c("#4477AA", "#CC6677", "#DDCC77", "#117733"), face = "bold"),
          axis.text.y = element_text(color = c("#4477AA", "#CC6677", "#DDCC77", "#117733"), face = "bold"))+
    xlab('k-means cluster')+
    ylab('perceptual cluster')
  
  return(heatmap_plot)
}



######### Distribution analysis #########
distri_vis <- function(df, x, y){
  df1 <- df %>% filter(normtime == 1) %>% 
    count({{x}}, {{y}})
  p <- ggplot(df1, aes(x={{x}},y=n, fill={{y}}, label=n)) + 
    geom_bar(position="stack", stat="identity")+
    geom_text(size = 4, position = position_stack(vjust = 0.5))+
    scale_fill_manual(values = c("#4477AA", "#CC6677", "#DDCC77", "#117733"))+
    theme_minimal()+
    #xlab('2nd.syllable.tone')+
    ylab('count')+
    labs(fill = 'cluster')+
    theme(text=element_text(size=20, family = 'Times New Roman'))
  
  return(p)
}
