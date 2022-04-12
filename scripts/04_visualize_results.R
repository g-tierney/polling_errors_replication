library(tidyverse)
rm(list=ls())

theme_set(theme_bw() + 
            theme(legend.position = "bottom",
                  text = element_text(size = 20)))

results <- read_csv("output/frozen_results_bias.csv") %>% 
  mutate(year = as.character(year))

results_tau <- read_csv("output/frozen_results_tau.csv") %>% 
  mutate(year = as.character(year))

load("temp/stan_data_presidential_auxiliary_2020Update.RData")

list_to_df <- function(data){
  df <- tibble(r = data$r,
               election_identifiers = data$election_identifiers[data$r],
               year = (2004 + (0:4)*4)[data$year[data$r]],
               state = data$state[data$r],
               y = data$y,
               n = data$n,
               t = data$t_int,
               v = data$v[data$r]) %>%
    mutate(error = y - v)
  
  df
}

polls_dataframe <- list_to_df(actual_polls_data)

#names for different models
model_crosswalk <- tibble(model = c("const_lpm","jasa_final_model","rw_reparam_lpm"),
                          Method = c("M1: Static","M2: Linear","M3: RW"),
                          IntroDescription = c("","Linear Model (Shirani-Mehr et al., 2018)","Proposed Random Walk Model"))


# plot data availability
polls_dataframe %>% 
  group_by(year) %>% 
  summarise(n=n(),
            n_1week = sum(t<=7),
            n_3week = sum(t<=21)) %>% 
  pivot_longer(-year) %>% 
  mutate(year = as.factor(year),
         name = case_when(name == "n" ~ "100 Days",
                   name == "n_1week" ~ "7 Days",
                   name == "n_3week" ~ "21 Days")) %>% 
  ggplot(aes(x=year,y=value,fill=name)) + 
  geom_col(position = position_dodge2()) + 
  labs(x= "Election Year",
       y= "Number of Polls",
       fill = "Days before Election") 
ggsave("output/figures/02_npolls_by_year.png",width = 9,height = 7)

library(geofacet)
polls_dataframe %>% 
  group_by(year,state) %>% 
  summarise(n=n(),
            n_1week = sum(t<=7),
            n_3week = sum(t<=21)) %>% 
  pivot_longer(-c(year,state)) %>% 
  mutate(year = as.factor(year %>% str_replace("20","'")),
         name = case_when(name == "n" ~ "100 Days",
                          name == "n_1week" ~ "7 Days",
                          name == "n_3week" ~ "21 Days")) %>% 
  filter(name == "100 Days") %>% 
  ggplot(aes(x=year,y=value)) + 
  geom_col() + 
  facet_geo(~ state) +
  labs(x= "Election Year",
       y= "Number of Polls",
       fill = "Days before\nElection") + 
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size=10))
ggsave("output/figures/03_npolls_by_year_state.png",width = 14,height = 7)


### plot select states ###

plot_state <- function(plot_state,plot_year=2008,results=dcc_results){
  results %>% 
    left_join(model_crosswalk) %>% 
    filter(state==plot_state,year %in% plot_year) %>% 
    filter(model %in% model_crosswalk$model) %>% 
    filter(t %% 10 == 0) %>% 
    ggplot(aes(x=t,y=mean,color = Method,fill=Method)) +
    geom_line() + geom_point() + 
    geom_ribbon(aes(ymin=q025,ymax=q975),alpha=.1) + 
    geom_hline(yintercept = 0) + 
    scale_y_continuous(labels = scales::percent) + 
    xlim(0,100) + 
    theme_bw() + 
    theme(legend.position = "bottom") + 
    labs(x="Using only polls at least T days before the election",
         y="Election Day Error",
         title=str_c(plot_state," ",plot_year," Polling Errors"))
}

plot_state_tau <- function(plot_state,plot_year=2008,results=results_tau){
  results %>% 
    left_join(model_crosswalk) %>% 
    filter(state==plot_state,year %in% plot_year) %>% 
    filter(model %in% model_crosswalk$model) %>% 
    filter(t %% 10 == 0) %>% 
    mutate(across(all_of(c("mean","q025","q975")),function(x) 2*x)) %>% 
    ggplot(aes(x=t,y=mean,color = Method,fill=Method)) +
    geom_line() + geom_point() + 
    geom_ribbon(aes(ymin=q025,ymax=q975),alpha=.1) + 
    geom_hline(yintercept = 0) + 
    scale_y_continuous(labels = scales::percent) + 
    xlim(0,100) + 
    theme_bw() + 
    theme(legend.position = "bottom") + 
    labs(x="Using only polls at least T days before the election",
         y="Excess MoE",
         title=str_c(plot_state," ",plot_year," Excess Polling Margin of Error"))
}

plot_polls <- function(plot_state,plot_year=2008){
  polls_dataframe %>% 
    filter(state == plot_state,year == plot_year) %>% 
    ggplot(aes(y=y,x=t)) + 
    geom_point() + 
    geom_hline(aes(yintercept = v),color="red") + 
    scale_y_continuous(labels = scales::percent) + 
    xlim(0,100) + 
    theme_bw() + 
    theme(legend.position = "bottom") + 
    labs(x="Days until election",
         y="Republican Two-Party Support",
         title=str_c(plot_state," ",plot_year," Polls"))
  
}

s1 <- "PA"; s2 <- "FL"
y1 <- 2008; y2 <- 2016
example_states <- gridExtra::grid.arrange(plot_state(s1,y1,results),
                                          plot_state_tau(s1,y1),
                                          plot_polls(s1,y1),
                                          plot_state(s2,y2,results),
                                          plot_state_tau(s2,y2),
                                          plot_polls(s2,y2),layout_matrix = matrix(1:6,3,2,byrow = F))
example_states
ggsave("output/figures/04_example_elections.png",plot = example_states,width = 10,height = 8)

t_linear <- 20
t_rw <- 50
bias_comparison <- results %>% 
  filter((t == t_rw & model == "rw_reparam_lpm") | (t == t_linear & model == "jasa_final_model")) %>% 
  filter(model %in% c("jasa_final_model","rw_reparam_lpm")) %>% 
  mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
         year = as.character(year)) %>% 
  pivot_wider(id_cols = c(state,year,election_identifiers),names_from = model,values_from = mean) %>% 
  ggplot(aes(x=RW,y=Linear,color=year)) + 
  geom_point() + 
  geom_abline(slope=1,intercept = 0) + 
  labs(x=str_c("M3: RW (T=",t_rw,")"),y=str_c("M2: Linear (T=",t_linear,")"),
       color = "Election Year") +
  coord_fixed(1) + 
  theme_bw() + 
  theme(legend.position = "bottom")
bias_comparison

t_linear <- 20
t_rw <- 50
tau_comparison <- results_tau %>% 
  filter(t == t_rw & model == "rw_reparam_lpm" | t == t_linear & model == "jasa_final_model") %>% 
  filter(model %in% c("jasa_final_model","rw_reparam_lpm")) %>% 
  mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
         year = as.character(year),
         mean=mean*2,
         q025=q025*2,
         q975=q975*2) %>% 
  pivot_wider(id_cols = c(state,year,election_identifiers),names_from = model,values_from = mean) %>% 
  ggplot(aes(x=RW,y=Linear,color=year)) + 
  geom_point() + 
  geom_abline(slope=1,intercept = 0) + 
  labs(x=str_c("M3: RW (T=",t_rw,")"),y=str_c("M2: Linear (T=",t_linear,")"),
       color = "Election Year") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_bw() + 
  theme(legend.position = "bottom")
tau_comparison
#ggsave("output/m2_m3_comparison_tau.png",height=8,width=10)


bind_rows(
  results_tau %>% 
    filter(t == t_rw & model == "rw_reparam_lpm" | t == t_linear & model == "jasa_final_model") %>% 
    filter(model %in% c("jasa_final_model","rw_reparam_lpm")) %>% 
    mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
           year = as.character(year),
           mean=mean*2,
           q025=q025*2,
           q975=q975*2) %>% 
    pivot_wider(id_cols = c(state,year,election_identifiers),names_from = model,values_from = mean) %>% 
    mutate(variable = "Excess Margin of Error"),
  results %>% 
    filter(t == t_rw & model == "rw_reparam_lpm" | t == t_linear & model == "jasa_final_model") %>% 
    filter(model %in% c("jasa_final_model","rw_reparam_lpm")) %>% 
    mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
           year = as.character(year)) %>% 
    pivot_wider(id_cols = c(state,year,election_identifiers),names_from = model,values_from = mean) %>% 
    mutate(variable = "Election Day Error")
) %>% 
  ggplot(aes(x=RW,y=Linear,color=year)) + 
  geom_point() + 
  geom_abline(slope=1,intercept = 0) + 
  labs(x=str_c("M3: RW (T=",t_rw,")"),y=str_c("M2: Linear (T=",t_linear,")"),
       color = "Election Year") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) + 
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.01)) + 
  facet_wrap(~variable,scales = "free")
ggsave("output/figures/05_bias_tau_comp.png",width=10,height=5)    


results %>% 
  filter(t == 10) %>% 
  filter(model %in% c("const_lpm","jasa_final_model","rw_reparam_lpm")) %>% 
  group_by(year,model) %>% 
  summarise(mean = mean(mean),
            q025 = mean(q025),
            q975 = mean(q975)) %>% 
  ggplot(aes(x=year,y=mean,color=model,
             ymin=q025,ymax=q975)) + 
  geom_point(position = position_dodge(width = .5)) + 
  #geom_errorbar(width = .2,position = position_dodge(width = .5)) + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year",y="Average Election Day Error")


t_rw <- 50
t_linear <- 20
results_tau %>% 
  filter(t == t_rw & model == "rw_reparam_lpm" | t == t_linear & model == "jasa_final_model") %>% 
  filter(model %in% c("const_lpm","jasa_final_model","rw_reparam_lpm")) %>% 
  group_by(year,model) %>% 
  summarise(mean = mean(mean)*2,
            q025 = mean(q025)*2,
            q975 = mean(q975)*2) %>% 
  ggplot(aes(x=year,y=mean,color=model,
             ymin=q025,ymax=q975)) + 
  geom_point(position = position_dodge(width = .5)) + 
  #geom_errorbar(width = .2,position = position_dodge(width = .5)) + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year",y="Excess MoE") + 
  scale_y_continuous(labels = scales::percent)



### plot states where things flip
election_bias_summaries <- results %>% 
  mutate(sig_party_favored = case_when(q025>0 ~ 1,
                                       q975<0 ~ -1,
                                       TRUE ~ 0)) %>% 
  group_by(model,state,year,election_identifiers) %>% 
  filter(t %in% seq(5,100,5)) %>% 
  summarise(n=n(),
            min_bias = min(mean),
            max_bias = max(mean),
            range = max_bias-min_bias,
            sd_bias = sd(mean),
            sign_flip = sign(min_bias) != sign(max_bias),
            sign_flip_30 = ifelse(min(t)<=30,sign(min(mean[t<=30])) != sign(max(mean[t<=30])),NA),
            sig_sign_flip = 1 %in% sig_party_favored & -1 %in% sig_party_favored,
            new = length(unique(sig_party_favored))>1,
            new30 = length(unique(sig_party_favored[t<=30]))>1) %>% 
  left_join(polls_dataframe %>% group_by(election_identifiers) %>% summarise(npolls=n()))

election_bias_summaries_my <- election_bias_summaries %>% 
  group_by(model,year) %>% 
  filter(!str_detect(model,"const"),npolls>5) %>% 
  summarise(nflips=sum(sign_flip),
            nflips30 = sum(sign_flip_30),
            avgflips=mean(sign_flip),
            avgflips30=mean(sign_flip_30),
            mean_range = mean(range),
            dif_conclusions = sum(new),
            dif_conclusions30 = sum(new30),
            dif_conclusions_mean = mean(new)) 

library(tidytext)
election_bias_summaries %>% 
  left_join(model_crosswalk) %>% 
  filter(model %in% model_crosswalk$model[2:3],
         #year %in% c(2008,2016),
         npolls>20) %>% 
  group_by(model,year) %>% 
  arrange(min_bias) %>% 
  mutate(year = str_c(year," Election Cycle"),
         #state=factor(state,levels=unique(state))
         state = reorder_within(state,min_bias,year)) %>% 
  ggplot(aes(ymin=min_bias,ymax=max_bias,x=state,color=Method)) + 
  geom_errorbar(width = .3,position = position_dodge2()) + 
  facet_wrap(~ year,scales = "free_x") +
  scale_x_reordered() + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "State",
       y = "Ellection Day Error") + 
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 5))

plots <- lapply(sort(unique(election_bias_summaries$year)),function(y){
  election_bias_summaries %>% 
    left_join(model_crosswalk) %>% 
    filter(model %in% model_crosswalk$model[2:3],
           year %in% c(y),
           npolls>20) %>% 
    group_by(model,year) %>% 
    arrange(min_bias) %>% 
    mutate(year = str_c(year," Election Cycle"),
           #state=factor(state,levels=unique(state))
           state = reorder_within(state,min_bias,year)) %>% 
    ggplot(aes(ymin=min_bias,ymax=max_bias,x=state,color=Method)) + 
    geom_errorbar(width = .3,position = position_dodge2()) + 
    facet_wrap(~ year,scales = "free_x") +
    scale_x_reordered() + 
    geom_hline(yintercept = 0) + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = "State",
         y = "Ellection Day Error") + 
    theme(text = element_text(size = 10),
          axis.text = element_text(size = 5))
})
plots[[1]]  
patch <- plots[[1]]
for(p in 2:length(plots)){
  patch <- patch + plots[[p]]
}
layout <- '
AABBCC
#DDEE#
'
patch + patchwork::plot_layout(design = layout,widths = rep(1,5),guides = 'collect')
ggsave("output/figures/06_error_ranges.png",width = 10,height = 5)

election_bias_summaries %>%
  filter(model %in% model_crosswalk$model[2:3],
         npolls>20) %>% 
  group_by(year,state) %>% 
  summarise(overlap = max(0,min(max_bias) - max(min_bias)),
            overlap_pct_linear = overlap/range[model == "jasa_final_model"],
            overlap_pct_rw = overlap/range[model == "rw_reparam_lpm"]) %>% 
  group_by(year) %>% 
  summarise(across(where(is.numeric),mean))


plot_data <- results %>% 
  filter(model %in% c("jasa_final_model","rw_reparam_lpm"),year == 2008,t %in% c(20,50,90)) %>% 
  group_by(model,state) %>% 
  mutate(party_favored = case_when(mean > 0 ~ "Republican Candidate",
                                   mean < 0 ~ "Democratic Candidate"),
         party_favored = factor(party_favored,levels = rev(unique(party_favored))),
         t = str_c(t," Days before the Election")) %>% 
  left_join(model_crosswalk) %>% 
  right_join(urbnmapr::states %>% rename(state=state_abbv)) %>% 
  filter(!state=="DC")

flip_map_data <- election_bias_summaries %>% 
  group_by(model,state) %>% 
  mutate(sign_flip_all = any(sign_flip)) %>% 
  mutate(sign_flip_str = ifelse(sign_flip,"Changes","Stays Constant"),
         sign_flip_all_str = ifelse(sign_flip_all,"Changes","Stays Constant"),
         favored_candidate = case_when(sign_flip ~ "Inconsistent",
                                       min_bias>0 ~ "Republican",
                                       max_bias<0 ~ "Democratic")) %>% 
  ungroup() %>% 
  complete(state,year,model) %>% 
  right_join(model_crosswalk) %>% 
  right_join(urbnmapr::states %>% rename(state=state_abbv)) %>% 
  mutate(favored_candidate = ifelse(is.na(favored_candidate),"No Polls",favored_candidate)) %>% 
  filter(!state=="DC")

flip_map_data %>% 
  ungroup() %>% 
  filter(year %in% c(2008,2016),!str_detect(model,"const")) %>% 
  ggplot(aes(long,lat,group = group,fill = favored_candidate,alpha=favored_candidate)) + 
  geom_polygon(color = "black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  facet_wrap(~IntroDescription + year,) +
  labs(fill = "States where the candidate whose support is overestimated by polls is ...",
       alpha = "States where the candidate whose support is overestimated by polls is ...",
       y="",x="") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank(),
        text = element_text(size = 11)) + 
  scale_fill_manual(breaks = c("Inconsistent","Republican","Democratic","No Polls"),
                    values = c("Purple","Red","Blue","Grey")) + 
  scale_alpha_manual(breaks = c("Inconsistent","Republican","Democratic","No Polls"),
                     values = c(1,0.7,0.7,0.7)) 
  
ggsave("output/figures/01_poll_flip_select_elections_map.png",width = 9,height = 8) 


