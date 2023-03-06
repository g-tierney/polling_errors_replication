library(tidyverse)
rm(list=ls())

theme_set(theme_bw() + 
            theme(legend.position = "bottom",
                  text = element_text(size = 20)))

results <- read_csv("output/frozen_results_bias.csv") %>% 
  mutate(year = as.character(year))

results_tau <- read_csv("output/frozen_results_tau.csv") %>% 
  mutate(year = as.character(year))

list_to_df <- function(data){
  df <- tibble(r = data$r,
               election_identifiers = data$election_identifiers[data$r],
               year = str_sub(data$election_identifiers,4,7)[data$r],
               state = data$state[data$r],
               y = data$y,
               n = data$n,
               t = data$t_int,
               v = data$v[data$r]) %>%
    mutate(error = y - v)
  
  df
}

load("temp/stan_data_senatorial_cnn_Polls.RData")
polls_dataframe_senate <- list_to_df(actual_polls_data)

load("temp/stan_data_presidential_auxiliary_2020Update.RData")
polls_dataframe_pres <- list_to_df(actual_polls_data)

polls_dataframe <- bind_rows(polls_dataframe_senate,
                             polls_dataframe_pres) %>% 
  mutate(elec=case_when(str_detect(election_identifiers,"Senatorial") ~ "senatorial",
                        str_detect(election_identifiers,"Presidential") ~ "presidential"))

#names for different models
model_crosswalk <- tibble(model = c("const_lpm","jasa_final_model","rw_reparam_lpm"),
                          Method = c("M1: Static","M2: Linear","M3: RW"),
                          IntroDescription = c("","Linear Model (Shirani-Mehr et al., 2018)","Proposed Random Walk Model"))

################
### Figure 1 ###

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


################
### Figure 2 ###

# plot data availability
polls_dataframe %>% 
  group_by(year,elec) %>% 
  summarise(n=n(),
            n_1week = sum(t<=7),
            n_3week = sum(t<=21)) %>% 
  pivot_longer(-c(year,elec)) %>% 
  mutate(year = as.factor(year),
         name = case_when(name == "n" ~ "100 Days",
                          name == "n_1week" ~ "7 Days",
                          name == "n_3week" ~ "21 Days"),
         elec = str_to_title(elec)) %>% 
  ggplot(aes(x=year,y=value,fill=name)) + 
  geom_col(position = position_dodge2()) + 
  facet_wrap(~elec,scales = "free_x") + 
  labs(x= "Election Year",
       y= "Number of Polls",
       fill = "Days before Election") + 
  theme(axis.text.x = element_text(hjust = 1,angle = 45))
ggsave("output/figures/02_npolls_by_year.png",width = 9,height = 7)

################
### Figure 3 ###

# above by state

library(geofacet)
polls_dataframe %>% 
  filter(elec=="presidential") %>% 
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


################
### Figure 4 ###

### plot select states ###

plot_state_alpha <- function(plot_state,plot_year=2008,plot_elec="pres",
                             results_alpha=results){
  results_alpha %>% 
    left_join(model_crosswalk) %>% 
    filter(state==plot_state,year %in% plot_year,
           str_detect(str_to_lower(election_identifiers),str_to_lower(plot_elec))) %>% 
    filter(model %in% model_crosswalk$model) %>% 
    #filter(t %% 10 == 0) %>% 
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
         title=str_c(plot_state," ",plot_year," ",plot_elec," Polling Errors"))
}

plot_state_tau <- function(plot_state,plot_year=2008,plot_elec="pres",
                           results=results_tau){
  results %>% 
    left_join(model_crosswalk) %>% 
    filter(state==plot_state,year %in% plot_year,
           str_detect(str_to_lower(election_identifiers),str_to_lower(plot_elec))) %>% 
    filter(model %in% model_crosswalk$model) %>% 
    #filter(t %% 10 == 0) %>% 
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

plot_polls <- function(plot_state,plot_year=2008,plot_elec="pres"){
  polls_dataframe %>% 
    filter(state == plot_state,year == plot_year,
           str_detect(str_to_lower(election_identifiers),str_to_lower(plot_elec))) %>% 
    ggplot(aes(y=y,x=t)) + 
    geom_point() + 
    geom_hline(aes(yintercept = v),color="red") + 
    scale_y_continuous(labels = scales::percent) + 
    xlim(0,100) + 
    theme_bw() + 
    theme(legend.position = "bottom") + 
    labs(x="Days until election",
         y="Republican Two-Party Support",
         title=str_c(plot_state," ",plot_year," ", plot_elec," Polls"))
  
}

s1 <- "PA"; s2 <- "FL"
y1 <- 2008; y2 <- 2016
e1 <- "Presidential"; e2 <- "Presidential"
text_size <- 17
example_states <- gridExtra::grid.arrange(plot_state_alpha(s1,y1,e1)+theme(text = element_text(size=text_size)),
                                          plot_state_tau(s1,y1)+theme(text = element_text(size=text_size)),
                                          plot_polls(s1,y1,e1)+theme(text = element_text(size=text_size)),
                                          plot_state_alpha(s2,y2,e1)+theme(text = element_text(size=text_size)),
                                          plot_state_tau(s2,y2)+theme(text = element_text(size=text_size)),
                                          plot_polls(s2,y2,e2)+theme(text = element_text(size=text_size)),
                                          layout_matrix = matrix(1:6,3,2,byrow = F))
example_states
ggsave("output/figures/04_example_elections.png",plot = example_states,width = 14,height = 15)

################
### Figure 5 ###

t_linear <- 21
t_rw <- 21

bind_rows(
  results_tau %>% 
    filter(t == t_rw & model == "rw_reparam_lpm" | t == t_linear & model == "jasa_final_model") %>% 
    filter(model %in% c("jasa_final_model","rw_reparam_lpm"),
           elec != "Gubernatorial") %>% 
    mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
           year = as.character(year),
           mean=mean*2,
           q025=q025*2,
           q975=q975*2,
           elec = str_to_title(elec)) %>% 
    pivot_wider(id_cols = c(state,year,election_identifiers,elec),names_from = model,values_from = mean) %>% 
    mutate(variable = "Excess Margin of Error"),
  results %>% 
    filter(t == t_rw & model == "rw_reparam_lpm" | t == t_linear & model == "jasa_final_model") %>% 
    filter(model %in% c("jasa_final_model","rw_reparam_lpm"),
           elec != "gubernatorial") %>% 
    mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
           year = as.character(year),
           elec=str_to_title(elec)) %>% 
    pivot_wider(id_cols = c(state,year,election_identifiers,elec),names_from = model,values_from = mean) %>% 
    mutate(variable = "Election Day Error")
) %>% 
  ggplot(aes(x=RW,y=Linear,color=year)) + 
  geom_point() + 
  geom_abline(slope=1,intercept = 0) + 
  labs(x=str_c("M3: RW (T=",t_rw,")"),y=str_c("M2: Linear (T=",t_linear,")"),
       color = "Election Year") +
  #geom_smooth(method="lm",se=F) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) + 
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.01)) + 
  facet_wrap(~variable+elec,scales = "free")
ggsave("output/figures/05_bias_tau_comp.png",width=10,height=10)    

################
### Figure 6 ###

error_range_comp <- results %>% 
  filter(elec != "gubernatorial") %>% 
  filter(model == "rw_reparam_lpm" |  model == "jasa_final_model") %>% 
  filter(model %in% c("jasa_final_model","rw_reparam_lpm")) %>% 
  filter(7<=t,t<=28) %>% 
  mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
         year = as.character(year),
         elec = str_to_title(elec)) %>% 
  group_by(state,year,election_identifiers,model,elec) %>% summarise(sd = sd(mean),range=max(mean)-min(mean)) %>% 
  pivot_wider(id_cols = c(state,year,election_identifiers,elec),names_from = model,values_from = range) 

error_range_comp %>% 
  ggplot(aes(x=RW,y=Linear,color=year)) + 
  geom_point(alpha=0) + 
  geom_text(aes(label=state),show.legend = F) + 
  geom_abline(slope=1,intercept = 0) + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Range of RW Error Estimates",
       y = "Range of Linear Error Estimates",
  ) + 
  facet_wrap(~elec,scales = "fixed") + 
  guides(color = guide_legend("Year",override.aes = list(alpha=1)))

ggsave("output/figures/06_error_range_comparison.png",width=10,height=7)

###################
### Regressions ###

reg_data <- results %>% 
  filter(model %in% c("jasa_final_model","rw_reparam_lpm")) %>% 
  filter(14<=t,t<=28) %>% 
  mutate(model = if_else(str_detect(model,"rw"),"RW","Linear"),
         year = as.character(year)) %>% 
  pivot_wider(id_cols = c(state,year,election_identifiers,t,elec),names_from = model,values_from = mean) %>% 
  mutate(elec = factor(elec,levels = unique(elec)),
         Linear = 100*Linear,
         RW = 100*RW)

reg_data_pres <- reg_data %>% filter(elec == "presidential")
regs <- list(lm(Linear ~ RW,data=reg_data_pres),
             lm(Linear ~ RW + state,data=reg_data_pres),
             lm(Linear ~ RW + state + year,data=reg_data_pres),
             lm(Linear ~ RW + state + year + factor(t),data=reg_data_pres))

stargazer::stargazer(rev(regs),type = "text",
                     star.cutoffs = c(0.05,0.01,0.001),
                     omit = c("state","year","factor"),df = F,omit.labels = c("State","Year","Window"))
stargazer::stargazer(rev(regs),type = "latex",
                     df = F,star.cutoffs = c(0.05,0.01,0.001),
                     omit = c("state","year","factor"),
                     omit.labels = c("State","Election Year","Cutoff Time"),
                     dep.var.caption = "",
                     dep.var.labels = "Linear Error",
                     covariate.labels = c("Random Walk Error",
                                          "Constant"),out.header = F)

reg_data_sen <- reg_data %>% filter(elec == "senatorial") %>% filter(as.numeric(year) %% 4 != 0)

regs <- list(lm(Linear ~ RW,data=reg_data_sen),
             lm(Linear ~ RW + state,data=reg_data_sen),
             lm(Linear ~ RW + state + year,data=reg_data_sen),
             lm(Linear ~ RW + state + year + factor(t),data=reg_data_sen))

stargazer::stargazer(rev(regs),type = "text",
                     omit = c("state","year","factor"),df = F,
                     omit.labels = c("State","Year","Day"))
stargazer::stargazer(rev(regs),type = "latex",
                     df = F,star.cutoffs = c(0.05,0.01,0.001),
                     omit = c("state","year","factor"),
                     omit.labels = c("State","Election Year","Cutoff Time"),
                     dep.var.caption = "",
                     dep.var.labels = "Linear Error",
                     covariate.labels = c("Random Walk Error",
                                          "Constant"),out.header = F)

regs <- list(lm(abs(Linear) ~ abs(RW) ,data=reg_data_sen),
             lm(abs(Linear) ~ abs(RW) + state,data=reg_data_sen),
             lm(abs(Linear) ~ abs(RW) + state + year,data=reg_data_sen),
             lm(abs(Linear) ~ abs(RW) + state + year + factor(t),data=reg_data_sen))

stargazer::stargazer(rev(regs),type = "text",
                     omit = c("state","year","factor"),df = F,omit.labels = c("State","Year","Day"))
stargazer::stargazer(rev(regs),type = "latex",
                     df = F,star.cutoffs = c(0.05,0.01,0.001),
                     omit = c("state","year","factor"),
                     omit.labels = c("State","Election Year","Cutoff Time"),
                     dep.var.caption = "",
                     dep.var.labels = "Linear Error",
                     covariate.labels = c("Random Walk Error",
                                          "Constant"),out.header = F)