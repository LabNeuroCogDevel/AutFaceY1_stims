#
# needs dfw to be in env
# dfw -- wide format 3tests/row
#


# Histogram of the totally correct responses by group and exp
# see also
# perl -slane 'print $#F+1 if ! /\*/' Y1 <- 1D/correct/2*/CMFT/Mem.1D |Rio -ne 'sum(df$V1)'
dfw %>% 
 group_by(subj,date,exp,acc) %>%
  mutate(group=ifelse(as.numeric(subj)>=200,'200','100')) %>%
  filter(acc==7) %>%  # 7 means all correct (1 is only L, 2 is only C, 3 is L+C, 5 L+R)
  ggplot(aes(x=exp,fill=group)) + 
  geom_histogram(position='dodge')+
  theme_bw() + ggtitle('Full correct Trial')


#PLOT: how many correct/incorrect trials are there
corrplot <- dfw %>% 
 group_by(subj,date,exp,acc) %>%
  mutate(group=ifelse(as.numeric(subj)>=200,'200','100')) %>%
  mutate(accLab=cut(acc,c(-1,1,6,8),labels=c("wrong","okay","perfect")) ) %>%
  ggplot(aes(x=exp,fill=accLab)) + 
  geom_histogram(position='stack')+ facet_wrap(~group) +
  theme_bw() + ggtitle('Mem+Test Trial correctness')

print(corrplot)
 
#PLOT: accuracy (std)
accplot <- dfw %>% 
 mutate(group=ifelse(as.numeric(subj)>=200,'200','100')) %>%
 group_by(subj,date,exp,group) %>%
 summarise(totacc=sum(acc==7)/n() ) %>%
 ggplot(aes(x=exp,y=totacc, fill=group)) + 
  geom_boxplot() +
  #geom_point(aes(color=group),alpha=.4, position='dodge') +
  theme_bw() + ggtitle('Mem+Test Trial correctness')

print(accplot)
