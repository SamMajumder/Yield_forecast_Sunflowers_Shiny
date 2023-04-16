

ALE <- readRDS("ALE.RDS") %>% 
  rename(Model_Type=`Model Type`)



ALE %>% filter(Model_Type == "National",
               Feature == "Jul_Tmax") %>% 
ggplot(aes(x = Variable_values,
           y = ALE)) + 
  geom_line(aes(color = Feature)) + 
  facet_wrap(Dataset~.) +
  labs(x = "Trait Value",
       y = "Accumulated Local Effects") +
  theme(text = element_text(size=10)) +
  theme(legend.position = "None")
