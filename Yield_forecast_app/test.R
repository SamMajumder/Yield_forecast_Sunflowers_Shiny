

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




#### 

States <- c("ND","SD","MN")

Future_predictions <- readRDS("Future_predictions_combined.RDS")

Future_predictions_ND_SD <- Future_predictions %>% 
                                           filter(State %in% States)
Model <- c("Global","Minnesota")

Variable_Imp_ND_SD <- Variable_Imp %>% 
                            filter(Model_Type %in% Model) 


ALE <- readRDS("ALE.RDS")


Model <- c("National","Minnesota")

ALE_ND_SD <- ALE %>% filter(`Model Type` %in% Model)



saveRDS(Future_predictions_ND_SD,"Future_predictions_combined_ND_SD.RDS")

saveRDS(Variable_Imp_ND_SD,"Variable_Imp_ND_SD.RDS")

saveRDS(ALE_ND_SD,"ALE_ND_SD.RDS")

rm(list = ls())

