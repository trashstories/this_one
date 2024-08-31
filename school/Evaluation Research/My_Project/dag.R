library(tidyverse)
library(ggdag)
library(dagitty)

dag1 <- dagitty('dag {
"Criminal Record" [pos="-1.162,-0.429"]
"Evidence of Guilt" [latent,pos="0.020,1.298"]
"Jail Conditions" [pos="0.701,-0.429"]
"Length of Pretrial Detention" [pos="-0.124,-0.280"]
"Plea Deals" [outcome,pos="1.330,0.124"]
Bail [exposure,pos="-2.007,0.124"]
Dependents [pos="-1.239,-0.771"]
Judge [pos="-1.746,0.704"]
Prosecutor [pos="0.020,0.704"]
Racism [latent,pos="-1.493,1.298"]
Resources [latent,pos="0.119,-1.022"]
SES [pos="-1.324,0.401"]
"Criminal Record" -> "Length of Pretrial Detention"
"Criminal Record" -> Bail
"Evidence of Guilt" -> Prosecutor
"Jail Conditions" -> "Length of Pretrial Detention"
"Jail Conditions" -> "Plea Deals"
"Length of Pretrial Detention" -> "Plea Deals"
Bail -> "Length of Pretrial Detention"
Bail -> "Plea Deals"
Dependents -> "Length of Pretrial Detention"
Dependents -> Bail
Judge -> Bail
Prosecutor -> "Plea Deals"
Prosecutor -> Judge
Racism -> Judge
Racism -> Prosecutor
Racism -> SES
Resources -> "Jail Conditions"
Resources -> "Length of Pretrial Detention"
SES -> "Criminal Record"
SES -> "Plea Deals"
SES -> Bail
}')

ggdag_status(dag1, text = FALSE, use_labels = "name") +
  guides(color = "none") + 
  theme_dag()









# dag <- dagitty('dag {
#   "Case Management" [pos="-1.047,0.550"]
#   "Criminal Record" [adjusted,pos="-0.630,0.667"]
#   "Recidivism Rate" [outcome,pos="0.797,0.600"]
#   "Services and Resources" [pos="-0.037,0.550"]
#   LEAD [exposure,pos="-2.000,0.600"]
#   Location [pos="-1.817,0.440"]
#   SES [pos="-1.703,0.702"]
#   Time [pos="-2.154,0.440"]
#   "Case Management" -> "Services and Resources"
#   "Criminal Record" -> "Recidivism Rate"
#   "Criminal Record" -> LEAD
#   "Services and Resources" -> "Recidivism Rate"
#   LEAD -> "Case Management"
#   LEAD -> "Recidivism Rate"
#   Location -> LEAD
#   SES -> "Criminal Record"
#   SES -> LEAD
#   Time -> LEAD
#   }')
# 
# # If you want the treatment and outcomes colored differently, 
# # replace ggdag() with ggdag_status()
# ggdag_status(dag, text = FALSE, use_labels = "name", seed = 1234) + 
#   theme_dag() + 
#   theme(legend.position = "bottom")





# dag {
#   bb="-4.777,-7.388,4.585,6.824"
#   "Criminal Record" [pos="-1.593,-2.222"]
#   "Evidence of Guilt" [latent,pos="-0.513,2.884"]
#   "Jail Conditions" [pos="2.480,-5.498"]
#   "Length of Pretrial Detention" [pos="-0.395,-5.498"]
#   "Plea Deals" [outcome,pos="4.113,-4.031"]
#   "State Resources" [latent,pos="1.319,-6.644"]
#   Bail [exposure,pos="-4.387,-4.031"]
#   Dependents [pos="-0.395,-3.006"]
#   Judge [pos="-3.072,0.814"]
#   Prosecutor [pos="-0.513,0.814"]
#   Racism [latent,pos="-2.473,2.884"]
#   SES [pos="-1.947,-0.292"]
#   "Criminal Record" -> Bail
#   "Evidence of Guilt" -> Prosecutor
#   "Jail Conditions" -> "Plea Deals"
#   "Length of Pretrial Detention" -> "Plea Deals"
#   "State Resources" -> "Jail Conditions"
#   "State Resources" -> "Length of Pretrial Detention"
#   Bail -> "Length of Pretrial Detention"
#   Bail -> "Plea Deals"
#   Dependents -> "Plea Deals"
#   Dependents -> Bail
#   Judge -> Bail
#   Prosecutor -> "Plea Deals"
#   Prosecutor -> Judge
#   Racism -> Judge
#   Racism -> Prosecutor
#   Racism -> SES
#   SES -> "Criminal Record"
#   SES -> "Plea Deals"
#   SES -> Bail
# }
