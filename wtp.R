### Investigating the WTP

devtools::source_gist("1fda3215ee548d64d42b1db78f880ec5")


ls()

rm("apollo_ztest","moveold", "quicktexregapollo_old")

#to remove irrelevant functions

#model1$estimate --> to see the parameter names and the estimates. 

#WTP = dividing the attribute parameter by the cost parameter
## include only the attributes for which I want to compute wtp


WTP<- wtp(cost ="b_prei_", attr = "b_FoUnt_",modelname = model3)

allwtp <-wtp(cost="b_prei_", attr = names(model3$estimate), modelname = model3)


# to create a nice looking table 

modeltex <- quicktexregapollo(model3,wtpest = allwtp)


#View(modeltex)

