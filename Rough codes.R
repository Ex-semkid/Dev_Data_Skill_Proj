ggplot(oecd_G7, mapping = aes(x = year, y = inpatient_care_alos,
                              color = country)) +
  geom_line(size = 1) + geom_point(color = "black") + scale_x_continuous(breaks = gap_years) +
  geom_text(aes(label = inpatient_care_alos, vjust = -0.3)) 



ggplot(oecd_G7, mapping = aes(x = year, y = curative_care_alos,
                              color = country)) +
  geom_line(size = 1) + geom_point(color="black") + scale_x_continuous(breaks = gap_years) +
  geom_text(aes(label = curative_care_alos, vjust = -0.3)) 

geom_smooth(method = "gam")
geom_smooth(method = "glm", se = FALSE)

facet_wrap(~year)
theme_classic()
+
  
  
  scale_x_continuous(breaks = gap_years) + geom_line(size = 0.7) + geom_point(color = "brown")

+ scale_y_log10()






#### MOdelling -----  
model = lm(formula = inpatient_care_alos ~ curative_care_alos, data = oecd_countries)

summary(model)
View(model)

oecd_countries$residuals <- model$residuals # adds residual column to oecd_countries table
oecd_countries$predicted <- model$fitted.values # adds fitted values column

View(oecd_countries) # View added columns
predict(model, data.frame(curative_care_alos = c(10,20,40)))

#############