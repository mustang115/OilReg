#todo : Passing in regsubsets


library(XLConnect)
library(data.table)
library(quantmod)
library(leaps)

######################################### get Data
source("~/_getData.R")
source("~/_rollingreg.R")


width=26   # 2 years of rolling regression
chart.width <- 104
wb.name <- "Regression_Ouptut.xls" # regression output file name



######################################### Variable transformations and Naming, Returns oilreg and oilregxts object
attach(oildata)
oilreg <- cbind("CL1" = CL1,
              "log(PROD)" = log(PROD),
              "diff(INV)" = diff(as.zoo(INV),1,na.pad=TRUE),
              "diff(RIG)" = diff(as.zoo(RIG),1,na.pad=TRUE),
              "CRKSPD" = CRKSPD,
              "POS_F" = log(POS)*diff(as.zoo(POS),1,na.pad=TRUE),
              "HGFE" = (ROC(COPPER,4,na.pad=TRUE)+ROC(IRON,4,na.pad=TRUE))/2,
              "log(SHIP)" = log(SHIP)
                        
)

detach(oildata)
oilregxts <- as.xts(x = oilreg,order.by = index(oilxts))

########################################## regression of the entire data and subset


#entire data
summary(lm(as.data.frame(oilreg)))

#subset of the data
summary(lm(as.data.frame(oilreg)[nrow(oilreg)-width:nrow(oilreg),]))

############################################# Check the R2 fit of models of each parameter fit for entire dataset, by default NA is omitted

regsubsets.out <-regsubsets(CL1~.,
                            data=oilreg,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")

plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")

###############################################

rolling.model <- rollingreg(oilregxts,width)


############################################# prepare to write all the output to a file
output <- merge(oilxts,rolling.model)
residuals <- lag(output$CL1,-1)-output$fit
colnames(residuals) <- "residuals"
output1 <- merge(output,residuals)

rmodel <- names(oilreg)
rmodelout <- paste(rmodel[1],
                   paste(rmodel[2:length(rmodel)],collapse="+"),
                   collapse="="
)


############################################ write output to XLsheet

wb = loadWorkbook(wb.name, create=TRUE)

#output the regsubsets chart
createSheet(wb, name = "model")

createName(wb, name = "chart", formula = "model!$B$2", overwrite=TRUE)
png(filename = "ModelSelection.png", width = 800, height = 600)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
dev.off()
addImage(wb, filename = "ModelSelection.png", name="chart", originalSize = TRUE)

#output the numbers
createSheet(wb, name = "output")
writeWorksheet(wb, data = as.data.table(output1), 
               sheet = "output", startRow = 1, startCol = 1)

#output the reg eq
createSheet(wb, name = "Equation")
writeWorksheet(wb, rmodelout, 
               sheet = "Equation", startRow = 1, startCol = 1)

saveWorkbook(wb)

################################ print Betas in a chart



pdf("regression_output.pdf",width=10,height=7)

for(i in 1:8)
{
  (chart_Series(last(rolling.model[,i],chart.width)))
  print(add_TA(last(output1$CL1, on = NA, chart.width)))
  title(colnames(rolling.model)[i])
}

# Plot Adj R2
chart_Series(last(rolling.model[,"Adj.R2"],chart.width))
print(add_TA(last(output1$CL1, on = NA, chart.width)))
title("Rolling Adj.R2")

print(autoplot.zoo(last(output1[,c("CL1","fit","lwr","upr")],chart.width), facet=NULL))

p1 <- ggplot(last(output1$CL1,chart.width), aes(x = Index, y = CL1))+
  geom_line()+
  geom_line(data=last(output1,chart.width))+
  geom_ribbon(data=last(output1,chart.width),aes(ymin=lwr,ymax=upr,alpha=0.1),fill="grey80")
print(p1) 

dev.off()


