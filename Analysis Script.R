library(QuantPsyc)
library(psych)
library(plyr)
library(readxl)
library(lawstat)
library(stats)
library(agricolae)
library(car)
library(onewaytests)
library(gplots)
library(readxl)
library(afex)
require(lsmeans)
library(lsmeans)
require(multcomp)
data(obk.long)
library(data.table)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
library(polycor)
library(MASS)
library(Hmisc)
library(xtable)
library(tidyr)
library(tibble)
library(dplyr)
library(MASS)
library(data.table)
library(mediation)
require(Hmisc)
library(rcompanion)


# Load data
data <- read_excel("Survey Results.xlsx")


# Build scales
# AUTHENTIC LEADERSHIP SUBSCALES AND FULL SCALE
RT = data[,c("RT1", "RT2", "RT3")] # relational transparency subscale
x = RT
z = Make.Z(x)
RT_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.80
RT_scale = rowSums(z)/ncol(z)
RT_hist= hist(RT_scale, main="RT", ylab="Frequency", xlab="Z-Score") # strong negative skew (towards right)

IMP = data[,c("IMP1", "IMP2", "IMP3")] #internalized moral perspective subscale
x = IMP
z = Make.Z(x)
IMP_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.65
IMP_scale = rowSums(z)/ncol(z)
IMP_hist = hist(IMP_scale, main="IMP", ylab="Frequency", xlab="Z-Score") # small negative skew (towards right)

BP = data[,c("BP1", "BP2", "BP3")] # balanced processing subscale
x = BP
z = Make.Z(x)
BP_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.82
BP_scale = rowSums(z)/ncol(z)
BP_hist = hist(BP_scale, main="BP", ylab="Frequency", xlab="Z-Score") # strong negative skew (towards right)

SA = data[,c("SA1", "SA2", "SA3")] # self-awareness subscale
x = SA
z = Make.Z(x)
SA_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.79
SA_scale = rowSums(z)/ncol(z)
SA_hist = hist(SA_scale, main="SA", ylab="Frequency", xlab="Z-Score") # small negative skew (towards right)

ALI = cbind(RT,IMP,BP,SA) # full authentic leadership inventory scale
x = ALI
z = Make.Z(x)
ALI_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.91
ALI_scale = rowSums(z)/ncol(z)
ALI_hist = hist(ALI_scale, main="ALI", ylab="Frequency", xlab="Z-Score") # strong negative skew (towards right)
plotNormalHistogram(ALI_scale,
                    main = "Authentic Leadership Perceptions",
                    xlab="Z-score",
                    col="cyan",
                    border="#000000")

# OTHER SCALES
RM = data[,c("RM1", "RM2", "RM3", "RM4")] # role modeling
x = RM
z = Make.Z(x)
RM_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.95
RM_scale = rowSums(z)/ncol(z)
RM_hist = hist(RM_scale, main="RM", ylab="Frequency", xlab="Z-Score") # small negative skew (towards right)
plotNormalHistogram(RM_scale,
                    main = "Acceptance of Leaders as Role Models",
                    xlab="Z-score",
                    col="cyan",
                    border="#000000")


FWL = data[,c("FWL1", "FWL2_rev", "FWL3","FWL4")] # follower's work-life balance
x = FWL
z = Make.Z(x)
FWL_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.91
FWL_scale = rowSums(z)/ncol(z)
FWL_hist = hist(FWL_scale, main="FWL", ylab="Frequency", xlab="Z-Score") # small negative skew (towards right)
plotNormalHistogram(FWL_scale,
                    main = "Followers' Work-Life Balance",
                    xlab="Z-score",
                    col="cyan",
                    border="#000000")


LWL = data[,c("LWL1", "LWL2_rev", "LWL3")] # leader's work-life balance
x = LWL
z = Make.Z(x)
LWL_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.83
LWL_scale = rowSums(z)/ncol(z)
LWL_hist = hist(LWL_scale, main="LWL", ylab="Frequency", xlab="Z-Score") # about normal
plotNormalHistogram(LWL_scale,
                    main = "Leaders' Work-Life Balance\nPerceptions",
                    xlab="Z-score",
                    col="cyan",
                    border="#000000")


JS = data[,c("JS1", "JS2", "JS3","JS4_rev")] # follower's job satisfaction
x = JS
z = Make.Z(x)
JS_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.93
JS_scale = rowSums(z)/ncol(z)
JS_hist = hist(JS_scale, main="JS", ylab="Frequency", xlab="Z-Score") # small negative skew (towards right)
plotNormalHistogram(JS_scale,
                    main = "Job Satisfaction",
                    xlab="Z-score",
                    col="cyan",
                    border="#000000")


JI = data[,c("JI1", "JI2", "JI3")] # job involvement 
x = JI
z = Make.Z(x)
JI_a = psych::alpha(as.matrix(z))$total[1,1] #raw_alpha = 0.66
JI_scale = rowSums(z)/ncol(z)
JI_hist = hist(JI_scale, main="JI", ylab="Frequency", xlab="Z-Score") # about normal

date= data[,'RecordedDate'] # date


# VARIABLES OF INTEREST
variables = cbind(date, ALI_scale,RM_scale,FWL_scale,LWL_scale,JS_scale,JI_scale,data[,c("LMX","FA","FG_num","LG_num","LA_num","WE_num","Org_Size_num","Sector_num")])


# ANOVAS
anova.function <-function(DV, DV_name){
  anova_model = Anova(lm(DV ~ LG_num + FG_num + LG_num:FG_num, data= variables))

  anova_df = data.frame()
  anova_df.columns = c(DV_name, "IV", "p")
  
  if (sum(anova_model$`Pr(>F)`[1:3] < 0.1)>0){
    if (anova_model$`Pr(>F)`[1] < 0.1){ 
      IV = "LG"
      new = data.frame("DV" = DV_name, "IV" = IV, p = anova_model$`Pr(>F)`[1] )
      anova_df = rbind(anova_df, new)
    }
    if (anova_model$`Pr(>F)`[2] < 0.1){ 
      IV = "FG" 
      new = data.frame("DV" = DV_name, "IV" = IV, p = anova_model$`Pr(>F)`[2] )
      anova_df = rbind(anova_df, new)
    }
    if (anova_model$`Pr(>F)`[3] < 0.1){ 
      IV = "LG*FG"
      new = data.frame("DV" = DV_name, "IV" = IV, p = anova_model$`Pr(>F)`[3] )
      anova_df = rbind(anova_df, new)
    }
  }
  return(anova_df)
}

full_anova = rbind(anova.function(RT_scale,"RT_scale"),
      anova.function(IMP_scale, "IMP_scale"),
      anova.function(BP_scale, "BP_scale"),
      anova.function(SA_scale, "SA_scale"),
      anova.function(ALI_scale, "ALI_scale"),
      anova.function(RM_scale, "RM_scale"),
      anova.function(FWL_scale, "FWL_scale"),
      anova.function(LWL_scale, "LWL_scale"),
      anova.function(JS_scale, "JS_scale"),
      anova.function(JI_scale, "JI_scale"))

full_anova
# There is a marginally significant main effect of leader gender on the relational transparency subset of authentic leadership (p = 0.075).
# There is a marginally significant main effect of leader gender on the perception of the leader's work-life balance (p = 0.095).


# CORRELATION MATRICES

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", ifelse(p < .10, "+   ","    ")))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 


# overall correlation matrix
overall = variables[,]
#drops <- c("FG_num","LG_num")
#overall = as.data.frame(overall[ , !(names(overall) %in% drops)])
overall_cor <- corstars(overall)
# Authentic Leadership is significantly correlated with role modeling (r = 0.83), leader work-life balance (0.27), 
# job satisfaction (0.43), job involvement (0.29), and leader-member exchange (0.73).

# Follower work-life balance is significantly correlated with leader work-life balance (0.24) and job involvement (-0.21)

# Leader work-life balance is significantly correlated with job satisfaction (0.26), leader-member exchange (0.27), 
# follower age (0.17), work experience (0.17), and organization size (0.26).

# Job satisfaction is significantly correlated with job involvement (0.64), leader-member exchange (0.50), 
# follower age (0.22), and work experience (0.20)

# Job involvement is significantly correlated with leader-member exchange (0.31)

# Follower age is significantly correlated with leader age (0.40) and work experience (0.85)

# Follower gender is significantly correlated with date of survey (-0.20), leader gender (0.25), and organizational size (-0.18)

# Leader age is significantly correlated with work experience (0.33)




# drop RecordedDate from variables
drops <- c("RecordedDate")
variables = variables[ , !(names(variables) %in% drops)]


# correlation matrices by leader and follower gender split
# leader gender splits
FL= as.data.frame(subset(variables, LG_num == 1))
FL_cor <- corstars(FL)

ML= as.data.frame(subset(variables, LG_num == 0))
ML_cor <- corstars(ML)

# follower gender splits
FF= as.data.frame(subset(variables, FG_num == 1))
FF_cor <- corstars(FF)

MF= as.data.frame(subset(variables, FG_num == 0))
MF_cor <- corstars(MF)

# interaction splits
FL_FF = as.data.frame(subset(FL, FG_num == 1))
FL_FF_cor <- corstars(FL_FF)

FL_MF = as.data.frame(subset(FL, FG_num == 0))
FL_MF_cor <- corstars(FL_MF)

ML_FF = as.data.frame(subset(ML, FG_num == 1))
ML_FF_cor <- corstars(ML_FF)

ML_MF = as.data.frame(subset(ML, FG_num == 0))
ML_MF_cor <- corstars(ML_MF)



# LINEAR REGRESSION
linear_reg <-function(dataset){
  IVs = cbind(dataset$ALI_scale,dataset$ALI_scale,dataset$RM_scale)
  DVs = cbind(dataset$JS_scale, dataset$RM_scale, dataset$JS_scale)


  for(i in 1:ncol(IVs)) {
    for (d in 1:ncol(IVs)) {
      if (i == d) {
        IV = IVs[,i]
        DV= DVs[,d]
      
        # without control variables
        sum = summary(lm(DV~IV, data= dataset))
        Adj.R = sum$adj.r.squared
        p = as.data.frame(coef(sum))["IV","Pr(>|t|)"]
        
        if(i == 1){
          df = data.frame("p"=p, "Adj.R" = Adj.R)
        } else{
          newdf = data.frame("p"=p, "Adj.R" = Adj.R)
          df = rbind(df, newdf)
        }
      }
    }
  } 
  return(df)
}

col= c("AL on JS","AL on RM","RM on JS")

overall_reg = t(linear_reg(overall))
var = rownames(overall_reg)
colnames(overall_reg) <- col
split = rep("overall",2)
overall_reg = cbind(var, split, as.data.frame(overall_reg))

FL_reg = t(linear_reg(FL))
colnames(FL_reg) <- col
split = rep("FL",2)
FL_reg = cbind(var, split, as.data.frame(FL_reg))

ML_reg = t(linear_reg(ML))
colnames(ML_reg) <- col
split = rep("ML",2)
ML_reg = cbind(var, split, as.data.frame(ML_reg))

FF_reg = t(linear_reg(FF))
colnames(FF_reg) <- col
split = rep("FF",2)
FF_reg = cbind(var, split, as.data.frame(FF_reg))

MF_reg = t(linear_reg(MF))
colnames(MF_reg) <- col
split = rep("MF",2)
MF_reg = cbind(var, split, as.data.frame(MF_reg))

FL_FF_reg = t(linear_reg(FL_FF))
colnames(FL_FF_reg) <- col
split = rep("FL_FF",2)
FL_FF_reg = cbind(var, split, as.data.frame(FL_FF_reg))

FL_MF_reg = t(linear_reg(FL_MF))
colnames(FL_MF_reg) <- col
split = rep("FL_MF",2)
FL_MF_reg = cbind(var, split, as.data.frame(FL_MF_reg))

ML_FF_reg = t(linear_reg(ML_FF))
colnames(ML_FF_reg) <- col
split = rep("ML_FF",2)
ML_FF_reg = cbind(var, split, as.data.frame(ML_FF_reg))

ML_MF_reg = t(linear_reg(ML_MF))
colnames(ML_MF_reg) <- col
split = rep("ML_MF",2)
ML_MF_reg = cbind(var, split, as.data.frame(ML_MF_reg))


reg = rbind(overall_reg, FF_reg, FL_reg, MF_reg, ML_reg, FL_FF_reg, FL_MF_reg, ML_FF_reg, ML_MF_reg)
# authentic leadership significantly predicts job satisfaction for all subsets and overall
# authentic leadership significantly predicts role modeling for all subsets and overall
# role modeling significantly predicts job satisfaction for all subsets and overall



  
# MEDIATION ANALYSES  
mediations <-function(dataset){
    
  # simple mediation 
  Xs = cbind(dataset$ALI_scale,dataset$ALI_scale,dataset$ALI_scale,dataset$ALI_scale,dataset$ALI_scale,dataset$LWL_scale)
  X_names = c("ALI_scale","ALI_scale","ALI_scale","ALI_scale","ALI_scale","LWL_scale")
  Ms = cbind(dataset$RM_scale, dataset$FWL_scale,dataset$LWL_scale,dataset$RM_scale, dataset$LWL_scale,dataset$RM_scale)
  M_names = c('RM_scale','FWL_scale','LWL_scale','RM_scale','LWL_scale','RM_scale')
  Ys = cbind(dataset$JS_scale, dataset$JS_scale, dataset$JS_scale, dataset$FWL_scale,dataset$FWL_scale,dataset$FWL_scale)
  Y_names = c('JS_scale','JS_scale','JS_scale','FWL_scale','FWL_scale','FWL_scale')
    
  for(x in 1:ncol(Xs)) {
    for(m in 1:ncol(Ms)) {
      for (y in 1:ncol(Ys)) {
        if ((x == m)&(m==y)) {
          X = Xs[,x]
          X_name = X_names[x]
          M = Ms[,m]
          M_name = M_names[m]
          Y = Ys[,y]
          Y_name = Y_names[y]
            
          model.M <- lm(M ~ X, dataset)
          model.Y <- lm(Y ~ X + M, dataset)
          sum = summary(mediate(model.M, model.Y, sims=500, boot=TRUE, treat='X', mediator='M'))
          PM = sum$n0
          e= sum$d0
          p = sum$d0.p
            
          if(x == 1){
            df = data.frame("X" = X_name, "M" = M_name, "Y"=Y_name, "estimate" = e, "Proportion Mediated" = PM, "p" = p)
          } else{
            newdf = data.frame("X" = X_name, "M" = M_name, "Y"=Y_name, "estimate" = e, "Proportion Mediated" = PM, "p" = p)
            df = rbind(df, newdf)
          }
          }
        }
      } 
    }

  # multiple mediation 
    model <- "
      LWL_scale ~ a1 * ALI_scale
      FWL_scale ~ a2 * ALI_scale + d21 * LWL_scale
      JS_scale ~  cp * ALI_scale + b1  * LWL_scale + b2 * FWL_scale
      ind_eff := a1 * d21 * b2
    "
    fit <- lavaan::sem(model = model, data = dataset, se = "boot", bootstrap = 500)
    sum= lavaan::parameterEstimates(fit, boot.ci.type = "bca.simple")
    e = sum[11,5]
    p= sum[11,8]
    newdf = data.frame("X" = "ALI_scale", "M" = "LWL_scale-->FWL_scale", "Y"="JS_scale", "estimate" = e, "Proportion Mediated" = "NA", "p" = p)
    df = rbind(df, newdf)
    return(df)
}  
  
split = rep("overall",7)
overall_med = cbind(split, as.data.frame(mediations(overall)))

split = rep("FL",7)
FL_med = cbind(split, as.data.frame(mediations(FL)))

split = rep("ML",7)
ML_med = cbind(split, as.data.frame(mediations(ML)))

split = rep("FF",7)
FF_med = cbind(split, as.data.frame(mediations(FF)))

split = rep("MF",7)
MF_med = cbind(split, as.data.frame(mediations(MF)))

split = rep("FL_FF",7)
FL_FF_med = cbind(split, as.data.frame(mediations(FL_FF)))

split = rep("FL_MF",7)
FL_MF_med = cbind(split, as.data.frame(mediations(FL_MF)))

split = rep("ML_FF",7)
ML_FF_med = cbind(split, as.data.frame(mediations(ML_FF)))

split = rep("ML_MF",7)
ML_MF_med = cbind(split, as.data.frame(mediations(ML_MF)))

med = rbind(overall_med, FL_med, ML_med, FF_med, MF_med, FL_FF_med, FL_MF_med, ML_FF_med, ML_MF_med)


  