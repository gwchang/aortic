csv.echo <- read.csv("./echo.csv")
csv.comorb <- read.csv("./comorbidities.csv")

names(csv.echo) <- paste("S1",names(csv.echo),sep=".")
names(csv.comorb) <- paste("S2",names(csv.comorb),sep=".")

data <- cbind(csv.echo, csv.comorb)

pid0 <- data$S1.Patient[which(data$S1.Aortic.Stenosis == 0
  & data$S1.Bicuspid.AV == 0 
  & data$S1.Rheumatic.AV == 0)]

pid1 <- data$S1.Patient[which(data$S1.Aortic.Stenosis == 1 
  & data$S1.Bicuspid.AV == 0 
  & data$S1.Rheumatic.AV == 0
  & data$S2.CPPD.Pseudogout.Chondrocalcinosis == 0  # S2.C
  & data$S2.SLE == 0                                # S2.E
  & data$S2.Psoriatic.Arthritis == 0                # S2.F
  & data$S2.Rheumatic.Fever == 0)]                  # S2.J

write("group 0 patients", stdout())
print(pid0)
write("group 1 patients", stdout())
print(pid1)


