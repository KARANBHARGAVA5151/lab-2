
#OULAD-STUDENTS DATA
##LOAD THE DATA
oulad_students = read.csv("C:\\Users\\KARAN BHARGAVA\\Downloads\\oulad-students.csv")

##CHECK THE DATA
head(oulad_students)

##SUMMARY OF THE DATA
summary(oulad_students)

##DATA PROCESSING, EXPLOARTION & ANALYSIS
sapply(oulad_students, class) #Gives us the class of each column

##TRAIN THE MODEL
oulad_students$final_result = ifelse(oulad_students$final_result == "PASS", 0, 1)
model = glm(final_result ~ id_student, data = oulad_students, family = "binomial")

##PREDICT 
predicted = predict(model, type = "response")

##CONVERT PREDICTED VALUES (0 or 1)
predicted = ifelse(predicted > 0.5, 1, 0)
length(predicted)

##CALCULATE ACCURACY
accuracy = mean(predicted)

##PLOT USING GGPLOT
plot = data.frame(x = c("accuracy"), y = c(accuracy))
library(ggplot2)
ggplot(plot, aes(x = x, y = y, fill = x)) + 
  geom_bar(stat = "identity")



