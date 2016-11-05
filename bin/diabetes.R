all.data <- read.csv('data/TheDanishLongitudinalStudyofAgeing_F10.csv')

#years <- c(1997, 2002, 2007)
#diabetes.vars <- c('A212', 'B212', 'C212')
#diabetes.difficulties.vars <- c('A213', 'B213', 'C213')

years <- c(1997, 2002, 2007)

# Has a doctor told you, that you have or within the past year have had:b. Diabetes?
diabetes.vars <- c('AV322', 'BV282', 'CV345')
# Has a doctor told you, that you have - or within the past year have had: b. Diabetes. Yes: Do you have daily genes?
diabetes.difficulties.vars <- c('AV323', 'BV283', 'CV346')
diab.diff.vars <- unlist(lapply(1:3, function(i) c(diabetes.vars[i], diabetes.difficulties.vars[i])))
diab.data <- all.data[,diabetes.vars]
diab.data[] <- lapply(diab.data, factor)
summary(diab.data)
diff.data <- all.data[,diabetes.difficulties.vars]
diab.diff.data <- all.data[,diab.diff.vars]
diab.diff.data[] <- lapply(diab.diff.data, factor)
summary(diab.data)
# 1	Yes	537	
# 2 or 5	No	5140	
# 8	Dont know	2	
# 9	No answer	0
# 10	Not applicable	0
diab.status.to.meaning        <- c('Y', 'N', 'N', NA,  NA,  NA)
names(diab.status.to.meaning) <- c('1', '2', '5', '8', '9', NA)
diab.data <- apply(diab.data, c(1,2), function(x) diab.status.to.meaning[as.character(x)])
colnames(diab.data) <- paste0(colnames(diab.data), '_diab')

# Has a doctor told you, that you have or within the past year have had:b. Depression?
depression.vars <- c('AV334', 'BV294', 'CV357')
depression.data <- all.data[,depression.vars]
depression.data[] <- lapply(depression.data, factor)
summary(depression.data)
# Values	Categories	N
# 1	Yes	721	
# 5	No	8801	
# 8	Dont know	42	
# 9	No answer
depression.status.to.meaning        <- c('Y', 'N', 'N', NA,  NA,  NA)
names(depression.status.to.meaning) <- c('1', '2', '5', '8', '9', NA)
depression.data <- apply(depression.data, c(1,2), function(x) depression.status.to.meaning[as.character(x)])
colnames(depression.data) <- paste0(colnames(depression.data), '_depression')
summary(depression.data)
  
# Are there sometimes situations, that you are alone, although you really want to be in the company of others?
alone.vars <- c('AV263', 'BV256', 'CV317')
alone.data <- all.data[,alone.vars]
alone.data[] <- lapply(alone.data, factor)
summary(alone.data)
# Values	Categories	N
# 1	Yes, often	289
# 2	Yes, sometimes	854	
# 3	Rarely	1141	
# 4	Never	5897	
# 8	Dont know
alone.status.to.meaning <-        c('Y', 'Y', 'Y', 'N', NA, NA, NA)
names(alone.status.to.meaning) <- c('1',    '2',   '3',   '4',  '8',  '9',  NA)
alone.data <- apply(alone.data, c(1,2), function(x) alone.status.to.meaning[as.character(x)])
colnames(alone.data) <- paste0(colnames(alone.data), '_alone')

#How many years for schooling do you have?
# 1997
#Values	Categories	N
#1	7 th grade	3049	
#2	8-9 th grade without examination	558	
#3	9 th grade with examination (The public school examination,	514	
#4	10 th grade without examination	45	
#5	10 th grade with examination (The extended public school exa	1126	
#6	Grammar school or the like	66	
#7	Grammar school examination, Higher Preparatory Examination,	476	
#8	Other schooling	26	
#88	Dont know	4	
#
# 2002
#1	7. Grade	1229	
#2	8-9. Grade	433	
#3	9. Grade with final examination	487	
#4	10. Grade without final examination	44	
#5	10. Grade with final examination	826	
#6	Grammar school or the like	67	
#7	General Certificate of Education or the like	448	
#8	Other schooling	24	
#88	Dont know	6	
#99	No answer
#
# 2007
# 1	7 or fewer years of schooling	7010	
# 2	8-9 years of schooling	2623	
# 4	10-11 years of schooling	0	
# 5	12+ years of schooling (general Certificate of Education)	0	
# 8	Dont know	0	
# 10	Not applicable	0	
# 9	No answer
#
schooling.vars <- c('AV42', 'BV62', 'CV62')
schooling.data <- all.data[, schooling.vars]
schooling.data[] <- lapply(schooling.data, factor)
summary(schooling.data)
schooling.to.meaning <-        c('7- years', '8-9 years', '8-9 years', '10-11 years', '10-11 years', '7- years', '7- years',    NA,  NA,  NA, NA,  NA,    NA)
names(schooling.to.meaning) <- c('1',        '2'        , '3'        , '4'           , '5'         , '6'       , '7',          '8', '9', '10', NA, '88', '99')
schooling.data <- apply(schooling.data, c(1,2), function(x) schooling.to.meaning[as.character(x)])
colnames(schooling.data) <- paste0(colnames(schooling.data), '_schooling')
summary(schooling.data)

# gender
gender.vars <- c('AV4', 'BV8', 'CV5')
gender.data <- all.data[, gender.vars]
gender.data[] <- lapply(gender.data, factor)
summary(gender.data)
gender.to.meaning <-        c('Male', 'Female', NA, NA)
names(gender.to.meaning) <- c('1',    '2',     '9', NA)
gender.data <- apply(gender.data, c(1,2), function(x) gender.to.meaning[as.character(x)])
colnames(gender.data) <- paste0(colnames(gender.data), '_gender')
summary(gender.data)

# study numbers
study.vars <- c('AV1', 'BV1', 'CV1')
study.data <- all.data[, study.vars]
study.data[] <- lapply(study.data, factor)
colnames(study.data) <- paste0(colnames(study.data), '_study')
summary(study.data)

# year of birth
birthyear.vars <- c('AV5', 'BV9', 'CV6')
birthyear.data <- all.data[, birthyear.vars]
birthyear.data[] <- lapply(birthyear.data, factor)
summary(birthyear.data)
birthyear.cv6.to.meaning <-        c('1955','1950','1945','1940','1935','1930','1925','1920',NA, NA, NA)
names(birthyear.cv6.to.meaning) <-     c('1','2','3','4','5','6','7','8','88','99', NA)
birthyear.bv9.to.meaning        <- c('1955','1950','1945','1940','1935','1930','1925','1920',NA,NA)
names(birthyear.bv9.to.meaning) <- c('1','2','3','4','5','6','7','8','9',NA)
birthyear.av5.to.meaning        <- c('1945','1940','1935','1930','1925','1920',NA,NA)
names(birthyear.av5.to.meaning) <- c('1','2','3','4','5','6','9',NA)
birthyear.data <- t(apply(birthyear.data, 1, function(r) c(birthyear.av5.to.meaning[r[1]],
                                                           birthyear.bv9.to.meaning[r[2]],
                                                           birthyear.cv6.to.meaning[r[3]])))
birthyear.data <- data.frame(birthyear.data)
colnames(birthyear.data) <- paste0(colnames(birthyear.data), '_birthyear')
summary(birthyear.data)

all.df <- cbind.data.frame(study.data, birthyear.data, gender.data, alone.data, diab.data, depression.data, schooling.data)
summary(all.df)
gz1 <- gzfile('results/diabetes_cleaned.csv.gz', 'w')
write.csv(all.df, gz1, row.names = F)
close(gz1)

#library(ggplot2)
#p <- ggplot(all.df) + aes('AV322_diab')
#p