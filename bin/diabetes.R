all.data <- read.csv('data/TheDanishLongitudinalStudyofAgeing_F10.csv')

#years <- c(1997, 2002, 2007)
#diabetes.vars <- c('A212', 'B212', 'C212')
#diabetes.difficulties.vars <- c('A213', 'B213', 'C213')

years <- c(1997, 2002, 2007)

# Has a doctor told you, that you have - or within the past year have had: b. Diabetes. Yes: Do you have daily genes?
diabetes.difficulties.vars <- c('AV323', 'BV283', 'CV346')
diab.diff.vars <- unlist(lapply(1:3, function(i) c(diabetes.vars[i], diabetes.difficulties.vars[i])))
diff.data <- all.data[,diabetes.difficulties.vars]
diab.diff.data <- all.data[,diab.diff.vars]
diab.diff.data[] <- lapply(diab.diff.data, factor)
summary(diab.diff.data)

# Has a doctor told you, that you have or within the past year have had:b. Diabetes?
diabetes.vars <- c('AV322', 'BV282', 'CV345')
diab.data <- all.data[,diabetes.vars]
diab.data[] <- lapply(diab.data, factor)
# 1	Yes	537	
# 2 or 5	No	5140	
# 8	Dont know	2	
# 9	No answer	0
# 10	Not applicable	0
diab.status.to.meaning        <- c('Y', 'N', 'N', NA,  NA,  NA)
names(diab.status.to.meaning) <- c('1', '2', '5', '8', '9', NA)
diab.data <- apply(diab.data, c(1,2), function(x) diab.status.to.meaning[as.character(x)])
colnames(diab.data) <- paste0(colnames(diab.data), '_diab')
summary(diab.data)

# Are there sometimes situations, that you are alone, although you really want to be in the company of others?
alone.vars <- c('AV263', 'BV256', 'CV317')
alone.data <- all.data[,alone.vars]
alone.data[] <- lapply(alone.data, factor)
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

# Health estimate
healthstat.vars <- c('AV317', 'BV277', 'CV340')
healthstat.data <- all.data[, healthstat.vars]
healthstat.data[] <- lapply(healthstat.data, factor)
summary(healthstat.data)
healthstat.to.meaning <-        c('Really good', 'Good', 'Moderately', 'Bad', 'Very Bad',NA,NA)
names(healthstat.to.meaning) <- c('1', '2', '3', '4', '5', 8, 9)
healthstat.data <- apply(healthstat.data, c(1,2), function(x) healthstat.to.meaning[as.character(x)])
colnames(healthstat.data) <- paste0(colnames(healthstat.data), '_healt')
summary(healthstat.data)

#------------------------------------------------------
all.df <- cbind.data.frame(study.data, birthyear.data, gender.data, alone.data, diab.data)
summary(all.df)
write.csv(all.df, 'results/alone_diabetes.csv', row.names = F)
