#-----�ۑ�P-------------------------------
#�J�����g�f�B���N�g�����f�[�^������Ƃ���ɕύX
setwd("C:/Users/81802/OneDrive/���H��/���Ƃ̎���/2021 1Q/�m���E���v�w/���ԃ��|�[�g")

#�J���}��؂�(CSV)�`���̃t�@�C�����f�[�^�t���[���Ƃ��ēǂݍ���
di <- read.table("DI4domain.csv", sep=",", header=T)

#di�̃q�X�g�O�����쐬
hist(di$Koyou, col="green", main="����ʌ���DI:�ٗp�̕��z", xlab="DI", ylab="�x��",breaks=seq(10, 70, 10))

#-----��Q------------------------------
#�J�����g�f�B���N�g�����f�[�^������Ƃ���ɕύX
setwd("C:/Users/81802/OneDrive/���H��/���Ƃ̎���/2021 1Q/�m���E���v�w/���ԃ��|�[�g")

#�J���}��؂�(CSV)�`���̃t�@�C�����f�[�^�t���[���Ƃ��ēǂݍ���
di_area <- read.table("DI4area.csv",sep=",", header=T)

#�����l
max_average <- 0�@#��Q�񂩂��(x-1)��܂ł�DI�̕��ϒl�̍ő�l(�b��̍ő�l)
min_average <- 100#��Q�񂩂��(x-1)��܂ł�DI�̕��ϒl�̍ŏ��l(�b��̍ŏ��l)
max_row <- 0      #�b��̍ő�l�������
min_row <- 0      #�b��̍ŏ��l�������

#��Q�񂩂��P�U��܂ň�񂸂������Ă���
for(x in 2:16){
  
  #��x���DI�̕��ϒl���b��̍ő�l���傫���Ƃ�
  if(mean(di_area[,x])>max_average){
    max_average <- mean(di_area[,x])#�b��̍ő�l����������
    max_row <- names(di_area)[x]    #�b��̍ő�l�������̏�������
  }
  
  #��x���DI�̕��ϒl���b��̍ŏ��l��菬�����Ƃ�
  if(mean(di_area[,x])<min_average){
    min_average <- mean(di_area[,x])#�b��̍ŏ��l����������
    min_row <- names(di_area)[x]#�b��̍ŏ��l�������̏�������
  }
}

#���ԓ���DI�̕��ϒl���ł������n��ƁA�ł��Ⴂ�n���\��
print(max_row)
print(min_row)

#���Ђ��}���쐬
boxplot(di_area[[min_row]], di_area[[max_row]],names=c(min_row,max_row), main="�n��ʌ���DI", xlab="�n��", ylab="DI")

#-----�ۑ�R-------------------------------------------
#�J�����g�f�B���N�g�����f�[�^������Ƃ���ɕύX
setwd("C:/Users/81802/OneDrive/���H��/���Ƃ̎���/2021 1Q/�m���E���v�w/���ԃ��|�[�g")

#�J���}��؂�(CSV)�`���̃t�@�C�����f�[�^�t���[���Ƃ��ēǂݍ���
di_area <- read.table("DI4area.csv",sep=",", header=T)
di <- read.table("DI4domain.csv", sep=",", header=T)

#�����l
min_cor <- 1 #��Ɠ����֘A����DI�����Q�񂩂��(x-1)��܂ł̂��ꂼ��̌���DI�ɑ΂��邻�ꂼ��̑��֌W���̍ŏ��l(�b��̑��֌W���̍ŏ��l)
min_cor_di_area <- "error"#�b��̑��֌W���̍ŏ��l�������̖��O

#��Q�񂩂�Ō�̗�܂łP�s���������s��
for(x in 2:ncol(di_area)){
  
  #��x��Ƃ̑��֌W�����b��̑��֌W���̍ŏ��l��菬�����Ƃ�
  if(cor(di$Kigyo,di_area[,x]) < min_cor){
    min_cor <- cor(di$Kigyo,di_area[,x])#�b��̑��֌W���̍ŏ��l����������
    min_cor_di_area <- names(di_area)[x]#�b��̑��֌W���̍ŏ��l�������̏�������
  }
}

#�ł����ւ��Ⴂ���̂�\��
print(min_cor_di_area)

#�U�z�}��`��
plot(di$Kigyo, di_area[[min_cor_di_area]], xlim=c(10,80), ylim=c(10,80), main="��Ɠ����֘A����DI����n��ʌ���DI�ɑ΂��鑊�ւ��ł��������n��", xlab="��Ɠ����֘A:Kigyo", ylab=min_cor_di_area)


#----------�ۑ�4--------------------------------------------------
target_file <- "��O���Y��.csv"
#----------------------------------------------
#�J�����g�f�B���N�g�����f�[�^������Ƃ���ɕύX
setwd("C:/Users/81802/OneDrive/���H��/���Ƃ̎���/2021 1Q/�m���E���v�w/���ԃ��|�[�g")

#�J���}��؂�(CSV)�`���̃t�@�C�����f�[�^�t���[���Ƃ��ēǂݍ���
di <- read.table("DI4domain.csv", sep=",", header=T)
target_data <- read.table(target_file, sep=",", header=T)  #60*element_count

#�����̃f�[�^�Ƀf�[�^�����錎�̂ݎc��
merged_data <- merge(di, target_data)
di_improved <- merged_data[,c(1:ncol(di))]
target_data <- merged_data[,c(1, c((ncol(di)+1):ncol(merged_data)))]

#target_data�̗�̐�
element_count <- ncol(target_data)
#-----------------------------------------------
#1�`element_count-1�܂�
rank <- 1
#----------------------------------------------
#�v�f�������̐��Ԃ�̗�x�N�g��
correlation_vector <- numeric(element_count-1)

#��Q�񂩂�target_data�̍Ō�̗�܂łP�s���������s��
for(x in 2:element_count){
  
  #target_data�̑悘��̕W���΍���0�łȂ��Ƃ�
  if(var(target_data[,x]) != 0){
    
    #DI_Total��correlation_vector��x��ڂ̑��֌W��
    correlation_vector[x-1] <- cor(di_improved[,2],target_data[,x]) 
  }
}

#���֌W���̒Ⴂ�񂩂珇�ɕ��ׂāA�扽�񂩂�correlation_list�ɋL�q���Ă���
correlation_list <- sort.list(correlation_vector) + 1  #target_data�̑�Q�񂩂�Ō�̗�܂ł̕��בւ��ł��邽�߁A���ׂĂ̗�1���������Ƃɒ���

#���֌W���̍����񂩂珇�ɕ��ׂĂ��̗��correlation_list�ɋL�q���Ă���
correlation_list <- rev(correlation_list)

#���֌W�����~���ɕ��בւ�[������]
correlation_vector <- rev(sort(correlation_vector))

#rank�Ԗڂ̑��֌W���Ƃ��̍��ڂ���
correlation_rank <- correlation_vector[rank]
correlation_rank_target_data <- names(target_data)[correlation_list[rank]]

#���֌W�����ł��������ʂ̑��֌W���Ƃ��̍��ڂ�\��
print(correlation_rank)
print(correlation_rank_target_data)

#�܂���O���t�쐬
merged_data <- merge(di_improved[,c(1,2)], target_data[,c(1,correlation_list[rank])], all=T)  #DI_Total��target_data�̑��֌W����ʂ̗����������
default_mai <-par()�@�@#�f�t�H���g�̒l��ۑ�
mai <- par()$mai       #�O���t�p�����[�^�̐ݒ�
mai[4] <- mai[1]       #�]���T�C�Y�̐ݒ�(�㉺�ƍ��E�̕��𑵂���)
par(mai = mai)         #�w�肵���]���T�C�Y�̓K�p

#target_data�̑��֊֌W��������̎��n��̐܂���O���t�\��
plot(merged_data[,3], type="l", xlab=names(merged_data)[1],ylab=names(merged_data)[3], main="�S��DI�ɍł����ւ̂���f�[�^")

#��̃O���t�𓯎��ɕ\��
par(new=T)    
plot(merged_data[,2], type="l", xlab="", ylab="", col="red", axes=FALSE, ylim= c(10,70))   #DI_Total�̎��n��̐܂���O���t
#2���ڂ̕\��
axis(side=4)    #�ڐ�����E�ɕ\��
# 2���ڂ̃��x���ݒ�
mtext(names(merged_data)[2],side=4,line=2)   #(���x����, ���x���̒u���ʒu, �O���t�̘g����̋���)
par(mai = default_mai$mai)  #�f�t�H���g�̒l�ɖ߂�



#-----�ۑ�T------------------------------
#�ύX��
Variant_name <- "Medium"�@#�l����Variant�����߂�
Covid19_file <- "Covid19.csv" #�����ς�(�����̕\�L�����A�ɍ��킹��)
population_file <- "WPP2019_TotalPopulationBySex.csv" #���������C���ς�
target_file <- "doctor_count.csv"�@#�P��ڂ��AISO3�̍����R�[�h�ŁA2��ڈȍ~���e�N�̒��ׂ����f�[�^
excluded_population <- 100000�@#���̒l�ȉ��̐l���̍�������
rank <- 1�@�@#target_file�̒��ő��֌W�������Ԗڂɋ������\�����邩
worst_rank <- FALSE�@#TRUE:target_file�̒��ő��֌W�����ł�������AFALSE:���֌W�����ł��Ⴂ��
#--------
#�J�����g�f�B���N�g�����f�[�^������Ƃ���ɕύX
setwd("C:/Users/81802/OneDrive/���H��/���Ƃ̎���/2021 1Q/�m���E���v�w/���ԃ��|�[�g")
#---------�t�@�C���ǂݍ���--------------------------
#df_covid19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_country.csv")
#�J���}��؂�(CSV)�`���̃t�@�C�����f�[�^�t���[���Ƃ��ēǂݍ���
df_covid19 <- read.csv(Covid19_file, sep=",",header=T)
df_population <- read.csv(population_file, sep=",", header=T)
#���ւ𒲂ׂ����f�[�^��ǂݍ���
target_data <- read.table(target_file, sep=",", header=T)

#----------�f�[�^�����ꂢ�ɂ���-----------------------
#2021�N�̃f�[�^�݂̂��c��
df_population2021 <- subset(df_population, Time=="2021")
#Variant_name�̂ݎc��
df_population2021_Medium <- subset(df_population2021, Variant==Variant_name)
#�A���t�@�x�b�g���ɕ��ׂȂ���
df_population2021_Medium <- df_population2021_Medium[order(df_population2021_Medium[,2]),]
#�񖼂�Location�ɂ���
colnames(df_covid19)[1] <- "Location"

#���A�̃f�[�^�̍��ƒn��ȊO�̑傫���G���A(�A�t���J�S�̂̐l���Ȃǁj������
#1�s�ڂ���189�s�ڂ܂�1�s����������
for(x in 1:189){
  #�����ԍ��������l�ɂȂ�܂ŏ���������(���f�[�^���A���t�@�x�b�g���ɕ���ł���̂ŁA���Ԃ�Ȃ��ԍ��̎��ɂ��̍s�𔲂����Ƃő�������)
  while(df_population2021_Medium[x,1]!=df_covid19[x,13]){
    #�l���̃f�[�^��x�s�ڂ���菜��
    df_population2021_Medium <- df_population2021_Medium[-x,]
  }
}
#Location����ɂ���df_population2021_Medium,��df_covid19�̓����ɂ���
merged_data <- merge(df_population2021_Medium, df_covid19, by="Location")
#100���l������̎��Ґ����v�Z���A���ꂼ��x�N�g���ɓ����
Death_per_1M <- round((merged_data$Deaths/merged_data$PopTotal) * 1000, 3)
#100���l������̎��Ґ��Ƃ����V�����s�����
merged_data <- cbind(merged_data, Death_per_1M)
#NA���c��ƌ����l�̏��������ɂ����Ȃ�̂ŁA�K�v�ȗ�݂̂��c��
merged_data <- merged_data[,c(1,2,9,10,12,13,15,21:24)]

#-----�ɒ[�ɐl���̏��Ȃ���������-----------------------
#�������̃��[�v�Ŏ����������Ƃł���镪���J�E���g����
counter <- 0
#�P�s�ڂ���Ō�̍s�܂ň�s����������
for (x in 1:nrow(merged_data)) {
  #�l������l�ȉ��̏ꍇ
  if(merged_data[x-counter,3] <= excluded_population / 1000){
    #��(x-counter)�s����菜��
    merged_data <- merged_data[-(x-counter),]
    #�������s�����̂ŁA���炷�����P���₷
    counter = counter + 1
  }
}

#-----���ʂ������牽�Ԗڂ��ɕς���-----
#���ւ𒲂ׂ����f�[�^�̗�
element_count = ncol(target_data)
#worst_rank��true�̏ꍇ
if(worst_rank){
  #������̏��ʂɕς���
  rank = element_count - rank
}

#-----���ւ𒲂ׂ����f�[�^�̊e��ő��ւ𒲂ׂ�-----
#�e��̑��ւ��x�N�g���̊e�v�f�ɓ����Ƃ��ɕK�v�ȃx�N�g�����쐬
correlation_vector <- numeric(element_count-1)
data_count <- numeric(element_count-1)
#��Q�񂩂�target_data�̍Ō�̗�܂łP�񂸂������s��
for(x in 2:element_count){
  #merged_data��merged_data_1�ɑ��
  merged_data_1 <- merged_data
  #merged_data_1�̍Ō�̗�ɒ��ׂ����f�[�^��ISO3(JPN�Ȃ�)����ɂ�������
  merged_data_1 <- merge(merged_data_1, target_data[,c(1, x)], by="ISO3", all=T)
  #�f�[�^�����������s������
  merged_data_1 <- na.omit(merged_data_1)
  data_count[x-1] <- nrow(merged_data_1)
  #merged_data�̑�12��̕W���΍���0�łȂ��Ƃ�
  if(var(merged_data_1[,12]) != 0){
    #DI_Total��correlation_vector��x��ڂ̑��֌W��
    correlation_vector[x-1] <- cor(merged_data_1[,11],merged_data_1[,12]) 
  }
}

#-----���֌W�����ł�����������߂�------
#���֌W���̒Ⴂ�񂩂珇�ɕ��ׂāA�扽�񂩂�correlation_list�ɋL�q���Ă���
correlation_list <- sort.list(correlation_vector) + 1  #target_data�̑�Q�񂩂�Ō�̗�܂ł̕��בւ��ł��邽�߁A���ׂĂ̗�1���������Ƃɒ���
#���֌W���̍����񂩂珇�ɕ��ׂĂ��̗��correlation_list�ɋL�q���Ă���
correlation_list <- rev(correlation_list)
saved_correlation_vector <- correlation_vector
#���֌W�����~���ɕ��בւ�[������]
correlation_vector <- rev(sort(correlation_vector))
#rank�Ԗڂ̑��֌W���Ƃ��̍��ڂ���
correlation_rank <- correlation_vector[rank]
correlation_rank_target_data <- names(target_data)[correlation_list[rank]]
#���֌W����rank�Ԗڂ̗�̊e�l��\��
print(correlation_rank)�@                   #���֌W��
print(correlation_rank_target_data)         #�N
print(data_count[correlation_list[rank]-1]) #�f�[�^��

#�ł��V�����N�̊e�l��\��
print(saved_correlation_vector[element_count-1])#���֌W��
print(names(target_data)[element_count-1])       #�N
print(data_count[element_count-1])               #�f�[�^��

#�ł��V�����N�̃f�[�^�������Ȃ��ꍇ�Ɏg�p
print(saved_correlation_vector[element_count-2])#���֌W��
print(names(target_data)[element_count-2])       #�N
print(data_count[element_count-2])               #�f�[�^��

#���֌W���̐���
plot(saved_correlation_vector,type="l")