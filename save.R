#-----課題１-------------------------------
#カレントディレクトリをデータがあるところに変更
setwd("C:/Users/81802/OneDrive/東工大/授業の資料/2021 1Q/確率・統計学/中間レポート")

#カンマ区切り(CSV)形式のファイルをデータフレームとして読み込み
di <- read.table("DI4domain.csv", sep=",", header=T)

#diのヒストグラム作成
hist(di$Koyou, col="green", main="分野別月次DI:雇用の分布", xlab="DI", ylab="度数",breaks=seq(10, 70, 10))

#-----問２------------------------------
#カレントディレクトリをデータがあるところに変更
setwd("C:/Users/81802/OneDrive/東工大/授業の資料/2021 1Q/確率・統計学/中間レポート")

#カンマ区切り(CSV)形式のファイルをデータフレームとして読み込み
di_area <- read.table("DI4area.csv",sep=",", header=T)

#初期値
max_average <- 0　#第２列から第(x-1)列までのDIの平均値の最大値(暫定の最大値)
min_average <- 100#第２列から第(x-1)列までのDIの平均値の最小値(暫定の最小値)
max_row <- 0      #暫定の最大値がある列
min_row <- 0      #暫定の最小値がある列

#第２列から第１６列まで一列ずつ処理していく
for(x in 2:16){
  
  #第x列のDIの平均値が暫定の最大値より大きいとき
  if(mean(di_area[,x])>max_average){
    max_average <- mean(di_area[,x])#暫定の最大値を書き換え
    max_row <- names(di_area)[x]    #暫定の最大値がある列の書き換え
  }
  
  #第x列のDIの平均値が暫定の最小値より小さいとき
  if(mean(di_area[,x])<min_average){
    min_average <- mean(di_area[,x])#暫定の最小値を書き換え
    min_row <- names(di_area)[x]#暫定の最小値がある列の書き換え
  }
}

#期間内のDIの平均値が最も高い地域と、最も低い地域を表示
print(max_row)
print(min_row)

#箱ひげ図を作成
boxplot(di_area[[min_row]], di_area[[max_row]],names=c(min_row,max_row), main="地域別月次DI", xlab="地域", ylab="DI")

#-----課題３-------------------------------------------
#カレントディレクトリをデータがあるところに変更
setwd("C:/Users/81802/OneDrive/東工大/授業の資料/2021 1Q/確率・統計学/中間レポート")

#カンマ区切り(CSV)形式のファイルをデータフレームとして読み込み
di_area <- read.table("DI4area.csv",sep=",", header=T)
di <- read.table("DI4domain.csv", sep=",", header=T)

#初期値
min_cor <- 1 #企業動向関連月次DIから第２列から第(x-1)列までのそれぞれの月次DIに対するそれぞれの相関係数の最小値(暫定の相関係数の最小値)
min_cor_di_area <- "error"#暫定の相関係数の最小値がある列の名前

#第２列から最後の列まで１行ずつ処理を行う
for(x in 2:ncol(di_area)){
  
  #第x列との相関係数が暫定の相関係数の最小値より小さいとき
  if(cor(di$Kigyo,di_area[,x]) < min_cor){
    min_cor <- cor(di$Kigyo,di_area[,x])#暫定の相関係数の最小値を書き換え
    min_cor_di_area <- names(di_area)[x]#暫定の相関係数の最小値がある列の書き換え
  }
}

#最も相関が低いものを表示
print(min_cor_di_area)

#散布図を描画
plot(di$Kigyo, di_area[[min_cor_di_area]], xlim=c(10,80), ylim=c(10,80), main="企業動向関連月次DIから地域別月次DIに対する相関が最も小さい地域", xlab="企業動向関連:Kigyo", ylab=min_cor_di_area)


#----------課題4--------------------------------------------------
target_file <- "第三次産業.csv"
#----------------------------------------------
#カレントディレクトリをデータがあるところに変更
setwd("C:/Users/81802/OneDrive/東工大/授業の資料/2021 1Q/確率・統計学/中間レポート")

#カンマ区切り(CSV)形式のファイルをデータフレームとして読み込み
di <- read.table("DI4domain.csv", sep=",", header=T)
target_data <- read.table(target_file, sep=",", header=T)  #60*element_count

#両方のデータにデータがある月のみ残す
merged_data <- merge(di, target_data)
di_improved <- merged_data[,c(1:ncol(di))]
target_data <- merged_data[,c(1, c((ncol(di)+1):ncol(merged_data)))]

#target_dataの列の数
element_count <- ncol(target_data)
#-----------------------------------------------
#1～element_count-1まで
rank <- 1
#----------------------------------------------
#要素がある列の数ぶんの零ベクトル
correlation_vector <- numeric(element_count-1)

#第２列からtarget_dataの最後の列まで１行ずつ処理を行う
for(x in 2:element_count){
  
  #target_dataの第ｘ列の標準偏差が0でないとき
  if(var(target_data[,x]) != 0){
    
    #DI_Totalとcorrelation_vectorのx列目の相関係数
    correlation_vector[x-1] <- cor(di_improved[,2],target_data[,x]) 
  }
}

#相関係数の低い列から順に並べて、第何列かをcorrelation_listに記述していく
correlation_list <- sort.list(correlation_vector) + 1  #target_dataの第２列から最後の列までの並べ替えであるため、すべての列が1小さいことに注意

#相関係数の高い列から順に並べてその列をcorrelation_listに記述していく
correlation_list <- rev(correlation_list)

#相関係数を降順に並べ替え[正→負]
correlation_vector <- rev(sort(correlation_vector))

#rank番目の相関係数とその項目を代入
correlation_rank <- correlation_vector[rank]
correlation_rank_target_data <- names(target_data)[correlation_list[rank]]

#相関係数が最も高い順位の相関係数とその項目を表示
print(correlation_rank)
print(correlation_rank_target_data)

#折れ線グラフ作成
merged_data <- merge(di_improved[,c(1,2)], target_data[,c(1,correlation_list[rank])], all=T)  #DI_Totalとtarget_dataの相関係数一位の列をくっつける
default_mai <-par()　　#デフォルトの値を保存
mai <- par()$mai       #グラフパラメータの設定
mai[4] <- mai[1]       #余白サイズの設定(上下と左右の幅を揃える)
par(mai = mai)         #指定した余白サイズの適用

#target_dataの相関関係が高い列の時系列の折れ線グラフ表示
plot(merged_data[,3], type="l", xlab=names(merged_data)[1],ylab=names(merged_data)[3], main="全国DIに最も相関のあるデータ")

#二つのグラフを同時に表示
par(new=T)    
plot(merged_data[,2], type="l", xlab="", ylab="", col="red", axes=FALSE, ylim= c(10,70))   #DI_Totalの時系列の折れ線グラフ
#2軸目の表示
axis(side=4)    #目盛りを右に表示
# 2軸目のラベル設定
mtext(names(merged_data)[2],side=4,line=2)   #(ラベル名, ラベルの置く位置, グラフの枠からの距離)
par(mai = default_mai$mai)  #デフォルトの値に戻す



#-----課題５------------------------------
#変更部
Variant_name <- "Medium"　#人口のVariantを決める
Covid19_file <- "Covid19.csv" #処理済み(国名の表記を国連に合わせる)
population_file <- "WPP2019_TotalPopulationBySex.csv" #文字化け修正済み
target_file <- "doctor_count.csv"　#１列目が、ISO3の国名コードで、2列目以降が各年の調べたいデータ
excluded_population <- 100000　#この値以下の人口の国を除く
rank <- 1　　#target_fileの中で相関係数が何番目に強い列を表示するか
worst_rank <- FALSE　#TRUE:target_fileの中で相関係数が最も高い列、FALSE:相関係数が最も低い列
#--------
#カレントディレクトリをデータがあるところに変更
setwd("C:/Users/81802/OneDrive/東工大/授業の資料/2021 1Q/確率・統計学/中間レポート")
#---------ファイル読み込み--------------------------
#df_covid19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_country.csv")
#カンマ区切り(CSV)形式のファイルをデータフレームとして読み込み
df_covid19 <- read.csv(Covid19_file, sep=",",header=T)
df_population <- read.csv(population_file, sep=",", header=T)
#相関を調べたいデータを読み込み
target_data <- read.table(target_file, sep=",", header=T)

#----------データをきれいにする-----------------------
#2021年のデータのみを残す
df_population2021 <- subset(df_population, Time=="2021")
#Variant_nameのみ残す
df_population2021_Medium <- subset(df_population2021, Variant==Variant_name)
#アルファベット順に並べなおす
df_population2021_Medium <- df_population2021_Medium[order(df_population2021_Medium[,2]),]
#列名をLocationにする
colnames(df_covid19)[1] <- "Location"

#国連のデータの国と地域以外の大きいエリア(アフリカ全体の人口など）を除く
#1行目から189行目まで1行ずつ処理する
for(x in 1:189){
  #国名番号が同じ値になるまで消し続ける(両データがアルファベット順に並んでいるので、かぶらない番号の時にその行を抜くことで揃えられる)
  while(df_population2021_Medium[x,1]!=df_covid19[x,13]){
    #人口のデータのx行目を取り除く
    df_population2021_Medium <- df_population2021_Medium[-x,]
  }
}
#Locationを基準にしてdf_population2021_Medium,とdf_covid19の二つを一つにする
merged_data <- merge(df_population2021_Medium, df_covid19, by="Location")
#100万人当たりの死者数を計算し、それぞれベクトルに入れる
Death_per_1M <- round((merged_data$Deaths/merged_data$PopTotal) * 1000, 3)
#100万人当たりの死者数という新しい行を作る
merged_data <- cbind(merged_data, Death_per_1M)
#NAが残ると欠損値の処理がしにくくなるので、必要な列のみを残す
merged_data <- merged_data[,c(1,2,9,10,12,13,15,21:24)]

#-----極端に人口の少ない国を除く-----------------------
#次処理のループで取り消したことでずれる分をカウントする
counter <- 0
#１行目から最後の行まで一行ずつ処理する
for (x in 1:nrow(merged_data)) {
  #人口が基準値以下の場合
  if(merged_data[x-counter,3] <= excluded_population / 1000){
    #第(x-counter)行を取り除く
    merged_data <- merged_data[-(x-counter),]
    #次から一行ずれるので、ずらす分を１増やす
    counter = counter + 1
  }
}

#-----順位を下から何番目かに変える-----
#相関を調べたいデータの列数
element_count = ncol(target_data)
#worst_rankがtrueの場合
if(worst_rank){
  #下からの順位に変える
  rank = element_count - rank
}

#-----相関を調べたいデータの各列で相関を調べる-----
#各列の相関をベクトルの各要素に入れるときに必要なベクトルを作成
correlation_vector <- numeric(element_count-1)
data_count <- numeric(element_count-1)
#第２列からtarget_dataの最後の列まで１列ずつ処理を行う
for(x in 2:element_count){
  #merged_dataをmerged_data_1に代入
  merged_data_1 <- merged_data
  #merged_data_1の最後の列に調べたいデータをISO3(JPNなど)を基準にくっつける
  merged_data_1 <- merge(merged_data_1, target_data[,c(1, x)], by="ISO3", all=T)
  #データが欠損した行を除く
  merged_data_1 <- na.omit(merged_data_1)
  data_count[x-1] <- nrow(merged_data_1)
  #merged_dataの第12列の標準偏差が0でないとき
  if(var(merged_data_1[,12]) != 0){
    #DI_Totalとcorrelation_vectorのx列目の相関係数
    correlation_vector[x-1] <- cor(merged_data_1[,11],merged_data_1[,12]) 
  }
}

#-----相関係数が最も高い列を求める------
#相関係数の低い列から順に並べて、第何列かをcorrelation_listに記述していく
correlation_list <- sort.list(correlation_vector) + 1  #target_dataの第２列から最後の列までの並べ替えであるため、すべての列が1小さいことに注意
#相関係数の高い列から順に並べてその列をcorrelation_listに記述していく
correlation_list <- rev(correlation_list)
saved_correlation_vector <- correlation_vector
#相関係数を降順に並べ替え[正→負]
correlation_vector <- rev(sort(correlation_vector))
#rank番目の相関係数とその項目を代入
correlation_rank <- correlation_vector[rank]
correlation_rank_target_data <- names(target_data)[correlation_list[rank]]
#相関係数がrank番目の列の各値を表示
print(correlation_rank)　                   #相関係数
print(correlation_rank_target_data)         #年
print(data_count[correlation_list[rank]-1]) #データ数

#最も新しい年の各値を表示
print(saved_correlation_vector[element_count-1])#相関係数
print(names(target_data)[element_count-1])       #年
print(data_count[element_count-1])               #データ数

#最も新しい年のデータ数が少ない場合に使用
print(saved_correlation_vector[element_count-2])#相関係数
print(names(target_data)[element_count-2])       #年
print(data_count[element_count-2])               #データ数

#相関係数の推移
plot(saved_correlation_vector,type="l")
