## Для лабораторної роботи необхідно завантажити zip файл з даними за
## посиланням: link(https://www.dropbox.com/s/i9wi47oyhfb7qlh/rprog_data_specdata.zip?dl=0)
## Це файл містить 332 csv файлів, що містять у собі результати спостережень за
## забрудненням повітря дрібнодисперсними частками (fine particular matter air
## pollution) у 332 локаціях у США. Кожен файл містить дані з одного монітору. ID
## номер кожного монітору міститься у назві файлу. Наприклад, дані з монітору
## під номером 200 містяться у файлі «200.csv». Кожен файл містить три змінні:
## Data: дата спостереження в форматі (рік-місяць-день), sulfate: рівень
## сульфатних часток в повітрі на дату (мікрограми на кубічний метр) та nitrate:
## рівень нітратних часток в повітрі на дату (мікрограми на кубічний метр). Для цій
## роботи необхідно додати на Github файл pmean.R, який містить усі функції. В
## файлі md необхідно указати виклик функції з аргументами та вивід у консоль
## результатів роботи функцій.

# 1. Функція pmean - обчислює середняє значення забруднення сульфатами або нітратами серед заданого переліка моніторів.
```{r}
pmean <- function(directory, pollutant, id = 1:332)
{
  vmeans <- vector() #ініц пустого вектора для подальшого запису елементів файлу
  drct <- unzip(paste0(directory, ".zip")) #розпак архіву для читання даних
    for (i in id) #ініц циклу
  {
    vmeans <- c(vmeans,mean(read.csv(drct[i])[pollutant],na.rm=TRUE)) #запису у вектор середніх значень стовбця кожноо файлу
  }
  return(vmeans) #вивід результатів
}
### ?повертає помилку argument is not numeric or logical: returning NAargument is not numeric or logical: returning NA[1] NA NA?
pmean ("specdata", "sulfate", 100:101)
```

# 2. функція complete - виводить кількість повних спостережень для кожного файлу

```{r}
complete <- function (directory, id = 1:322) 
{
  drct <- unzip(paste0(directory, ".zip"))#розпак архіву для читання даних
  df <- data.frame() #ініц пустого DF для подальшого запису елементів файлу
  for (i in id) #ініц циклу
  {
    fl <- read.csv(drct[i]) #зчит файлу у ДФ
    df <- rbind(df,data.frame(id = i, nobs = sum(!is.na(rowSums(fl[,2:3]))))) #записує у ДФ суму строк, де у 2 і 3 стовпця не NA
  }
return(df)
}
complete("specdata", 1)
```

# 3. фунція corr - обчислює кореляцію між сульфатами та нітратами для моніторів, кількість спостережень яких більша за порогове значенн

```{r}
corr <-  function (directory, threshold = 0){
  drct <- unzip(paste0(directory,".zip")) #розпак архіву для читання даних
  kil <- length(drct) #запис кількості файлів у ахріві
  corr1 <- vector('numeric')
  for(i in 1:kil){
    filee <-  read.csv(drct[i]) #зчитування окремого файлу 
    if (sum(!is.na(rowSums(filee[,2:3]))) > threshold){
 corr1 <- c(corr1,cor(filee[["sulfate"]],filee[["nitrate"]], use = "complete.obs" ))
      return(corr1)
    }
      else {
        return(f <- vector('numeric'))
        }
  }
}
#?повертає помилку Error in corr1[["nitrate"]] : subscript out of bounds?

#cor(x, y)возвращает параметрический коэффициент корреляции Пирсона (или корреляционную матрицу, если х и у являются матрицами или таблицами данных)
#complete.obs приведет к получению корреляционной матрицы без NA С.
cr <- corr("specdata", 10)
print(cr)
```
