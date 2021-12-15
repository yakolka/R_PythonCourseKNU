### Файл необхідно завантажити в змінну census_df. Зчитуємо файл census_df <- read.csv("census.csv", stringsAsFactors = FALSE)

```{r}
census_df <- read.csv("census.csv", stringsAsFactors = FALSE)

### 5 Питання 5 В якому штаті (state) більше всього округів (county)? Функція повинна повернути одне текстове значення

names(which.max(sapply(split(census_df$COUNTY,census_df$STNAME),length)))

[1] "Texas"

### 6 Якщо розглядати три найбільш населених округа (county) з кожного штату, які три найбільш населені штати (в порядку з більш до менш населеного)? Використовуйте CENSUS2010POP. Функція повинна повернути вектор з трьох текстових значень.

sort_county <- sapply(split(census_df$CENSUS2010POP,census_df$STNAME),function (x) sort(x,decreasing = T))
max_sums <- sort(sapply(sort_county,function (x) sum(x[2:4])), decreasing = TRUE)
names(max_sums[1:3])

[1] "California" "Texas"      "Illinois"  

### 7 Який округ (county) має найбільшу абсолютну зміну в населенні протягом періоду 2010-2015? (Підказка: значення населення зберігається в колонках з POPESTIMATE2010 до POPESTIMATE2015. Необхідно розглядати всі шість колонок). Якщо населення округу за 5річний період 100, 120, 80, 105, 100, 130, то найбільша різниця за період буде |130-80|=50. Функція повинна повернути одне текстове значення.

pops <- c("POPESTIMATE2010","POPESTIMATE2011","POPESTIMATE2012","POPESTIMATE2013","POPESTIMATE2014","POPESTIMATE2015")
new_census <- census_df[census_df$COUNTY != 0,]
n_max <- which.max(apply(new_census[pops],1,function (x) abs(range(x)[1] - range(x)[2])))
new_census[n_max,"CTYNAME"]

[1] "Harris County"

### 8 В census_df США поділені на 4 регіони (колонка "REGION"). Напишіть функцію, яка знаходить округи (county), що належать регіонам 1 або 2, назва яких починається з "Washington" та POPESTIMATE2015 більше ніж POPESTIMATE2014. Функція повинна повернути 5х2 дата фрейм з колонками "STNAME", "CTYNAME".

with_names <- census_df[substr(census_df$CTYNAME,1,10) == "Washington",]
first_second <- with_names[with_names$REGION == 1 | with_names$REGION == 2 ,]
POPS <- first_second[first_second$POPESTIMATE2015 > first_second$POPESTIMATE2014,]
POPS[c("STNAME", "CTYNAME")]


STNAME  CTYNAME
<chr>   <chr>
897	Iowa	Washington County		
1420	Minnesota	Washington County		
2346	Pennsylvania	Washington County		
2356	Rhode Island	Washington County		
3164	Wisconsin	Washington County
```

