### 1. Напишіть функцію prepare_set <- function(file_name) {} яка в якості аргументу приймає ім’я файлу і повертає дата фрейм. Збережіть цей дата фрейм в змінну olympics olympics <- prepare_set(“olympics.csv”)

```{r}

install.packages("stringr")

library("stringr")
prepare_set <- function(namefile) 
{
  df <- read.csv(namefile, skip = 1, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)

### 2. Першому стовпцю дати назву 'Country'

names(df)[1] <- "Country"

### 3. Автоматично в циклі згенерувати назви останніх стовпців за наступними правилами:
###3.1. Видалити з назви “X.U.2116..”, тобто “X.U.2116..Summer” буде “Summer”
###3.2. “X01..” замінити на “Gold”, тобто “ X01...1” буде “Gold.1”
###3.3. “X02..” та “X03..” замінити на “Silver” та “Bronze” відповідно
###В результаті повинні бути наступні назви стовпців: "Country", "Summer", "Gold", "Silver", "Bronze", "Total", "Winter", "Gold.1", "Silver.1", "Bronze.1", "Total.1", "Games", "Gold.2", "Silver.2", "Bronze.2", "Combined.total"

  nam <- names(df)
  for (i in 2:length(names(df))){ #конвер. довжини назви стовпця
    if (substr(nam[i],1,3) == "X.."){ #substr (x, start, stop) - заміна назви стов. на симв. вектор
      names(df)[i] <- substr(nam[i],4,nchar(nam[i]))
    } else if(substr(nam[i],1,2) == "X0"){
      n <- substr(nam[i],3,3)
      if (n == "1"){
        names(df)[i] <- paste0("Gold",substr(nam[i],6,nchar(nam[i])))
      } else if (n == "2"){
        names(df)[i] <- paste0("Silver",substr(nam[i],6,nchar(nam[i])))
      } else if (n == "3"){
        names(df)[i] <- paste0("Bronze",substr(nam[i],6,nchar(nam[i])))
      }
    }
  }

### 4. Необхідно привести назви країн до виду "Afghanistan", "Algeria" і т.п. Для цього можна використати функцію str_split бібліотеки stringr. В назві країн не повинно бути пробілів на початку та в кінці.
  splited <- str_split(df$Country," ")
  for (i in 1:length(splited)){ 
    df$Country[i] <- splited[[i]][1]
  }
  id <- vector()
  for(i in 1:length(df$Country)){
    id <- c(id,toupper(substr(df$Country[i],1,3)))
  }
### 5 Додайте до дата фрейму новий стовпець “ID”, в який запишіть трибуквений код країна. Наприклад, "AFG", "ALG" і т.п.

   df$ID <- id 

### 6 Видаліть з дата фрейму останню строку “Totals”
    df <- df[-dim(df)[1],]
  
    return (df)
  }
olympics <- prepare_set("olympics.csv")
olympics


#Для кожного наступного питання напишіть функцію, яка повертає вказаний результат. Назви функції “answer_one” для питання 1, “answer_two” для питання 2 і т.д.

## Питання 1 Котра країна виграла найбільшу кількість золотих нагород на літніх іграх? Функція повинна повернути одне текстове значення.

answer_one <- olympics$Country[which.max(olympics$Gold)]
answer_one

[1] "United States"

## Питання 2 Яка країна має найбільшу різницю між кількістю нагород на літніх та зимових іграх? Функція повинна повернути одне текстове значення.

answer_two <- olympics$Country[which.max(abs(olympics$Total - olympics$Total.1))]
answer_two

[1] "United States"


## Питання 3 В якій крайні найбільша різниця між літніми та зимовими золотими нагородами відносно до загальної кількості нагород (Summer Gold - Winter Gold) / Total Gold. Врахувати тільки країни які мають як мінімум по одній нагороді в літніх та зимових іграх. Функція повинна повернути одне текстове значення.

##На жаль, не вийшло :( 

### Питання 4 Необхідно знайти кількість балів по кожній крайні. Бали рахуються наступним чином: Золота нагорода Gold.2 це три бали, срібна Silver.2 - 2 бали та бронзова Bronze.2 – 1 бал. Функція повинна повертати дата фрейм довжиною 146, який складається з двох колонок: "Country", "Points".

scores <- apply(olympics[,13:15],1,function (x) x[1]*3 + x[2]*2 + x[3]*1)
answer_four <- data.frame(Country = olympics$Country,Points = scores)
answer_four
 Country Points
1                        Afghanistan      2
2                            Algeria     27
3                          Argentina    130
4                            Armenia     16
5                        Australasia     22
6                          Australia    923
7                            Austria    569
8                         Azerbaijan     43
9                            Bahamas     24
10                           Bahrain      1
11                          Barbados      1
12                           Belarus    154
13                           Belgium    276
14                           Bermuda      1
15                           Bohemia      5
16                          Botswana      2
17                            Brazil    184
18               British West Indies      2
19                          Bulgaria    411
20                           Burundi      3
21                          Cameroon     12
22                            Canada    846
23                             Chile     24
24                             China   1120
25                          Colombia     29
26                        Costa Rica      7
27                       Ivory Coast      2
28                           Croatia     67
29                              Cuba    420
30                            Cyprus      2
31                    Czech Republic    134
32                    Czechoslovakia    327
33                           Denmark    335
34                          Djibouti      1
35                Dominican Republic     14
36                           Ecuador      5
37                             Egypt     49
38                           Eritrea      1
39                           Estonia     77
40                          Ethiopia     94
41                           Finland    895
42                            France   1500
43                             Gabon      2
44                           Georgia     42
45                           Germany   1546
46            United Team of Germany    269
47                      East Germany   1068
48                      West Germany    459
49                             Ghana      5
50                     Great Britain   1574
51                            Greece    213
52                           Grenada      3
53                         Guatemala      2
54                            Guyana      1
55                             Haiti      3
56                         Hong Kong      6
57                           Hungary    962
58                           Iceland      6
59                             India     50
60                         Indonesia     49
61                              Iran    110
62                              Iraq      1
63                           Ireland     55
64                            Israel     10
65                             Italy   1333
66                           Jamaica    131
67                             Japan    866
68                        Kazakhstan    113
69                             Kenya    168
70                       North Korea     90
71                       South Korea    609
72                            Kuwait      2
73                        Kyrgyzstan      4
74                            Latvia     47
75                           Lebanon      6
76                     Liechtenstein     15
77                         Lithuania     38
78                        Luxembourg      9
79                         Macedonia      1
80                          Malaysia      9
81                         Mauritius      1
82                            Mexico    109
83                           Moldova      9
84                          Mongolia     37
85                        Montenegro      2
86                           Morocco     39
87                        Mozambique      4
88                           Namibia      8
89                       Netherlands    727
90              Netherlands Antilles      2
91                       New Zealand    203
92                             Niger      1
93                           Nigeria     37
94                            Norway    985
95                          Pakistan     19
96                            Panama      5
97                          Paraguay      2
98                              Peru      9
99                       Philippines     11
100                           Poland    520
101                         Portugal     39
102                      Puerto Rico     10
103                            Qatar      4
104                          Romania    572
105                           Russia   1042
106                   Russian Empire     14
107                     Soviet Union   2526
108                     Unified Team    287
109                     Saudi Arabia      4
110                          Senegal      2
111                           Serbia     11
112            Serbia and Montenegro     17
113                        Singapore      6
114                         Slovakia     58
115                         Slovenia     56
116                     South Africa    148
117                            Spain    268
118                        Sri Lanka      4
119                            Sudan      2
120                         Suriname      4
121                           Sweden   1217
122                      Switzerland    630
123                            Syria      6
124                   Chinese Taipei     32
125                       Tajikistan      4
126                         Tanzania      4
127                         Thailand     44
128                             Togo      1
129                            Tonga      2
130              Trinidad and Tobago     27
131                          Tunisia     19
132                           Turkey    191
133                           Uganda     14
134                          Ukraine    220
135             United Arab Emirates      3
136                    United States   5684
137                          Uruguay     16
138                       Uzbekistan     38
139                        Venezuela     18
140                          Vietnam      4
141                   Virgin Islands      2
142                       Yugoslavia    171
143 Independent Olympic Participants      4
144                           Zambia      3
145                         Zimbabwe     18
146                       Mixed team     38


```
