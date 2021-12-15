# Function #1

```{r}
pmean ("specdata", "sulfate", 1:10)

pmean ("specdata", "sulfate", 55)

pmean ("specdata", "nitrate")
```
повертає помилку: returning NAargument is not numeric or logical: returning NA [1] NA NA NA NA NA NA NA NA NA NA

# Function #2

```{r}
complete("specdata", 1)
```
id nobs  
1 	117  

```{r}
complete("specdata", c(2, 4, 8, 10, 12))
```
id nobs  
2	1041  
4	474  
8	192  
10 148  
12	96  

```{r}
complete("specdata", 50:60)
```
id nobs  
50	459  
51	193  
52	812  
53	342  
54	219  
55	372  
56	642  
57	452  
58	391  
59	445  
60  448  

# Function #3
```{r}
cr <- corr("specdata", 150)
print(cr)

 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
 -0.07588814
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
```


```{r}
cr <- corr("specdata", 400)
print(cr)

 -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884
 -0.15782860
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 

```

```{r}
cr <- corr("specdata", 5000)
print(cr)

numeric(0)
0
"numeric"
```




