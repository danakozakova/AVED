# Týden 3: Datové struktury a faktory
# https://is.muni.cz/auth/el/econ/podzim2025/MPE_AVED/um/exercises/ex03.html
rm(list = ls())
###############################################################################
# Na zahřátí
###############################################################################
### UKOL 1 ###
# Nejprve načtěte balík tibble.
library(tibble)

### UKOL 2 ###
# Vytvořte tabulku df třídy tibble, který bude obsahovat následující proměnné (sloupce):
  
#   id … hodnoty 1, 2, 3, 4 a 5 typu integer
# value … hodnoty 1.3, 1.7, 2.7, 3.15 a 17

df <- tibble(id = 1:5,
             value = c(1.3, 1.7, 2.7, 3.15, 17))

### UKOL 3 ###
# Z tabulky df vyberte 
# první dva řádky
df[1:2,]

# tabulku, která obsahuje jen sloupec value
df[,"value"]

# vektor value
df$value # tohle je vektor

df[["value"]] # aj takto, fajn pouzitie, ked je nazov stlpca viacslovny

### UKOL 4 ###
# Načtěte data uložená v souboru “exdata03.RData”.
load("exdata03.RData")

###############################################################################
# Opáčko z makra
###############################################################################
# Tabulka macro obsahuje tři proměnné:
#   time … roky
#   gdp_nominal … HDP v běžných cenách v mil. Kč
#   gdp_real … HDP ve stálých cenách roku 2010 v mil. Kč
# Zdroj dat: Eurostat.

macro

### UKOL 1 ###
# Přidejte do tabulky následující proměnné:
#   deflator … deflátor HDP – spočítáte jako Y_t/y_t * 100 pro kazde t, 
#     kde Y_t je HDP v běžných cenách a y_t HDP ve stálých cenách v čase 
macro$deflator <- (macro$gdp_nominal /macro$gdp_real)*100

#   inflation … míru inflace – míra inflace v roce t je P_t/P_t-1, kde P_t je hodnota deflátoru v roce t
macro$inflation <- c(NA, macro$deflator[-1]/macro$deflator[-nrow(macro)] - 1)

#   real_growth … míru růstu reálného HDP – míra růstu v čase t je y_t/y_t-1 
macro$gdp_growth <- c(NA, macro$gdp_real[-1]/macro$gdp_real[-nrow(macro)] -1)
# Pozor! Proměnné inflation a real_growth mají jinou délku než ostatní vektory. Vyřešte to tak, abyste neztratili žádná data.

macro

### UKOL 2 ###
# Vykreslete do grafu vývoj inflace a reálného růstu v čase.
plot(x = macro$time, y = macro$inflation, type = "l", 
     ylim = range(c(macro$inflation, macro$gdp_growth), na.rm = TRUE))
lines(x = macro$time, y = macro$gdp_growth, col = "blue")
abline(h = 0, lty = 3)
legend(2014, 0.1,
       col = c("black", "blue"),
       legend = c("inflation", "real_growth"),
       lty = 1)

### UKOL 3 ###
# Spočítejte korelaci inflace a růstu reálného HDP v čase pomocí funkce cor(); 
# chybějících hodnot se zbavíte parametrem use = "complete.obs".
cor(macro$inflation, macro$gdp_growth, use ="complete.obs")

###############################################################################
# Jestli přivedeš domů nějakou obezitu…
###############################################################################
# Tabulka experiment (třída tibble) obsahuje data o subjektech nějakého lékařského experimentu. Sloupce tabulky mají následující význam:
#   
#   id … jednoznačné identifikační číslo subjektu
#   name … jméno subjektu
#   height … výška subjektu
#   weight … váha subjektu
experiment

### UKOL 1 ###
# Zjistěte, kolik máme pozorování.
pocet <- nrow(experiment)
pocet
length(experiment) # pocet slopcu
length(experiment$id) # pocet prvku v sloupci id

### UKOL 2 ###
# Přidejte do tabulky proměnnou (sloupec) bmi, která bude obsahovat BMI jednotlivých subjektů. 
# BMI vypočítáte jako w/h^2, kde m je váha v kg a h je výška v metrech.
experiment$bmi <- experiment$weight/((experiment$height/100)^2)

### UKOL 3 ###
# Přidejte do tabulky proměnnou gender, která bude obsahovat pohlaví subjektů. Postupujte ve třech krocích:
# 1.  Výraz stringr::str_detect(experiment$name, "[ae]$") vrátí hodnotu TRUE pro všechna jména, 
# která končí na “a” nebo “e” a FALSE pro ostatní. Většina ženských jmen tak bude mít hodnotu TRUE a všechna mužská jména hodnotu FALSE.
stringr::str_detect(experiment$name, "[ae]$")

# 2. Převeďte logické hodnoty na faktor tak, aby jeho hodnoty byly “female” a “male” (v tomto pořadí).
experiment$gender <- stringr::str_detect(experiment$name, "[ae]$") |> 
  factor(levels = c(TRUE, FALSE), labels = c("female", "male"))

# 3. Je potřeba opravit hodnotu pro jména “Miriam” a “Ingrid”.
experiment$gender[experiment$name == "Miriam" | experiment$name == "Ingrid"] <- "female" # samostatne porovnavat je rychlejsi
experiment$gender[experiment$name %in% c("Miriam", "Ingrid")] <- "female" # krajsie

# Změňte pořadí proměnných v tabulce tak, aby bylo id, name, gender, height, weight a bmi.

experiment <- experiment[, c("id", "name", "gender", "height", "weight", "bmi")]

### UKOL 4 ###
# Spočítejte medián BMI pro celý vzorek, jen pro muže a jen pro ženy.
BMI <-  median(experiment$bmi)
BMI

BMI_male <- median(experiment$bmi[experiment$gender == "male"])
BMI_male

BMI_female <- median(experiment$bmi[experiment$gender == "female"])
BMI_female

### UKOL 5 ###
# Přidejte do tabulky proměnnou obese, která bude obsahovat logickou hodnotu TRUE u subjektů, 
# které mají BMI >= 30.
experiment$obese <- experiment$bmi >= 30
experiment

### UKOL 6 ###
# Vytvořte tabulku obese, který bude obsahovat pouze ty subjekty, které jsou obézní. 
# Odstraňte z tabulky (v něm zbytečnou) proměnnou obese.

obese <- experiment[experiment$obese,]
obese$obese <- NULL
obese

### UKOL 7 ###
# Seřaďte subjekty v tabulce obese od subjektu s nejvyšším BMI po subjekt s nejnižším BMI. 
obese <- obese[order(-obese$bmi),]
obese

# Vypište pouze 5 nejobéznějších subjektů.
obese|> head(5)

### UKOL 8 ###
# Experiment bude probíhat pouze na obézních subjektech. Potřebujete je rozdělit do dvou skupin: 
# na treatmentovou a kontrolní skupinu. Do treatmentové skupiny chcete vybrat náhodně polovinu subjektů; 
# zbytek bude kontrolní skupina. Informace o tom, do jaké skupiny subjekt patří, bude obsahovat proměnná group, 
# která bude faktor o dvou hodnotách: “treatment” a “control”.

# Nápověda:
# - výraz sample(N, n), kde N je jedno celé číslo, vrátí náhodný výběr n hodnot z čísel 1, 2 .. N bez opakování
# - výraz sample(v, n), kde v je vektor délky aspoň 2, vrátí náhodný výběř n hodnot z vektoru v bez opakování
# Pozor: Protože rozdělujeme subjekty do skupin náhodně, při každém běhu vyjdou jiné výsledky – leda, 
# že bychom nastavili počáteční stav generátoru náhodných čísel pomocí funkce set.seed().
sample(pocet, pocet)
# vyber polovicu cisel zo vsetkych moznych
pocet_obese <- nrow(obese)
treat <- sample(pocet_obese, pocet_obese/2)

obese$group <- FALSE
obese$group[treat] <- TRUE

obese$group <- factor(obese$group, levels = c(TRUE, FALSE), labels = c("treatment", "control"))
obese

# ine riesenie
rep_len(1:2, 10)
rep_len(c("treatment", "control"), 20)

obese$group <- rep_len(c("treatment", "control"), pocet_obese) |>
  factor()
obese

### UKOL 9 ###




#############################################################################
rm(list = ls())
# Kapitola 4.2 Atomické matice a nasledujúce
# https://aved.econ.muni.cz/data_structures.html
# matice se třemi řádky a čtyřmi sloupci, po sloupcích
matrix(1:12, nrow = 3)
matrix(1:12, nrow = 3, byrow = TRUE)

matrix(1:11, nrow = 3, byrow = TRUE)

matrix(1:12, nrow = 3, ncol = 5,  byrow = TRUE)

###
m <- matrix(1:12, nrow = 3)
m
m |> is.matrix()
m |> nrow()
m |> ncol()
m |> dim()
m |> length()
m |> attributes()

###
A <- matrix(1:12, nrow = 3)
B <- matrix(101:112, nrow = 3)
A
B
rbind(A, B)
cbind(A, B)

rownames(A) <- c("a", "b", "c")
colnames(A) <- c("alpha", "beta", "gamma", "delta")
A

dimnames(A) <- list(id = c("A", "B", "C"), variables = c("Alpha", "Beta", "Gamma", "Delta"))
A

A |> attributes()

# z vektora matica
M <- 1:12
M
M |> dim()
M |> is.matrix()
M |> is.vector()

dim(M) <- c(3, 4)
M
M |> dim()
M |> is.matrix()
M |> is.vector()

# operacie
M <- matrix(c(1:8, 0), nrow = 3)
M
solve(M)
M%*%solve(M)
E <- diag(1, nrow = nrow(M), ncol = ncol(M))
E
all.equal(M%*%solve(M), E)

diag(1, 4, 3)
diag(M)
diag(M, 4, 3)

# všechny řádky, ve kterých je prvek v prvním sloupci >= 2
M[M[,1] >= 2, ]

M
order(M)
order(M[,1])

# řádky matice seřazené podle posledního sloupce
M[, ncol(M)]
order(M[, ncol(M)])
M[order(M[, ncol(M)]),]

# nahodna permutace sloupcu
perm <- sample(ncol(M))
M[, perm]

M[, 1, drop = FALSE] # drop - aby sa nezmenila matica na vektor

# Zoznamy
l <- list(1L, 11, 1:3, "ahoj", list(1, 1:3, "ahoj"))
l
names(l)

l <- list(a = 1, b = "ahoj", c = 1:3, d = list(1:3, "ahoj"))
l
names(l)

is.list(l)
is.vector(l)
str(l)
lobstr::tree(l)

#
l <- list(a = 1, b = 1:3, c = "ahoj")
l[1]
l[[1]]
l$a

#list
l[1] |> is.list()
l[1] |> is.numeric()

#prvok
l[[1]] |> is.list()
l[[1]] |> is.numeric()

l$a |> is.list()
l$a |> is.numeric()

l[[2]][[3]]  # třetí prvek vektoru, který je druhým prvkem seznamu

# protože druhým prvkem seznamu je zde atomický vektor,
# mohou být druhé závorky jednoduché:
l[[2]][3]

l[["b"]]  # prvek se jménem b
l$b       # totéž (uvozovky se zde neuvádějí)


# chyba
l[["d"]]
l$d
l[[4]]

# partial matching
l <- list(prvni_prvek = 1, druhy_prvek = 2)
l$prvni
l[["prvni"]]
l[["prvni", exact = FALSE]]

# 4.4. Tabulky data.frame
experiment <- data.frame(id = c(1, 2, 3, 41),
                         vyska = c(158, 174, 167, 203),
                         vaha = c(51, 110, 68, 97))
experiment

experiment <- data.frame(
  id = c(1, 2, 3, 41),
  gender = "muž",
  vyska = c(158, 174, 167, 203),
  vaha = c(51, 110, 68, 97),
  zdravy = c(TRUE, TRUE, FALSE, TRUE)
)
experiment

# vsetky mozne kombinacie
expand.grid(
  x = 1:3,
  y = factor(c("male", "female")),
  z = c(TRUE, FALSE)
)

# ako pri matrix, funguje nrow, ncol, length, dim
# aj class, names, row.names

experiment |> nrow()
experiment |> ncol()
experiment |> length()
experiment |> dim()
experiment |> attributes()

colnames(experiment) <- c("id", "sex", "height", "weight", "healthy")
experiment

# použije automatickou konverzi na stejný typ
as.matrix(experiment)

# použije explicitní konverzi na reálná čísla
data.matrix(experiment)

M <- matrix(1:12, nrow = 3)
as.data.frame(M)
data.frame(M)

colnames(M) <- c("a", "b", "c", "d")
as.data.frame(M)
data.frame(M)

#
d <- data.frame(x = 1:7,
                y = c(3, 1, NA, 7, 5, 12, NA))
d

d$x
d[["x"]]
d[[1]]

d[1]
d["x"]

d[1:2]

d[1:2, "x"]
d[1:2, "x", drop = FALSE]

d[1:3, 1:2]
d[d[,"y"]<7,]
d[d$y< 7,]
d[d$y< 7 & !is.na(d$y),]

complete.cases(d)
d[complete.cases(d),]

d$z <- letters[1:nrow(d)]
d

d$z <- NULL
d

d[order(d$y),]
d[order(d$y),2:1]

# Tabulky tibble
library(tibble)
ds <- tibble(
  x = 1:1e6,
  y = 2 * x,
  z = x / 3 + 1.5 * y - 7)
ds

tribble(
  ~name, ~weight, ~height,
  "Adam", 68, 193,
  "Bětka", 55, 163,
  "Cyril", 103, 159
)

ds <- tibble(a = 1:3, b = 11:13)
ds
ds$c <- ds[, "a"]  # vybere se tibble s jedním sloupcem a vloží do sloupce c
ds

ds$c |> class()
ds$a |> class()

ds$d <- ds[["a"]]
ds
ds$d |> class()


library(tibble)
library(dplyr)
df <- tribble(
  ~x, ~y, ~z,
  1,  1,  1,
  1,  2,  3,
  2,  3,  5,
  2,  4,  6,
  3,  5,  7
)
df |> group_by(x) |> summarize(my = mean(y), mz = mean(z))

df %>% group_by(x) %>% summarize(my = mean(y), mz = mean(z))

mtcars |> lm(mpg ~ disp, data = _)
mtcars %>% lm(mpg ~ disp, .)


mtcars |> lm(mpg ~ disp, data = _) |> _$coef
mtcars %>% lm(mpg ~ disp, .) %>% .$coef

x %>% f(y = nrow(.), z = ncol(.))
x %>% {f(y = nrow(.), z = ncol(.))}

# benchmark
library(microbenchmark)
library(dplyr)
library(tibble)

x <- rep(NA_integer_, 1e7)
y <- rep(NA_real_, 1e7)
df <- data.frame(x = x, y = y)
dt <- tibble(x = x, y = y)

performance <- microbenchmark(
  "vektory" = {x[1000] <- 1L; y[1000] <- 1},
  "data.frame 1" = {df[1000, "x"] <- 1L; df[1000, "y"] <- 1},
  "data.frame 2" = {df$x[1000] <- 1L; df$y[1000] <- 1},
  "data.frame 3" = df[1000, ] <- data.frame(x = 1L, y = 1),
  "tibble 1" = {dt[1000, "x"] <- 1L; dt[1000, "y"] <- 1},
  "tibble 2" = {dt$x[1000] <- 1L; dt$y[1000] <- 1},
  "tibble 3" = dt[1000, ] <- tibble(x = 1L, y = 1),
  unit = "ms") |>
  summary() |>
  select(expr, min, mean, median, max)

# Kapitola 5 Faktory
factor(c("žena", "muž", "muž", "žena"))
factor(c("žena", "muž", "muž", "žena"), levels = c("muž", "žena", "jiné"), labels = c("M", "Z", "X"))

factor(c("žena", "muž", "muž", "dieťa", "žena"), levels = c("muž", "žena", "jiné"), labels = c("M", "Z", "X"))

quality <- factor(c("poor", "satisfactory", "excelent"),
                  levels = c("poor", "satisfactory", "excelent"),
                  ordered = TRUE)
quality
