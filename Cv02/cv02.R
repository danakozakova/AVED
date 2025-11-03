# https://is.muni.cz/auth/el/econ/podzim2025/MPE_AVED/um/exercises/ex02.html
###########################################################################
### Na zahrati ###
###########################################################################
### UKOL 1 ###
# Vytvořte vektory:
#   
#   logický v1 = (pravda, nepravda, pravda),
#   celočíselný v2 = (1, 2, 3) a
#   reálný v3 = (4, 5, 6)
#   Spojte je dohromady: v = (v1, v2, v3).
  
v1 <-  c(TRUE, FALSE, TRUE)
v1 |> str() # logi
v1 |> typeof() # logical

v2 <- 1:3 # integer, sequence of numbers is allways wholenumber
v2 |> str() # int
v2 |> typeof() # integer

# or this way:
v2_a <- c(1L, 2L, 3L) 
v2_a |> str() # int
v2_a |> typeof() # integer

v3 <- c(4, 5, 6) # double, operácia "rovná sa" nie je správna pre double
v3 |> str() # num
v3 |> typeof()
  
# Je vektor v atomický? Pokud ano, jaký má typ?

v <- c(v1, v2, v3)
v # automatic conversion of TRUE/FALSE to value 1/0
v |> str() # num
v |> typeof() # double

# vector is atomic
# vztah typov:
#  logical --> integer --> double --> character

#   Jakou má vektor v délku?
length(v)

### Úkol 2 ###
# do vnutra [] mozno zapisovat
# -- position (positive)
# -- position (negative) - which position do not take
# -- logical vector = condition
# -- name of the element

# Z vektoru v 
## vyhoďte první a poslední prvek
v[2:length(v)-1]
v[-c(1, length(v))]

## vyberte každý 3. prvek
v[seq(from = 3, to = length(v), by =3)]

## vyhoďte všechny nuly
v[v != 0]

### Úkol 3 ###
## Vytvořte vektor x = (1, 1.01, 1.02, …, 4.99, 5) a vektor y = log(x). Vykreslete do grafu pomocí plot(x, y).
x <- seq(from = 1, to = 5, by = 0.01)
# or:
x_a <- (100 : 500)/100

y <- log(x)

plot(x = x, y = y, type = 'l')

### Úkol 4 ###
## Výraz sample(100, 30) vrátí 30 náhodných přirozených čísel z intervalu od 1 do 100 bez opakování.
sample(100, 30)

## Najděte:
numbers <- sample(100, 30)
numbers
  
## nejmenší číslo
min(numbers)

## druhé nejmenší číslo
sort(numbers)[2]
# other way
numbers[numbers != min(numbers)] |> min() # cisla jsou rozne

## druhé největší číslo
sort(numbers)[length(numbers)-1]
# other way
sort(numbers, decreasing = TRUE)[2]

## Nápověda: funkce sort() čísla setřídí.

###############################################################################
# Krásy složeného úročení
###############################################################################
### UKOL 1 ###
# Na účtu máme vklad 1 000 Kč. Úroková sazba je 2 % ročně. Jak se vyvíjí nominální hodnota vkladu v nejbližších 50 letech při složeném úročení? Spočítejte a vykreslete do grafu.
# Nápověda: hodnota vkladu je po letech při úrokové sazbě rovna

rm(list = ls())
deposit <- 1000
r <- 2/100
n <- 50
years <- 1:n

values <- deposit * (1 + r)^years
values

plot(x = n, y = values, type = "l")

### UKOL 2 ###
# Jaká je hodnota vkladu
# po roce
values[1]

# po 30 letech
values[30]

# na konci období
values[length(values)]

# v každém sudé roce
values[seq(from = 2, to = length(values), by = 2)]

values[c(FALSE, TRUE)]

### UKOL 3 ###
# Stejné zadání jako úkol 1, ale úrok je náhodný z U(0,2r), kde r je úroková sazba uvažovaná výše. 
# Nápověda:  Funkce runif(n, min, max) vygeneruje atomický vektor n náhodných čísel z rovnoměrného spojitého rozdělení U(min, max)
# Funkce cumprod(x) spočítá kumulativní násobek prvků vektoru x, tj. pro vektor (1, 2, 3) vrátí vektor (1, 1x2, 1x2x3) = (1, 2, 3).
# Funkce lines(x, y, col) umožní do grafu přidat novou čáru s barvou col (např. “red”).

rm(list = ls())
deposit <- 1000
r <- 2/100
n <- 50
years <- 1:n
u <- runif(n, min = 0, max = 2*r)

values <- deposit * (1 + r)^years
values2 <- deposit * cumprod(1 + u)

plot(x = years, y = values, type = "l")
lines(x = years, y = values2, col = "red")

### UKOL 4 ###
# O kolik procent se liší hodnota vkladu mezi pevným a kolísajícím úrokem? Vykreslete do grafu.
# Domluva: základ pro procenta bude hodnota vkladu při pevném úročení.

diff <- 100* (values2 - values)/values
plot(x = years, y = diff, type = "l")
abline(h = 0, col = "red")

### UKOL 5 ###
# Zjistěte, kdy bude hodnota vkladu (při pevné sazbě) aspoň dvojnásobná, tj. vyšší než 2 000? 
# Kdy se to stane poprvé, tj. jak dlouho trvá, než se vklad zdvojnásobí? Jak dobře funguje “pravidlo 72”?
# Nápověda: Pravidlo 72 říká: “Doba, za kterou se vklad zdvojnásobí, je 72 děleno úroková sazba v procentech.”

print("Ve kterých létach alespoň dvojnásobná:")
years[values >= 2*deposit]

print("Ve kterém roce poprvé dvojnásobná:")
years[values >= 2*deposit][1]

print("pravidlo 72:")
72/(100*r)
years[values >= 2*deposit][1]

### UKOL 6 ###
# Změňte úrokovou sazbu a podívejte se, jak se změní výsledky. 
# (Kód by měl být napsaný tak, aby bylo možné jakoukoli konstantu změnit jen na jednom místě!)

###############################################################################
# Růst hospodářství
###############################################################################
### UKOL 1 ### 
# Růst hospodářství
# Soubor “exdata02.RData” obsahuje dva vektory: hdp_bc je vektor ročních hodnot HDP v mil. Kč v běžných cenách; hdp_sc je vektor ročních hodnot HDP v mil. Kč v cenách roku 2010 za stejné období. Zdroj dat je Eurostat. Vektor cas obsahuje odpovídající roky.
# 
# Data načte pomocí funkce load("exdata02.RData").

rm(list = ls())
load("exdata02.RData")

# Úkol 1
# Spočítejte hodnotu deflátoru cen a vykreslete jej do grafu.
# 
# Nápověda: Deflátor P_t = Y_t/y_t * 100, 
# Y_t = bezne ceny, y_t = stale ceny
# , kde 
# je HDP v běžných cenách a 
# je HDP ve stálých cenách. (V základním roce 2010 by měla být hodnota deflátoru 100.)
deflator <- 100 * hdp_bc / hdp_sc
plot(x = cas, y = deflator, type = 'l')
abline(h = 100, lty = 3)
abline(v = 2010, lty = 3)


# Úkol 2
# Spočítejte roční tempa růstu nominálního HDP, reálného HDP a roční míru inflace a vykreslete je do grafu.
# Nápověda: růst i inflace jsou tempa růstu veličiny. Tempo růstu veličiny y_t v čase t
# spočítáme jako (y_t/y_t-1) - 1 a můžeme je případně vynásobit 100. (BTW, proč je v grafu vynechán první rok?)

print("Realne HDP:")
real <- hdp_sc[-1]/hdp_sc[-length(hdp_sc)] - 1
real <- c(NA, real)

print("Nominální HDP:")
nominal <- hdp_bc[-1]/hdp_bc[-length(hdp_bc)] - 1
nominal <- c(NA, nominal)

print("Inflace:")
inflation <- deflator[-1]/deflator[-length(deflator)] - 1
inflation <- c(NA, inflation)

plot(x = cas, y = real, type = "l", 
     ylim = range(c(real, nominal, inflation), na.rm = TRUE))
lines(x = cas, y = nominal, col = "blue")
lines(x = cas, y = inflation, col = "red")
legend(2013, 0.15, 
       col = c("black", "blue", "red"),
       legend = c("real", "nominal", "inflation"),
       lty = 1)

# Úkol 3
# Spočítejte korelaci inflace a růstu reálného HDP v čase pomocí funkce cor(); 
# chybějících hodnot se zbavíte parametrem use = "complete.obs".
print("Korelace")
cor(inflation, real, use = "complete")

###############################################################################
# Elasticita
###############################################################################

# Máme dva vektory: vektor cen p = (100, 90, 80, 70, 60, 50) Kč a vektor prodaných množství 
# q = (30, 60, 90, 120, 200, 210) tisíc kusů.

### UKOL 1 ### 
# Spočítejte tržby při každé ceně a vykreslete je do grafu. (Tržby jsou pi x qi, kde pi je cena a qi je odpovídající množství.)

rm(list = ls())
price <- c(100, 90, 80, 70, 60, 50)
quantity <- c(30, 60, 90, 120, 200, 210)

revenues <- price * quantity
plot(x = price, y = revenues)

### Úkol 2 ###
# Spočítejte
# - cenovou elasticitu mezi nejvyšší a nejnižší cenou
# Nápověda: empirická cenová elasticita se spočítá jako (rozdil mnozstvi deleno stredne mnozstvi) / (rozdil ceny deleno stredni cena)
last_index <- length(quantity)
elasticity_last <- ((quantity[last_index] - quantity[1])/(quantity[last_index] + quantity[1]))/
  ((price[last_index] - price[1])/(price[last_index] + price[1]))

print("Elasticity between the lowest and the highest price:")
print(elasticity_last)

# - cenovou elasticitu mezi každými dvěma sousedními cenami

elasticity <- ((quantity[-1] - quantity[-last_index])/(quantity[-1] + quantity[-last_index]))/
  ((price[-1] - price[-last_index])/(price[-1] + price[-last_index]))
print("Continual elasticity:")
print(elasticity)

plot(x = (price[-1] + price[-last_index]), y = elasticity)
abline(h = -1, lty = 3)

################################################################################
# učebné materiály
################################################################################
# Kapitola 2
##############
x <- c(id = 1, meno = "Fero", vek = 12)
x

x |> attributes()
attr(x, "names")
attr(x, "names") <- c("id", "surname", "vek")
attr(x, "names")
x
attr(x, "names") <- NULL
attr(x, "names")
x

###
X <- matrix(1:12, nrow = 3)  # matice má počet řádků a sloupců
X

attributes(X)
attr(X, "dim")

attr(X, "dim") <- NULL
X
typeof(X)
str(X)

# Kapitola 3
###############
x4 <- 'Josef řekl: "Miluji R!"'  # řetězec
x4
print(x4)
cat(x4)

format(2.7e-5, scientific = FALSE)
sqrt(2) ^ 2 - 2  # odmocnina 2 umocněná na druhou minus 2

is.infinite(Inf)
is.infinite(-Inf)
is.infinite(NA)
is.infinite(NaN)
is.infinite(8)


is.na(NA)
is.na(NaN)

is.nan(NA)
is.nan(NaN)
# NaN ==> NA

as.logical(c(-1, 0, 0.1, 1, 2, 5))
x <- c(1, 2, 3, 7, 19, 31)  # vytvoří vektor daných čísel
# kolik hodnot x je větší než 10?
sum(x > 10)

x <- "ahoj"
as.numeric(x)

# Zvysok po deleni
31 %% 7
# Celociselne delenie
31 %/% 7


x <- 12L / 3L
x
typeof(x)

## porovnanie
x1 <- 0.5 - 0.3
x2 <- 0.3 - 0.1
x1 == x2                           # na většině počítačů FALSE
x1 - x2

all.equal(x1, x2)

all.equal(x1, 7)

## logicke vektory
c(TRUE, FALSE) && c(TRUE, TRUE)
c(TRUE, FALSE) & c(TRUE, TRUE)

all(c(TRUE, FALSE, FALSE))
any(c(TRUE, FALSE, FALSE))


p <- 1:-1
is.numeric(p) && p > 0
is.numeric(p)
p > 0
all(p>0)
is.numeric(p) && all(p > 0)

# Základní datové struktury
x <- c(a = 1, "good parameter" = 7, c = 17)
x

attr(x, "names") <- c("A", "Good Parameter", "C")
x

names(x) <- c("aa", "bb", "cc")
x

setNames(x, c("A", "B", "C"))

z <- numeric(0)  # parametr je délka vektoru
z
1:10   # vektor celých čísel 1 až 10
10:1   # vektor celých čísel sestupně 10 až 1
# sekvence od ... do
seq(from = 1, to = 10, by = 3)  # s daným krokem

seq(from = 1, to = 10, length.out = 4)  # s danou délkou výsledku
seq(from = 1, to = 12, length.out = 4)  # s danou délkou výsledku

seq_along(c(1, 3, 17, 135))  # celá čísla od 1 do délky zadaného vektoru

seq_len(17)  # celá čísla od 1 nahoru se zadanou nezápornou délkou

# opakování hodnot ve vektoru
rep(c(1, 3), times = 5)  # celý vektor 5 krát
rep(c(1, 3), each = 5)  # každý prvek 3 krát

rep_len(c(1, 3), length.out = 13)  # celý vektor do délky 5

# alokace pameti
x <- numeric(1e6)  # alokace prázdného vektoru o milonu prvků
x[1] <- 1          # přidání prvků (další řádky vynechány)
n <- 7654          # skutečný počet vložených prvků
x <- x[1:n]        # zkrácení vektoru na potřebnou délku

rep(NA_real_, 1e6)
