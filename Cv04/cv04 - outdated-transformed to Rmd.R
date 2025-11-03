# Týden 3: Funkce a iterace nad vektory
# https://is.muni.cz/auth/el/econ/podzim2025/MPE_AVED/um/exercises/ex04.html
rm(list = ls())
###############################################################################
# Na zahřátí
###############################################################################
### UKOL 1 ###
# Vytvořte funkci totez(x), která vrátí svůj argument x beze změny (totéž dělá funkce identity()). Vyzkoušejte na několika vstupech
totez <- function(x) x
  # nemusi byt return, lebo posledny vyhodnoteny vyraz sa vrati ako return. Vlastne sa return pouziva iba uprostred funkcie.
  # totez bez () vypise obsah funkcie. Ale pozor! Ak nahodou existuje premenna totez, tak vypise tu premennu, nie funkciu
  # balik na testovanie funkcii - balik testchat
# test
totez(5)
totez(2:6)
totez(letters)

### UKOL 2 ###
# Vytvořte funkci root(x, n), která spočítá n-tu odmocninu. Pokud nebude n zadáno, bude funkce počítat druhou odmocninu. Funkci vyzkoušejte na několika vstupech.
root <- function(x, n = 2) 
  x^(1/n)

root(64,2)
root(64,3)
root(c(81,64), 2)
root(c(81,64), c(2,3))
root(c(81,64), c(2,3,1))
root(c(81,64, 225), c(2,3))

### UKOL 3 ###
## Vytvořte seznam s, který bude obsahovat vektory (1), (1, 2), (1, 2, 3), (1, 2, 3, 4) a (1, 2, 3, 4, 5). 
s <- list(1, 1:2, 1:3, 1:4, 1:5)

# ina moznost vytvorenia zoznamu:
library(purrr)
s <- map(1:5, function(n) 1:n) 
  # map(zoznam prvkov, funkcia) aplikuj funkciu na kazdy prvok z toho zoznamu
  # ilustracny obrazok: prvky su nieco na dopravnikovom pas, funkcia je to, 4o sa vykona s vecou na pase

# to iste cez map, ale este strucnejsie, vysledok rovnaky:
s <- map(1:5, ~1:.)
  # Vlnka ~ označuje anonymní funkci
  # Tečka . představuje aktuální prvek (nahrazuje x)
  
## Následně vytvořte atomický vektor v, který bude obsahovat průměr prvků seznamu s, tj. v[1] bude mean(s[[1]]) atd.
v <- map(s, mean) # toto vrati zoznam = list, nie atomicky vektor
v[2]
v[[2]]

v <- map_dbl(s, mean) 
  #map_dbl je ina verzia vektora map
  # vrati atomicky = numericky vektor (double)

  # map() → vždy vrací seznam (list)
  # map_dbl() → vrací numerický vektor (double)
  # map_int() → vrací celočíselný vektor (integer)
  # map_chr() → vrací znakový vektor (character)
  # map_lgl() → vrací logický vektor (logical)

### UKOL 4 ###
# Vytvořte funkci Sign(x), která vrátí následující hodnoty:
#   Sign(x) = 1 pre x > 0, 0 pre x = 0, -1 pre x <0
Sign <- function(x) {
  if (x < 0) return (-1)
  if (x == 0) return (0)
  1
}
Sign(-5)
  # OK
Sign(c(2, -7, 11, 0, -5))
  # if v R není vektorizovaný| preto to nefunguje

SignVec <- Vectorize(Sign)
  # vektorizovat funkciu
SignVec(c(2, -7, 11, 0, -5))
  # zavolat tu vektorizovanu funkciu

### UKOL 5 ###
# Nasimulujte seznam, který bude obsahovat 5 vektorů 10 náhodných čísel z rovnoměrného spojitého rozdělení U(0, 1). 
# Funkce runif(n) vygeneruje atomický vektor n takových náhodných čísel.
n <- 10
n_vek <- 5

list5 <- map(1:n_vek, ~runif(n))

###############################################################################
# Hrátky s funkcemi
###############################################################################

### UKOL 1 ###
# Vytvořte funkci vekt(n), která
# - vrátí vektor hodnot 1, 2 ..n , pokud n je jedno celé číslo větší než 0
# - vrátí hodnotu NA, jinak
# Funkci vyzkoušejte na několika vstupech.

vekt <- function(n) {
  if (!is.numeric(n)) return (NA)
  if(length(n) != 1) return (NA)
  if (n <= 0) return (NA)
  if (round(n) != n) return (NA)
  return(1:n)
}

vekt(5) # cele cislo ulozene ako double --> OK
vekt(11L) # cele cislo ulozene ako 
vekt(2:7)
vekt(-3)
vekt(0)
vekt('ahoj')

# jine řešení
vekt <- function(n) {
  if (!(is.numeric(n) && length(n) == 1 && round(n) == n && n > 0))
    return(NA)
  1:n
}
  # dvojity ampersand - pre atomicke vektory a je skracujuce. Ak je jasny vysledok, tak dalej nevydnocuje

### UKOL 2 ###
# Vytvořte funkce nmax(x, n), která vrátí
# - n-tou největší hodnotu ve vektoru x, pokud x je číselný vektor a n je celočíselný skalár větší než 0
# - největší hodnotu ve vektoru x, pokud x je číselný vektor a n nebylo zadáno
# - NA, pokud x je číselný vektor a n celočíselný skalár, ale daný prvek v x není
# - zhavaruje, jinak
# Uvažujte pouze unikátní hodnoty. Pokud vektor x obsahuje dvě stejné hodnoty, eliminujte je pomocí funkce unique().
# Nápověda: může se hodit funkce sort().

## x je číselný vektor a 
x <- c(5, 13, 7)
x <- list(5, 13, 7)
x <- "ano"
is.numeric(x)

## n je celočíselný skalár větší než 0 - podmienky ako vyssie
## if (!is.numeric(n)) return (NA)
## if(length(n) != 1) return (NA)
## if (n <= 0) return (NA)
## if (round(n) != n) return (NA)

max <- function(x, n) {
  if (!is.numeric(x)) return (NA)
  if (!is.numeric(n)) return (NA)
  if(length(n) != 1) return (NA)
  if (n <= 0) return (NA)
  if (round(n) != n) return (NA)
  return ("OK")
}

max(c(5, 13, 7), 3)
max(c(5, 13, 7), "A")
max(list(5, 13, 7), 1)

max <- function(x, n = 1) {
  if (!is.numeric(x)) return (NA)
  if (!is.numeric(n)) return (NA)
  if(length(n) != 1) return (NA)
  if (n <= 0) return (NA)
  if (round(n) != n) return (NA)
  sort(unique(x), decreasing = TRUE)[n]
}
max(c(5, 6, 13, 7), 3) # OK, vrati 6
max(c(5, 13, 7), "A") # not OK
max(list(5, 13, 7), 1) # not OK
max(c(15, 16, 113, 17)) # ok, vrati maximum
max(c(5, 6, 13, 7), 5) # not OK, ale sam ma osetrene, ze index je mimo

# STOPIF
max <- function(x, n = 1) {
  if ((!is.numeric(x)) || !is.numeric(n) || length(n) != 1 || n <= 0 || round(n) != n)
    stop("Bad input")
  sort(unique(x), decreasing = TRUE)[n]
}
max(c(5, 6, 13, 7), 3) # OK, vrati 6
max(c(5, 13, 7), "A") # not OK
max(list(5, 13, 7), 1) # not OK
max(c(15, 16, 113, 17)) # ok, vrati maximum
max(c(5, 6, 13, 7), 5) # not OK, ale sam ma osetrene, ze index je mimo

# STOPIFNOT
max <- function(x, n = 1) {
  stopifnot(is.numeric(x), is.numeric(n), length(n) == 1, n > 0, round(n) == n)
  sort(unique(x), decreasing = TRUE)[n]
}
max(c(5, 6, 13, 7), 3) # OK, vrati 6
max(c(5, 13, 7), "A") # not OK
max(list(5, 13, 7), 1) # not OK
max(c(15, 16, 113, 17)) # ok, vrati maximum
max(c(5, 6, 13, 7), 5) # not OK, ale sam ma osetrene, ze index je mimo


###############################################################################
# Iterace
###############################################################################
rm(list = ls())

### UKOL 1 ###
# Načtěte tabulku mpg z balíku ggplot2 pomocí následujícího výrazu (data se načtou do pracovního prostředí):
#   data("mpg", package = "ggplot2")
# 1. zjistěte, jakých typů jsou jednotlivé sloupce tabulky mpg
# Výsledkem má být v obou případech pojmenovaný atomický vektor.

library(ggplot2)
data("mpg", package = "ggplot2")

class(mpg) # toto mi da class celej tabulky

# potrebujem class pre jednotlive stlpce, tabulka je vektor stlpcov, potrebujem ich class
map(mpg, class) # skoro dobre, len to nie je atomicky vektor

map_chr(mpg, class) # atomicky vektor

# 2. zjistěte, kolik hodnot chybí v jednotlivých proměnných
# Výsledkem má být v obou případech pojmenovaný atomický vektor.

map(mpg, ~sum(is.na(.))) # da vektor stlpcov, v kazdom stlpci spocita pocet NA, cez anonymnu funkciu

map_int(mpg, ~sum(is.na(.))) # ale ako atomicky vektor, nie list 

### UKOL 2 ###
# Otestujte, zda jsou průměry numerických proměnných v tabulce mpg statisticky různé od nuly. 
# K tomu použijte funkci t.test(x), která testuje, zda je numerický vektor x statisticky různý od nuly. 
# Vaším úkolem je použít tuto funkci strojově na všechny numerické sloupce tabulky mpg. Můžete udělat dvě varianty:
 
# 1.  vypíšete celý výsledek testu
t.test(mpg$displ)

map_lgl(mpg, ~is.numeric(.))

map(mpg, ~if (is.numeric(.)) t.test(.) else NA)

  # ina moznost: map_if(vektor, podmienka, vratit, inak)
map_if(mpg, is.numeric, t.test, .else = ~NA) 

# 2. vypíšete pouze p-hodnotu; funkce t.test() vrací objekt třídy htest, který funguje podobně jako seznam; 
# z výsledku každého testu si vezměte pouze proměnnou p.value a poskládejte je za sebe do vektoru
t.test(mpg$displ)
t.test(mpg$displ)$p.value

map(mpg, ~if (is.numeric(.)) t.test(.)$p.value else NA)
map_dbl(mpg, ~if (is.numeric(.)) t.test(.)$p.value else NA)

# alebo
map_if(mpg, is.numeric, ~t.test(.)$p.value, .else = ~NA) # tu som musela dat do t.test aj ~, aj (.)
  # a este atomicky vektor z toho
map_if(mpg, is.numeric, ~t.test(.)$p.value, .else = ~NA) |> map_dbl(1)


###############################################################################
# Das ganze tschechische Volk ist eine Simulantenbande!
###############################################################################
rm(list = ls())

### UKOL 1 ###
# V nejmenovaném předmětu studenti u zkoušky vyplňují test o 50 otázkách. Každá otázka je a/b/c/d test, 
# kde právě jedna odpověď je správná. Studenti mohou test dvakrát opakovat (tj. skládat třikrát). 
# Ke složení testu je třeba získat aspoň 30 správných odpovědí. Zjistěte simulací, jaká je pravděpodobnost, 
# že zkoušku složí student, který na všechny otázky odpovídá zcela náhodně.
# 
# Doporučený postup:
# 1. vytvořte si funkci jeden_test(o) který vrátí počet správných odpovědí, pokud student odpovídá na o otázek; 
# ke generování náhodných odpovědí můžete použít funkci sample(); její parametr prob umožňuje zadat pravděpodobnosti 
# jednotlivých výsledků

  # vygeneruj 50 cisel 0 alebo 1, ak pravdepodobnost nuly je 3/4, pravdepodobnost 1 je 1/4
sample(c(0,1), 50,replace = TRUE, prob = c(3/4, 1/4))
  # pocet spravnych odpovedi je suma tychto nul a jednotiek

jeden_test <- function(questions) {
  stopifnot(is.numeric(questions), 
            length(questions) == 1, 
            questions > 0, 
            round(questions) == questions)
  sum(sample(c(0,1), 
             size = questions,
             replace = TRUE, 
             prob = c(3/4, 1/4)))
}

o <- 50
jeden_test(o)

# 2. vytvořte si funkci testy(o, opak), která nasimuluje opak opakování testu a vrátí maximum bodů, kterých student dosáhl 
# (ke složení zkoušky stačí mít aspoň na jednom pokusu 30 bodů)
opak <- 3
seq_len(opak) # vytvori vektor od 1 po opak, je to spravnejsie ako 1: opak, lebo opak moze byt aj 0..
map(seq_len(opak), ~jeden_test(o)) # zaujímavé, ale bez ~ to nefungovalo
map_int(seq_len(opak), ~jeden_test(o)) # atomicky vektor
map_int(seq_len(opak), ~jeden_test(o)) |> max() # daj mi z neho max
seq_len(opak) |> map_int( ~jeden_test(o)) |> max() # a iny zapis cez pipeline

testy <- function(o, opak) {
  if ((!is.numeric(o)) 
      || !is.numeric(opak) 
      || length(o) != 1 
      || length(opak) != 1 
      || o <= 0 
      || opak <= 0
      || round(o) != o
      || round(opak) != opak)
    stop("Bad input")
  seq_len(opak) |> map_int( ~jeden_test(o)) |> max()  
} 

testy(o, opak)

# 3. nasimulujte 10 000 krát funkci testy() a spočítejte frekvenci, s jakou studenti testy složili
limit <- 30

  # skusme 10 simulacii
map_int(seq_len(10), ~testy(o, opak))
  # ktori mali viac ako 30 bodov
map_int(seq_len(10), ~testy(o, opak)) >= 30
  # keby som chcela pocet, tak suma, ale ja potrebujem frekvenciu, cize priemer
mean(map_int(seq_len(10), ~testy(o, opak)) >= 30)

sim <- 10e4
  # a teraz 1000 krat
mean(map_int(seq_len(sim), ~testy(o, opak)) >= limit)

  # skusme nizsie, hranicu  20 bodov
mean(map_int(seq_len(sim), ~testy(o, opak)) >= 20)

### UKOL 2 ###
# Zobecněte výsledek předchozího výpočtu tak, že spočítáte, jaká je pravděpodobnost, že zkoušku složí student, který zná odpověď na N
# otázek, zatímco na ostatní odpovídá zcela náhodně.

# Doporučený postup:
# 1. upravte funkce vytvořené výše o to, že na některé otázky zná student odpověď

jeden_test <- function(questions, knows = 0) {
  stopifnot(is.numeric(questions), 
            is.numeric(knows),
            length(questions) == 1,
            length(knows) == 1,
            questions > 0, 
            knows >= 0,
            questions >= knows,
            round(questions) == questions,
            round(knows) == knows)
  map_int(knows, ~as.integer(.) + sum(sample(c(0,1), 
                                             size = questions - .,
                                             replace = TRUE,
                                             prob = c(3/4, 1/4))))
}
N <- 5
jeden_test(o, knows = N)

testy <- function(o, opak, knows = 0) {
  stopifnot (is.numeric(o), 
             is.numeric(opak),
             is.numeric(knows),
             length(o) == 1,
             length(opak) == 1, 
             length(knows) == 1,
             o > 0, 
             opak > 0,
             knows >= 0, 
             round(o) == o, 
             round(opak) == opak, 
             round(knows) == knows,
             knows <= o)
  seq_len(opak) |> 
    map_int(~jeden_test(o, knows)) |> max()
} 

testy(o, opak, knows = N)

# 2. z výpočtu pravděpodobnosti udělejte funkci
probab <- function(o, opak, knows, sim) {
  mean(map_int(seq_len(sim), ~testy(o, opak, knows)) >= limit)
}
knows <- 10
limit <- 30

probab(o, opak, knows, sim)   

# 3. tuto funkci iterujte pro znalost od 0 do 50 otázek
probab_vector <- function(o, opak, knows, sim) {
  map_dbl(knows, ~probab(o, opak, ., sim))
}

knows <- c(15,16)
knows <- seq_len(50)

  # pravdepodobnost pro vektor poctu otazek, ktere zna
vector_y <- probab_vector(o, opak, knows, sim) 

  # graf
plot(x = knows, vector_y , type = 'l')

### UKOL 3 ###

dokoncit

#############################################################################
rm(list = ls())
# Kapitola 6 Rídící struktury
# https://aved.econ.muni.cz/control_structures.html
