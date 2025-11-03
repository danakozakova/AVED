# Týden 5: Načítání dat do R
# https://is.muni.cz/auth/el/econ/podzim2025/MPE_AVED/um/exercises/ex05.html
rm(list = ls())
###############################################################################
# Na zahřátí
###############################################################################
### UKOL 1 ###
# Načtěte z balíku ggplot2 tabulku txhousing a vypište několik prvních jeho řádků. Jaký typ (třídu) má tato tabulka?

library(ggplot2)
txhousing # lazy load
  # alebo 
ggplot2::txhousing
  ## alebo ;plne inak: natiahnut do environment, moze prepisat moju premennu v environment
data("txhousing", package = "ggplot2")

txhousing |> class()
txhousing |> head()

###############################################################################
# Gross domestic product at market prices
###############################################################################
# Začneme s daty z Eurostatu s kódem tec00001. 
# Data najdete na adrese http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&language=en&pcode=tec00001&plugin=1

### UKOL 1 ###
# Načtěte data z binárního R formátu. Soubor se jmenuje “tec00001.RData”. Obsahuje tabulku tec00001.
load('tec00001.RData')  
   # to su tidy data, klucom su prve dva stlpce

### UKOL 2 ###
# Načtěte stejnou tabulku ze souboru “tec00001.csv”. Tabulku pojmenujte tec00001csv.
library(readr)
tec00001 <- read_csv("tec00001.csv", 
                     col_types = cols(
                       na_item = col_character(),
                       unit = col_character(),
                       geo = col_character(),
                       time = col_date(format = "%Y-%m-%d"),
                       values = col_double()
                      ))
tec00001

### UKOL 3 ### Subor TSV - data oddelene tabulatorom
# Načtěte stejnou tabulku ze souboru “tec00001.tsv.gz”. Tabulku pojmenujte tec00001tsv. 
# Jak se tato tabulka liší od tabulky tec00001csv? (Tento soubor je přímo stažený z Eurostatu.)

# ZLE  ## toto načíta defaultne, ale zle
tec00001tsv <- read_csv("tec00001.tsv.gz")  
tec00001tsv

# LEPSIE  ## toto mi správne načíta
tec00001tsv <- read_tsv(
  "tec00001.tsv.gz",
  col_types = "cnnnnnnnnnnnn",
  na = ":"
)
tec00001tsv

# NAKLIKANE CEZ dialog okno
tec00001tsv <- read_delim("tec00001.tsv.gz", 
                           delim = "\t", escape_double = FALSE, 
                           col_types = cols(`2007` = col_number()), 
                           trim_ws = TRUE)
problems(tec00001_tsv) # pozri problemy

tec00001tsv <- read_tsv("tec00001.tsv.gz",
                        col_types = cols(.default = col_character()),
                        na = ':') # nedefinovana hodnota je :

# DALSIE RIESENIE - defaultne su to cisla, len prvy stlpec je text --> stratime poznamky pri cislach
tec00001tsv <- read_tsv("exdata05/tec00001.tsv.gz",
                        col_types = cols(.default = col_number(),
                                         `na_item,unit,geo\\time` =
                                           col_character()),
                        na = ":")
tec00001tsv

#   - nejlepší může být načíst všechny sloupce jako řetězce a pak si poznámky i
#     čísla z dat vytáhnout ručně
#     (můj kód používá balík tidyr a regulární výrazy, které se naučíte používat 
#      v příštích týdnech)
tec00001tsv <- read_tsv("tec00001.tsv.gz",
                        col_types = cols(.default = col_character()),
                        na = ":") |>
  tidyr::separate_wider_regex(
    -1,
    patterns = c(year = "[^\\s]*", "\\s+", note = ".*"),
    names_sep = "_",
    too_few = "align_start")
tec00001tsv


  ### UKOL 4 ###
# Načtěte stejnou tabulku ze souboru “Eurostat_Table_tec00001FlagDesc_11c465e3-eae1-4c8e-b956-f473d4a78a0a.xls”. 
# Tabulku pojmenujte tec00001xls. Jak se tabulka tec00001xls liší od tabulek tec00001csv a tec00001tsv? 
#   (Tento soubor je přímo stažený z Eurostatu.)

library(readxl)
filename <- "Eurostat_Table_tec00001FlagDesc_11c465e3-eae1-4c8e-b956-f473d4a78a0a.xls"
tec00001xls <- read_excel(filename,
                          range = "A4:Y47", na = ":", 
                          col_types = c("text", rep(c("numeric", "skip"), 12)))


# načtení dat z excelu, dobře formátované, ne tidy data
library(readxl)
# ztratíme poznámky
tec00001xls <- read_excel("exdata05/Eurostat_Table_tec00001FlagDesc_11c465e3-eae1-4c8e-b956-f473d4a78a0a.xls",
                          range = "A4:Y47", na = ":",
                          col_types = c("text", rep(c("numeric", "skip"), 12))) # text, rok(cislo), vyhod, rok(cislo), vyhod, ....
tec00001xls

# to iste, ale zachovame poznámky
# len zmeneny skip na text
tec00001xls <- read_excel("exdata05/Eurostat_Table_tec00001FlagDesc_11c465e3-eae1-4c8e-b956-f473d4a78a0a.xls",
                          range = "A4:Y47", na = ":",
                          col_types = c("text", rep(c("numeric", "text"), 12))) # text, rok(cislo), vyhod, rok(cislo), vyhod, ....
tec00001xls

### Poznámka 1 ###
# nacitanie priamo z Eurostat
  # install.packages("eurostat")
tec00001 <- eurostat::get_eurostat("tec00001")
tec00001


#############################################################################
 # Murphy: Co se může pokazit…
#############################################################################
### UKOL 1 ###
filename <- "experiment.csv"
read_lines(filename)

read_lines(filename) |> head(15) |> cat(sep = "\n")

### UKOL 2 ###
# Načtěte data ze souboru “experiment.csv” do proměnné experiment tak, 
# aby proměnná treatment byla faktor s úrovněmi “control”, “group A” a “group B”.

library(readr)
experiment <- read_delim("experiment.csv", 
                         delim = ":", 
                         escape_double = FALSE, 
                         col_types = cols(id = col_integer(), 
                                          height = col_integer(), weight = col_integer(), 
                                          treatment = col_factor(levels = c("control", "group A", "group B"))), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE, 
                         skip = 8)
experiment

### UKOL 3 ###
# Načtěte data ze souboru “experiment2.csv” do proměnné experiment2.
library(readr)
experiment2 <- read_delim("experiment2.csv", 
                          delim = ":", escape_double = FALSE, col_names = FALSE, 
                          col_types = cols(`1` = col_integer(), 
                                           X1 = col_integer(), X5 = col_factor(levels = c("control", 
                                                                                          "group A", "group B"))), 
                          locale = locale(decimal_mark = ","))
colnames(experiment2) <- c("id", "name", "height", "weight",
                           "treatment", "value")
experiment2

# ALEBO v JEDNOM
experiment2 <- read_delim("experiment2.csv", 
                          delim = ":", escape_double = FALSE, 
                          col_names = c("id", "name", "height", "weight", "treatment", "value"),
                          col_types = cols(id = col_integer(),
                                           name = col_character(),
                                           height = col_integer(), 
                                           weight = col_integer(),
                                           treatment = col_factor(levels = c("control", 
                                                                             "group A", 
                                                                             "group B")),
                                           value = col_double()),
                          locale = locale(decimal_mark = ","))
experiment2

DOKONCIT
#############################################################################
rm(list = ls())
# Kapitola 10  Načítání a ukládání dat ze souborů
# https://aved.econ.muni.cz/reading_and_writing_data_from_files.html

# Funkcie read_XXX z balika readr vracaju tabulku triedy tibble
# Zakladna funkcia je read_delim
