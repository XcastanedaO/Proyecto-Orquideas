############ Lectura originales ########
Datos_2012_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2012_875.xls")
Datos_2013_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2013_875.xls")
Datos_2014_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2014_875.xls")
Datos_2015_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2015_875.xls")
Datos_2016_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2016_875.xlsx")
Datos_2017_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2017_875.xlsx")
Datos_2018_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2018_875.xlsx")
Datos_2019_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2019_875.xlsx")
Datos_2020_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2020_875.xlsx")
Datos_2021_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2021_875.xlsx")
Datos_2022_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2022_875.xlsx")
Datos_2023_875 <- read_excel("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/Datos_2023_875.xlsx")

############ Combinar #########
datos_combinados <- bind_rows(Datos_2017_875,Datos_2018_875,Datos_2019_875,Datos_2020_875,Datos_2021_875)

datos_combinados <- datos_combinados %>% filter(SEXO == "F")

# Supongamos que tienes un data frame llamado 'data'
write_csv(datos_combinados, "C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/w_sivigila_2017_2021.csv")

################ Lectura combinados ############
w_sivigila_2012_2016 <- read_csv("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/w_sivigila_2012_2016.csv")
w_sivigila_2017_2021 <- read_csv("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/w_sivigila_2017_2021.csv")
w_sivigila_2022_2023 <- read_csv("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/w_sivigila_2022_2023.csv")

sivigila_2012_2016 <- read_csv("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/sivigila_2012_2016.csv")
sivigila_2017_2021 <- read_csv("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/sivigila_2017_2021.csv")
sivigila_2022_2023 <- read_csv("C:/Users/sofia/Desktop/Proyecto-Orquideas/data/raw/SIVIGILA/sivigila_2022_2023.csv")

