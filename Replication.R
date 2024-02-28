#+ Codebook Replication Script
#+ 27 Feb 2024

# Load Packages ----------------------
library(tidyverse)
library(countrycode)
# install.packages('countrycode')

# Load Data ----------------------
df = read_csv('WB_WDI_download.csv', na = "..") # replace missing values with NA 
df_health = read_csv('healthcare_index_OWID.csv')

## Rename DF Variables
df2 = # there is probably a more efficiently method to do this step
  df |>
  rename(
    Country = `Country Name`, vName = `Series Name`, vCode = `Series Code`,
    '1960' = '1960 [YR1960]', '1961' = '1961 [YR1961]', '1962' = '1962 [YR1962]', '1963' = '1963 [YR1963]', '1964' = '1964 [YR1964]',
    '1965' = '1965 [YR1965]', '1966' = '1966 [YR1966]', '1967' = '1967 [YR1967]', '1968' = '1968 [YR1968]', '1969' = '1969 [YR1969]',
    '1970' = '1970 [YR1970]', '1971' = '1971 [YR1971]', '1972' = '1972 [YR1972]', '1973' = '1973 [YR1973]', '1974' = '1974 [YR1974]',
    '1975' = '1975 [YR1975]', '1976' = '1976 [YR1976]', '1977' = '1977 [YR1977]', '1978' = '1978 [YR1978]', '1979' = '1979 [YR1979]',
    '1980' = '1980 [YR1980]', '1981' = '1981 [YR1981]', '1982' = '1982 [YR1982]', '1983' = '1983 [YR1983]', '1984' = '1984 [YR1984]',
    '1985' = '1985 [YR1985]', '1986' = '1986 [YR1986]', '1987' = '1987 [YR1987]', '1988' = '1988 [YR1988]', '1989' = '1989 [YR1989]',
    '1990' = '1990 [YR1990]', '1991' = '1991 [YR1991]', '1992' = '1992 [YR1992]', '1993' = '1993 [YR1993]', '1994' = '1994 [YR1994]',
    '1995' = '1995 [YR1995]', '1996' = '1996 [YR1996]', '1997' = '1997 [YR1997]', '1998' = '1998 [YR1998]', '1999' = '1999 [YR1999]',
    '2000' = '2000 [YR2000]', '2001' = '2001 [YR2001]', '2002' = '2002 [YR2002]', '2003' = '2003 [YR2003]', '2004' = '2004 [YR2004]',
    '2005' = '2005 [YR2005]', '2006' = '2006 [YR2006]', '2007' = '2007 [YR2007]', '2008' = '2008 [YR2008]', '2009' = '2009 [YR2009]',
    '2010' = '2010 [YR2010]', '2011' = '2011 [YR2011]', '2012' = '2012 [YR2012]', '2013' = '2013 [YR2013]', '2014' = '2014 [YR2014]',
    '2015' = '2015 [YR2015]', '2016' = '2016 [YR2016]', '2017' = '2017 [YR2017]', '2018' = '2018 [YR2018]', '2019' = '2019 [YR2019]',
    '2020' = '2020 [YR2020]', '2021' = '2021 [YR2021]', '2022' = '2022 [YR2022]'
  )

# Create Lengthening Pivot ------------------------------

## Test lengthening pivot with first row of data
arg = 
  df2 |>
  filter(
    Country == 'Argentina',
    vCode == 'SP.POP.TOTL'
  )

## Lengthening pivot
arg |> 
  pivot_longer(
    cols = 5:67,
    names_to = 'year',
    names_transform = list(year = as.integer),
    values_to = 'place holder'
  )

# Create Widening Pivot ----------------------

## Test widening pivot with first column of data
y1960 = 
  df2 |>
  select(Country:'1960', -vName)

y1960 |> # widening pivot
  group_by(Country) |>
  pivot_wider(
    names_from = vCode,
    values_from = '1960'
  )

# Lengthen Data ------------------------
df2_long =
  df2 |>
  select(-vName) |>
  pivot_longer(
    cols = 4:66,
    names_to = 'year',
    values_to = 'place holder' 
  )

# Widen Data ----------------------
df3 = 
  df2_long |>
  pivot_wider(
    names_from = vCode,
    values_from = 'place holder'
  )   

# Tidy Data

## Rename, reorder, recode
df4 =
  df3 |>
  rename( # Rename World Bank variables
    Country_Code = `Country Code`,
    Year = year,
    Pop = 'SP.POP.TOTL',
    GDP = 'NY.GDP.MKTP.PP.CD',
    Pov_Ratio = 'SI.POV.DDAY',
    GINI_Num = 'SI.POV.GINI',
    Int_Aid = 'DT.ODA.ALLD.CD',
    Edu_Attain = 'SE.PRM.CUAT.ZS',
    Water_Acc = 'SH.H2O.BASW.ZS',
    San_Acc = 'SH.STA.BASS.ZS',
    Mat_Mort = 'SH.STA.MMRT',
    Child_Mort = 'SH.DYN.MORT'
  ) |>
  mutate(
    Edu_Attain = Edu_Attain / 100, # Convert to percentage
    Water_Acc = Water_Acc / 100, # Convert to percentage
    San_Acc = San_Acc / 100, # Convert to percentage
    ID = row_number(), .before = 1 # Create ID column
  ) |>
  mutate(
    GDPC = GDP / Pop, .before = 7 # Create GDPC variable
  ) |>
  mutate(
    Year = as.numeric(Year) # Convert Year variable from character to numeric
  )

## Create Continent variable
df4$Continent <- countrycode( # Add continent variable from country code
  sourcevar = df4$Country, # Code Source: https://infoart.medium.com/getting-continents-from-country-names-in-r-4d07a98f84ee
  origin = "country.name", # Requires 'countrycode' R package
  destination = "continent"
  )

df5 = # Add continent values for "countries" not included in 'countrycode' package 
  df4 |>
  mutate(Continent = if_else(Country == 'Channel Islands'| Country == 'Kosovo', 'Europe', Continent, missing = NULL), .before = 4
  )

# unique(df5$Continent)

df5 = # reorder Continent variable
  df5[, c(1:3,16,4:15)]

## Convert Continent variable to factor variable
df6 =
  df5 |>
    mutate(Continent = as.factor(Continent))

## Convert GINI_Num variable to create factor variable
FctWhen = function(...) {
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when( !!!args )
  exec(fct_relevel, cases, !!!rhs)
} 

df7 =
  df6 |>
  mutate(
    GINI = FctWhen(
      GINI_Num <= 30 ~ "High Equality", # criterion ~ output if met
      GINI_Num %in% 30.1:40 ~ "Middle Equality",
      GINI_Num >= 40.1 ~ "Low Equality"
    ), .before = 10
  ) |>
  select(-GINI_Num)

## Rename healthcare index variables and merge with World Bank data
df_health =
  df_health |>
  rename(
    Country = Entity,
    Country_Code = Code,
    HAQ = `HAQ Index (IHME (2017))`
  )

df8 =
  left_join(
    df7, 
    df_health,
    join_by(Country_Code, Year)
  ) |>
  rename(
    Country = Country.x
  ) |>
  select(-Country.y)

## Export dataset
save(df8, file = 'Codebook_Dataset.RData') # Copied this code from R survival guide, don't know why it's not working...
write_csv(df8, file = 'Codebook_Dataset.csv')


# Produce Codebook tables -------------------------------

## Factor variable tables
cbfactor = function(.data, x){
  x = enquo(x)
  
  count(.data, !!x) |>
    mutate(
      Value = row_number(),
      Label = as_factor(!!x),
      Freq = n,
      .keep = 'none'
    ) |>
    knitr::kable()
}

cbfactor(df8, Continent)
cbfactor(df8, GINI)

## Numeric variable tables
cbnumberic = function(.data, x){
  x = enquo(x)
  
  count(.data, !!x) |>
    summarise(
      min = min(!!x, na.rm = T),
      mean = mean(!!x, na.rm = T),
      median = median(!!x, na.rm = T),
      max = max(!!x, na.rm = T)
    ) |>
    knitr::kable(digits = 1L)
}

cbnumberic(df8, Pop)
cbnumberic(df8, GDP)
cbnumberic(df8, GDPC)
cbnumberic(df8, Pov_Ratio)
cbnumberic(df8, Int_Aid)
cbnumberic(df8, Edu_Attain)
cbnumberic(df8, Water_Acc)
cbnumberic(df8, San_Acc)
cbnumberic(df8, Mat_Mort)
cbnumberic(df8, Child_Mort)
cbnumberic(df8, HAQ)
