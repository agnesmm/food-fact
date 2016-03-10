library(ggplot2)
library(gridExtra)
library(dplyr)



##load data (url juste pour info)
url_data_food_facts <- "https://www.kaggle.com/openfoodfacts/world-food-facts"

path_data_food_facts <- "/Users/agnesmustar/0R/world-food-facts/FoodFacts.csv"
data_food_facts <- read.csv(path_data_food_facts, na.strings = c("NA",""," "))

## product sold in France 

library(dplyr)
tbl_food_france <- 
        data_food_facts%>%
        tbl_df()%>%
        filter(grepl("France", data_food_facts$countries_en) == TRUE)

## on retire les colonnes avec + 50% de NA's
tbl_food_france <- tbl_food_france[, colMeans(is.na(tbl_food_france)) < 0.5]
summary(colMeans(!is.na(tbl_food_france))*100)


## hist des NA's 
par(mar = c(18,4,2,2))
tbl_food_france %>%
        is.na()%>%
        colSums()%>%
        barplot(las=2)

## creation de nutrition avec moins de colonnes que tbl_food_france
nutrition <- tbl_food_france %>%
        select(8, 10, contains("100g"), contains("oil"), contains("nutrition"))

## comparaison nutrition_score en fr et uk
## alure un peu différente, moyenne et mediane proche
i <- qplot(nutrition_score_fr_100g, data = nutrition) + 
        geom_vline(xintercept = mean(nutrition$nutrition_score_fr_100g, na.rm = T), 
                   color = "red", size = 1) +
        geom_vline(xintercept = median(nutrition$nutrition_score_fr_100g, na.rm = T), 
                   color = "blue", size = 1)

j <- qplot(nutrition_score_uk_100g, data = nutrition) + 
        geom_vline(xintercept = mean(nutrition$nutrition_score_uk_100g, na.rm = T), 
                   color = "red", size = 1) +
        geom_vline(xintercept = median(nutrition$nutrition_score_fr_100g, na.rm = T), 
                   color = "blue", size = 1)

grid.arrange(i,j, nrow = 2)

## pk des valeurs negatives 
## A - Vert : de -15 à -2
## B - Jaune : de -1 à 3
## C - Orange : de 4 à 11
## D - Rose : de 12 à 16
## E - Rouge : 17 à 40

## dans un premier temps on ne prend pas en compte les NAs 
nutrition <- nutrition %>%
        filter(is.na(nutrition_grade_fr) == FALSE)

## deux plot comme ca 
categorie <- qplot(nutrition_grade_fr, data = nutrition)
with(nutrition, plot(nutrition_grade_fr, sugars_100g))

# names(nutrition)
# [1] "product_name"                            "quantity"                               
# [3] "energy_100g"                             "fat_100g"                               
# [5] "saturated_fat_100g"                      "carbohydrates_100g"                     
# [7] "sugars_100g"                             "proteins_100g"                          
# [9] "salt_100g"                               "sodium_100g"                            
# [11] "nutrition_score_fr_100g"                 "nutrition_score_uk_100g"                
# [13] "ingredients_from_palm_oil_n"             "ingredients_that_may_be_from_palm_oil_n"
# [15] "nutrition_grade_fr" 
## est ce que les 5 categories sont bien dans les bonnes prop
## semble plutot pas mal (avec mise echelle y de 0 à 100)
qplot(nutrition$nutrition_grade_fr)
nutrition%>%
        group_by(nutrition_grade_fr)%>%
        summarise(prop = n()/ nrow(nutrition)*100)%>%
        ggplot(.,aes(x =nutrition_grade_fr, y = prop)) + 
        geom_point() + geom_hline(yintercept = 20) +
        ylim(c(0,100))




# fruits legumes et noix
# fibres 
# protéines 
#pareil le faire aves de bars 
bar("proteins_100g")




##carbohydrates
nutrition %>%
        group_by(nutrition_grade_fr) %>%
        summarise(cmean = mean(carbohydrates_100g, na.rm = T)) %>%
        ggplot(aes(nutrition_grade_fr, cmean)) + 
        geom_bar(stat = "identity", width = 0.5, fill = "#607D8B") +
        theme_light()


## on refait une fonction comme dessiner mais avec des bar
## cette fois le df et nutrition_grade ne sont plus dans les arguments 

bar <- function(ob){
        nutrition %>%
                group_by_(~nutrition_grade_fr)%>%
                summarise_(., Mean = interp(~mean(ob, na.rm = T), ob = as.name(ob))) %>%
                ggplot(aes(nutrition_grade_fr, Mean)) + 
                geom_bar(stat = "identity", width = 0.5, fill = "#607D8B") +
                ggtitle(as.name(ob)) +
                xlab("Catégorie") +
                theme_light() +
                theme(plot.title = element_text(size = 18, color = "black"),
                      axis.text = element_text(size = 12, color = "#607D8B"),
                      axis.title = element_text(size = 15, color = "#607D8B"))
}

grid.arrange(bar("sodium_100g"), bar("saturated_fat_100g"), bar("energy_100g"), bar("sugars_100g"))


## différence entre salt et sodium = non juste un coeff 2.5
## manque sel ? oit etre un gplot ou un qplot 
sel <- ggplot(nutrition, aes(salt_100g, sodium_100g))
sel + geom_point(size = 1.5 ) + 
        geom_smooth(method = "lm", se = FALSE, color = "#E91E63") + 
        theme_light() 

test <- sodium_100g*2.5
ggplot(nutrition, aes(salt_100g, sodium_100g*2.5))
plot(nutrition$salt_100g, nutrition$sodium_100g *2.5) + abline(a = 1, b = 1, col = 2)


bar("fat_100g")
bar("carbohydrates_100g")

## proteine : equivalent pour chaque categorie
bar("proteins_100g")
summary(nutrition$proteins_100g)

nutrition %>%
        group_by(nutrition_grade_fr) %>%
        summarise(mean(proteins_100g, na.rm = T), median(proteins_100g, na.rm = T), 
                  max(proteins_100g, na.rm = T), min(proteins_100g, na.rm = T))


## on sinteresse a l'huile de palme
## comme l'huile 0 ou 1 quand on fait la moyenne et qu'on multiplie par 100 cela donne la proportion 
## attention soucis : comme c'est le nombre d'ingredients contenant palm oil parfois == 2, 
## on devrait dire que ceux qui sont égal a deux sont égal a 1 ?
nutrition%>%
        group_by(nutrition_grade_fr)%>%
        summarise(prop = mean(ingredients_from_palm_oil_n, na.rm = T)*100) %>%
        ggplot(aes(nutrition_grade_fr, prop)) + 
        geom_bar(stat="identity", fill = "#607D8B", width = 0.7) +
        ggtitle("Proportion de produits contenant de l'huile de palme") +
        xlab("Catégorie du produit") +
        ylab("Pourcentage") +
        theme_light() +
        theme(plot.title = element_text(size = 17, vjust = 2, color = "#607D8B"),
              axis.text = element_text(size = 13, color = "black"),
              axis.title = element_text(size = 15, color = "#607D8B"))

## plus sur l'huile de palme 
nutrition %>%
        group_by(nutrition_grade_fr) %>%
        summarise(mean(ingredients_from_palm_oil_n, na.rm = T), median(ingredients_from_palm_oil_n, na.rm = T), 
                  max(ingredients_from_palm_oil_n, na.rm = T), min(ingredients_from_palm_oil_n, na.rm = T))

egal2 <- nutrition %>%
        filter(ingredients_from_palm_oil_n > 1)
dim(egal2)
##18 lignes (en fait c'est egal a un ou 2 jamais plus (: ))

## peut etre 
nutrition %>%
        group_by(nutrition_grade_fr) %>%
        summarise(mean(ingredients_that_may_be_from_palm_oil_n, na.rm = T), 
                  median(ingredients_that_may_be_from_palm_oil_n, na.rm = T), 
                  max(ingredients_that_may_be_from_palm_oil_n, na.rm = T), 
                  min(ingredients_that_may_be_from_palm_oil_n, na.rm = T))
peut2 <- nutrition %>%
        filter(ingredients_that_may_be_from_palm_oil_n  > 1)
head(peut2)
dim(peut2)
## dim = 748 lignes bcp de produits dont on ne sait pas s'ils contiennent de l'huile de p 

## on crée une colonne, le produit contient-il de l'hdp? (0 = non, 1 = oui, NA = NA)

nutrition$palm_oil <- nutrition$ingredients_from_palm_oil_n
nutrition$palm_oil[nutrition$palm_oil > 0] <- 1 



## on crée une colonne, le produit contient il probablement de l'huile de palme 
## 1 = il y a peut etre de l'huile de palme 0 = pas d'hdp
nutrition$may_be_palm_oil <- nutrition$ingredients_that_may_be_from_palm_oil_n
nutrition$may_be_palm_oil[nutrition$may_be_palm_oil > 0] <- 1 

## pas bon on veut 1 si on est sur qu'il y a du l'huile de p 
a <- nutrition %>%
        filter(palm_oil == 1, may_be_palm_oil == 0)
dim(a)
#[1] 1244   17
## donc pour tous les palm_oil = 1 on met may_be a 1
nutrition[,"may_be_palm_oil"][nutrition[,"palm_oil"] == "1"] <- 1



# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.0000  0.1428  0.0000  1.0000    1222 

g <- ggplot(nutrition, aes(palm_oil, fill = palm_oil)) 
g + geom_bar(width = 0.8) +
        theme(legend.position = "bottom", plot.title = element_text(size = 17, color = "#607D8B")) +
        ggtitle("Proportion d'aliments contenant de l'huile de palme") +
        xlab(NULL) 




# "quantity" = variables trop diff pour le moment (caractere)
# "carbohydrates_100g" 
# "nutrition_score_fr_100g"
# proteins_100g
# "fat_100g"
# salt_100g
# ingredients_that_may_be_from_palm_oil_n
# energie satured fat sugars sodium



# fill = "white", color = "#EC407A"
# p
# p + theme_gray()
# p + theme_bw()
# p + theme_linedraw()
# p + theme_light()
# p + theme_minimal()
# p + theme_classic()