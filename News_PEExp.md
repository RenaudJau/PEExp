---
title: "Expé"
output: 
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

# Quels éléments favorisent l’occurrence d’effets de priorité lors de la restauration par semis d’espèces ?

Voici une page qui donne quelques nouvelles de l'expérimentation PEExp présentée dans le [poster](https://github.com/RenaudJau/PEExp/blob/main/Poster_ECOVEG_PEExp.pdf) présenté à [ECOVEG](https://www.gembloux.ulg.ac.be/ecoveg15/).

### Historique *in situ*

La contingence historique peut modifier la structure d’une communauté par effet de priorité. Il s’agit du fait qu’une espèce exerce un effet compétitif/facilitant causé par son arrivée précoce dans la communauté. Cet effet pourrait être utilisé en restauration : par exemple semer deux espèces dans un ordre déterminé pour en favoriser une. Mais dans quelles conditions ces effets de priorité s’exercent-t-ils ?

Dans le cadre d'un projet de restauration écologique de pelouses sèches en Haute-Durance (cf [thèse d'Aure Durbecq](https://tel.archives-ouvertes.fr/tel-03638021), nous avions déjà mis en place une expérimentation *in situ* en testant l'ordre d'arrivée de deux groupes d'espèces : un groupe d'espèces subordonnées, un groupe d'espèces dominantes.

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/reference.jpg)

Les espèces du groupe des subordonnées :

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/subordonnees.png)

Les espèces du groupe des dominantes :

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/dominantes.png)

Nous avions 4 modalités qui consistaient à semer :

* toutes les espèces en même temps la première année

* aucune espèce

* les dominantes l'année 1 et les subordonnées l'année 2

* les subordonnées l'année 1 et les dominantes l'année 2

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/plan_exp.jpg)

Avec comme hypothèse générale que le fait de semer les subordonnées en premier permettrait de faire coexister l'ensemble des espèces. Derrière cette hypothèse générale, nous souhaitions comprendre plus en détails les éléments qui déterminaient l'occurence ou non des effets de priorité. Pour cela il fallait multiplier les modalités de semis : espèces seules ou en interaction, semis simultané ou en décalé, conditions écologiques... Même en réduisant les répétitions, faire cette expérimentation *in situ* aurait néccessité une surface (et des temps de maintien de l'expérimentation) beaucoup trop grands ! D'où l'idée de passer à une expérimentation en mésocosme.

### Hypothèses de l'expérimentation *ex situ*

Nous émettons l’hypothèse que l’occurrence des effets de priorité entre deux espèces A et B et leurs directions dépendent :

* de l’environnement et des optima écologiques des deux espèces 

*En semant en monoculture chacune des espèces sur un gradient de conditions écologiques, il est possible de déterminer la "distance à l'optimum" : autrement dit le pourcentage de biomasse perdu par rapport à des conditions optimales.*

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/mesure_dist.png)

* de l’asymétrie des interactions biotiques entre les deux espèces

*En semant en monoculture et en interaction les espèces A et B on peut dans un premier temps mesurer le RII (Relative Interaction Index : 1 = facilitation obligatoire ; 0 = pas d'interaction ; -1 = exclusion compétitive) des deux espèces et dans un deuxième temps mesurer l'écart entre ces RII : cela donne l'asymétrie de l'interaction entre les 2 espèces.*

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/mesure_inter.png)

Avec une occurence des effets de priorité lorsque l'asymétrie et l'équidistance aux optima écologiques sont relativement modérés mais non nuls :

*Après avoir mesuré l'asymétrie d'interaction entre les espèces, puis la distance à l'optimum de chaque espèce, il est possible de replacer chaque "trinome" (espèce A - espèce B - environnement) dans le cadre des hypothèses suivants. Il "suffit" enfin de mesurer l'occurence d'effets de priorité pour tester ces hypothèses.*

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/hyp_gene.png)

### Les tests préliminaires mis en place :

*Choix du gradient de stress* 

Pour choisir le gradient de stress adapté nous avons mis à germer les espèces dans 8 types de substrats différents :
Allant du substrat 1, le moins fertile et le moins rétenteur en eau avec 1:9 de terreau et 4,5:9 de sable et de graviers; jusqu’au substrat 9, le plus fertile et le plus rétenteur en eau avec 8:9 de terreau et 0,5:9 de sable et de graviers.

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/pretest2_750.jpg)

Après avoir semé, arrosé, mis au germinateur et obtenu une croissance suffisante, nous avons récolté la biomasse aérienne.

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/images_pretest_750.jpg)

Résultats du test préliminaire :

Les relevés de la biomasse aérienne montrent que les espèces sont en mesure de croître dans tous les types de substrat. Ensuite, ils semblent qu'effectivement les substrats avec une plus forte proportion de terreau, sont plus favorables à la croissance des six espèces étudiées.
Nous avonc donc choisi le substrat 3 et 8 pour notre gradient de stress final: Deux types de sols dans lesquels les espèces peuvent se développer mais qui sont suffisamment distincts pour induire des réponses potentiellement différentes chez les espèces étudiées.

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/result_prem_biomasse_750.png)

*Choix des traits mesurés*

Si la plupart des études sur le sujet s’intéressent principalement aux traits aériens, certaines se penchent également sur les manifestations des effets de priorité au niveau racinaire et mettent en évidence que la biomasse souterraine est significativement affectée (cf les articles suivants : [1](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2745.12829?casa_token=kc3yBdODZggAAAAA%3AEcz2r4sKcmmT2esZP2lm53gz_9m5ftFgSNThObu7t2ewz_bHgwVCki4J2iXdMsPyxkeeZrs5gut4ZV4NaQ),[2](https://reader.elsevier.com/reader/sd/pii/S1439179122000044?token=3048582DC34D54945B675C976CBBFEE41874DC6625D96E56B3E75B66475E3CD842B671C7E58600B6FD0D20905D3BBEB6&originRegion=eu-west-1&originCreation=20220523165611), [3](https://onlinelibrary.wiley.com/doi/full/10.1111/oik.08886))

Aussi, nous avons à notre tour, envisagé de mesurer ces traits racinaires.

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/testracine_750.jpg)


Malheureusement les conclusions tirées des pré-tests sont sans appel : 

-procédé beaucoup trop long pour une expérimentation avec 736 pots (entre 10 et 20mintes par pot pour isoler les racines)

-impossibilité d’isoler les racines de chacune des espèces séparément

-déchirements au niveau des radicelles risquant d’induire des biais dans les mesures de biomasse racinaire

Bref, le design de notre expérimentation ne nous permettra malheureusement pas de mesurer la biomasse racinaire ! Pour une prochaine expérimentation peut-être 😊



### Les mesures dont nous aurons besoin :

Pour tester ce cadre d'hypothèses, nous aurons besoin de connaître pour 2 (groupes d')espèces A et B :

* la distance à l'optimum : il faut donc faire pousser les espèces en monoculture sur un gradient de conditions écologiques (ici la proportion terreau / (sable+gravier) qui est relative à la quantité de nutriments et de rétention en eau du substrat)
* l'asymétrie des interactions : il faut donc faire pousser A et B en monoculture et en interaction.
* l'occurence d'effet de priorité : pour cela, il faut semer A et B simultanément et en décalé, puis comparer les biomasses quand elles arrivent simultanément, en premier ou en second.

Nous avons souhaité mesurer le comportement de chaque espèce individuellement, mise en relation avec le groupe d'espèce dont elle ne fait pas partie. Par exemple : *Plantago lanceolata* est une subordonnée, elle sera donc mise en interaction avec les dominantes.

Si on fait quelques répétitions, on arrive vite à un grand nombre de pots : 736 pour être exact !

### Du 16 mars au 13 avril: pesée des graines

La quantité de graines a été pesée pour chaque pot, et disposée dans des enveloppes prêtes à être semées le jour J :

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/pesee.png)

### Du 14 avril au 15 avril : préparation des substrats

Le mélange des substrats a été réalisé à l'aide d'une bétonnière et d'un peu d'huile de coude. Initialement prévue sur des tables, nous nous sommes vite rendu compte que le poids total serait un problème pour l'équilibre de ces tables.. Afin d'éviter toute catastrophe, les pots ont été redescendus avant le semis.

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/substrat.png)

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/ausol.png)

### Du 19 avril au 20 avril : Semis n°1

Immédiatement après le premier semis, arrosage et protection contre les oiseaux ont été mis en place :

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/semis.png)

### Depuis le 20 avril : On regarde les plantes pousser

En essayant de ne pas piéger les voisins dans les filets.. Et en testant des mesures du recouvrement de végétation..

![](https://raw.githubusercontent.com/RenaudJau/PEExp/main/suivi1.png)

*Suite(s) au prochain(s) épisode(s)*



