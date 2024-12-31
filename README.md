# Réécriture de termes

## Résumé :

L'objectif de ce projet est, à travers la réécriture de termes, de trouver toutes les implications d'un ensemble d'équations. Pour ce faire, on peut effectuer une *Complétion de Knuth-Bendix* pour chaque équation. Puis, en utilisant le système de réécriture obtenu, on peut déterminer quelle équation est impliquée par l'équation associée à ce système en normalisant les deux côtés de l'équation en question. S'ils sont égaux, alors elle est impliquée ; sinon, non.

## Travail effectué :

Avant de s'attaquer aux équations, on va d'abord s'intéresser aux mots, ce qui a été implémenté ici. On retrouve les fonctions liées à la réécriture dans le répertoire ```lib``` et les fonctions liées à l'utilisation de la réécriture dans ```bin```.

Le programme s'effectue en deux temps : d'abord, on complète les systèmes de réécriture, puis, on fabrique le graphe des implications. Tout est sauvegardé dans le répertoire ```archives```.

## Comment compiler :

Il suffit d'effectuer un ```make``` à la racine du projet. Par défaut, des tests vont être lancés avant la complétion et la création du graphe.

On peut aussi lancer un exemple et visualiser les systèmes complétés en éditant les variables associées dans le fichier ```bin/helper/globals.ml```.