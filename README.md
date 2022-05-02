# D1_MSFD
GUI for computation of PH2 and PH3 indicators in the frame of MSFD, Descriptor 1: "Pelagic Habitat" 

Data format for PH1, PH2 and PH3 indices computation (D1 MSFD: Pelagic Habitat).

PH1:	

PH2:	CSV file (separator ';' and decimal ',')
	Filename must contain 'PHYTO' or 'ZOO'
	First column --> "station" (character format) without `'`
	Second column --> "date" (yyyy-mm-dd format)
	Third column --> "values" (numeric format)
Data cleaning:	Remove years with less than 9 months
		Remove years before year missing
		"values"=NA if the begininng month is not "O1"

PH3:	CSV file (separator ';' and decimal ',')
	First column --> "station" (character format)
	Second column --> "day" (numeric format)
	Third column --> "month" (numeric format)
	Fourth column --> "year" (numeric format)
	Fifth column --> "longitude" (numeric format)
	Sixth column --> "latitude" (numeric format)
	Seventh column --> "taxon" (character format)
	Eighth column --> "abundance" (numeric format)
Data cleaning:	Remove years with less than 6 months
		Remove stations with less than 5 years

Physico-Chemical:	CSV file (separator ';' and decimal ',')
			First column --> "station" (character format) without `'`
			Second column --> "date" (yyyy-mm-dd format)
			Third column --> "param" (character format)
			Fourth column --> "values" (numeric format)

Problèmes dans les codes Matlab (I.Rombouts) : 
- Indices "Richness" faux (double suppression des colonnes correspondants aux metadonnées, donc suppression des premières colonnes de taxons).
- Indices "LCBD" faux (somme abondances et années)
- Monthly_pool --> fusion de différents taxons (ex: Alexandrium + Alexandrium minutum)


Compatibilité scripts avec version Java :

dans le fichier R "init DCSMM"
ligne 4 Sys.setenv(Java_Home="C:/Program Files/Java/jre1.8.0_141/") : Vérifier que la version Java installée sur l'ordinateur est la même :
dans disque C, Programmes ou Programmes File : Java (pour PC portable, version = 144 et non 141).

V2.6 = v2.5 avec résolution d'un problème posé par les données zoo de Pte et Gde Rade (pour lesquelles le PH2 n'est calculable que sur la période 2011-2015) :
la v2.5 calcule le cycle uniquement sur la période "de référence" qui est définie par défaut comme étant toute la période précédent la période d'évaluation. Dans notre cas : avant 2010.
Les stations dont les séries temporelles sont trop courtes pour qu'une période de référence antérieure à la période d'évaluation puisse servir au calcul du cycle, sont éliminées.
Cela pose problème pour les stations zoo pour lesquelles les séries sont courtes. Le script de la v2.6 a donc été modifié pour :
- calcul du cycle sur une période de réf (définie par défaut comme étant toute la période précédent la période d'évaluation).
- calcul du cycle sur toute la série de données (donc sur la période d'évaluation) lorsqu'il n'est pas possible de définir une période de référence (le problème ne se pose pour le moment que pour ces deux stations Zoo de Toulon).

Pour l'évaluation on reste là-dessus. Par la suite, il faudra veiller à définir des conditions pour la période de référence et notamment une durée minimum. Car si, par exemple, on a une série qui va de 2009 à 2015 et qu'on définie la période d'évaluation de 2010 à 2015,
cela signifie que le cycle ne sera calculé qu'à partir des données d'une année (2009) ce qui fait trop court et risque de biaiser la définition du cycle.

V2.7 = v2.6 avec :
- résolution d'un pb pour faire tourner data CPR : sur les stations où il n'y avait qu'une année (n'appartenant pas à la période d'évaluation), le scipt buguait
- actualisation du script pour calcul du PH3

V2.8 = v2.7
avec :
- PH2 : CumSum sur les anomalies (sans retrancher la moyenne) + CumSum sur anomalies (-moy) + Cum Sum sur data brutes (-moy)
- tendance sur les anomalies et sur les data brutes
- interface : zoo ou phyto
- répertoire somme retiré

PH3 : possibilité calcul sur la période d'évaluation.

V2.9 : 
ajout des tests de permutation pour LCBD dans PH3.

v2.10 :
PH2 : correction : erreur dans les versions précédentes : le cycle était calculé sur la période d'évaluation au lieu d'être calculé sur la période de référence.
Dans version 2.10 : ok, calcul du cycle sur période de réf.

PH3 : 
ajout des moyennes annuelles pour tous les indices.

V2.11 :
Ajout de la tendance (spearman) sur les CumSum (en plus de données brutes et anomalies).

v2.12 :
= version 2.11 mais il y avait une erreur dans la formule de l'indice Hulburt pour le PH3.
Erreur rectifiée et on repasse à la formule d'Isabelle (sans passer par log de x+1, comme préconisé par Alex et Anaïs).
