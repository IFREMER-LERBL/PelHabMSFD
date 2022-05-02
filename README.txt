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

Probl�mes dans les codes Matlab (I.Rombouts) : 
- Indices "Richness" faux (double suppression des colonnes correspondants aux metadonn�es, donc suppression des premi�res colonnes de taxons).
- Indices "LCBD" faux (somme abondances et ann�es)
- Monthly_pool --> fusion de diff�rents taxons (ex: Alexandrium + Alexandrium minutum)


Compatibilit� scripts avec version Java :

dans le fichier R "init DCSMM"
ligne 4 Sys.setenv(Java_Home="C:/Program Files/Java/jre1.8.0_141/") : V�rifier que la version Java install�e sur l'ordinateur est la m�me :
dans disque C, Programmes ou Programmes File : Java (pour PC portable, version = 144 et non 141).

V2.6 = v2.5 avec r�solution d'un probl�me pos� par les donn�es zoo de Pte et Gde Rade (pour lesquelles le PH2 n'est calculable que sur la p�riode 2011-2015) :
la v2.5 calcule le cycle uniquement sur la p�riode "de r�f�rence" qui est d�finie par d�faut comme �tant toute la p�riode pr�c�dent la p�riode d'�valuation. Dans notre cas : avant 2010.
Les stations dont les s�ries temporelles sont trop courtes pour qu'une p�riode de r�f�rence ant�rieure � la p�riode d'�valuation puisse servir au calcul du cycle, sont �limin�es.
Cela pose probl�me pour les stations zoo pour lesquelles les s�ries sont courtes. Le script de la v2.6 a donc �t� modifi� pour :
- calcul du cycle sur une p�riode de r�f (d�finie par d�faut comme �tant toute la p�riode pr�c�dent la p�riode d'�valuation).
- calcul du cycle sur toute la s�rie de donn�es (donc sur la p�riode d'�valuation) lorsqu'il n'est pas possible de d�finir une p�riode de r�f�rence (le probl�me ne se pose pour le moment que pour ces deux stations Zoo de Toulon).

Pour l'�valuation on reste l�-dessus. Par la suite, il faudra veiller � d�finir des conditions pour la p�riode de r�f�rence et notamment une dur�e minimum. Car si, par exemple, on a une s�rie qui va de 2009 � 2015 et qu'on d�finie la p�riode d'�valuation de 2010 � 2015,
cela signifie que le cycle ne sera calcul� qu'� partir des donn�es d'une ann�e (2009) ce qui fait trop court et risque de biaiser la d�finition du cycle.

V2.7 = v2.6 avec :
- r�solution d'un pb pour faire tourner data CPR : sur les stations o� il n'y avait qu'une ann�e (n'appartenant pas � la p�riode d'�valuation), le scipt buguait
- actualisation du script pour calcul du PH3

V2.8 = v2.7
avec :
- PH2 : CumSum sur les anomalies (sans retrancher la moyenne) + CumSum sur anomalies (-moy) + Cum Sum sur data brutes (-moy)
- tendance sur les anomalies et sur les data brutes
- interface : zoo ou phyto
- r�pertoire somme retir�

PH3 : possibilit� calcul sur la p�riode d'�valuation.

V2.9 : 
ajout des tests de permutation pour LCBD dans PH3.

v2.10 :
PH2 : correction : erreur dans les versions pr�c�dentes : le cycle �tait calcul� sur la p�riode d'�valuation au lieu d'�tre calcul� sur la p�riode de r�f�rence.
Dans version 2.10 : ok, calcul du cycle sur p�riode de r�f.

PH3 : 
ajout des moyennes annuelles pour tous les indices.

V2.11 :
Ajout de la tendance (spearman) sur les CumSum (en plus de donn�es brutes et anomalies).

v2.12 :
= version 2.11 mais il y avait une erreur dans la formule de l'indice Hulburt pour le PH3.
Erreur rectifi�e et on repasse � la formule d'Isabelle (sans passer par log de x+1, comme pr�conis� par Alex et Ana�s).