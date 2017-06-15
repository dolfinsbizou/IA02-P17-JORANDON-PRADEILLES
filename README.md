# IA02-P17-JORANDON-PRADEILLES

Play Arimaa: http://personal.inet.fi/koti/egaga/arimaa-begin/tutorial.html

# Install

Copy `arimaa.pl` into the apps/arimaa folder of pengines.
Run.

# Réflexion

À propos des déplacements possibles, on prend que partiellement en compte le fait qu'il ne faut pas marcher sur d'autres pièces.
Il faut aussi empêcher les déplacements pour lesquels il y a une pièce adverse de plus fort niveau qui bloque.
Il faut aussi empêcher de continuer les déplacements qui aboutissent sur des trous qui ne sont pas défendus par des pièces amies.
En bref, il faut pour pouvoir faire tout ça, décomposer en interne de la fonction get_possible_move les déplacements en mouvements unitaires, et avorter au point critique (rencontre d'un trou non protégé, rencontre d'un adversaire plus fort) les déplacements concernés. Pour le cas où un mouvement aboutirait sur une pièce, plutôt que de choisir de calculer un détour (compliqué, chiant, grosse flemme), on peut simplement avorter le mouvement.
