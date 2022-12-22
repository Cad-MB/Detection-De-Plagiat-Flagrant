type 'a suffixtree = Node of int * string * 'a suffixtree list
val vide : 'a suffixtree
val getenfant : 'a suffixtree -> 'a suffixtree list
val explode : string -> string list
val appartient : string -> 'a suffixtree -> bool * 'a suffixtree
val filter : string -> 'a suffixtree list -> 'a suffixtree list
val print_list_enfant : 'a suffixtree list -> string
val ajout : string -> 'a suffixtree -> int -> 'a suffixtree
val arbresuffixe : 'a suffixtree -> string -> int -> 'a suffixtree
val souschaine : string -> string -> bool
val recup_chaine : 'a suffixtree list -> string
val recup_chaine_un : 'a suffixtree list -> string
val hauteur_arbre : 'a suffixtree -> int
val souschainecommunes : string -> string -> string
val compression : 'a suffixtree -> 'a suffixtree
val souschainescommunescompress : string -> string -> string
val arbre : 'a suffixtree
val compare_mot : 'a list -> 'a list -> 'a list
val ajout_mot_compress : string -> 'a suffixtree -> 'a suffixtree
val arbresuffixescompresse : 'a suffixtree -> string -> 'a suffixtree
