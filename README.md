# TIPE 2022
TIPE concours des ENS.

But : typechecker du calcul des constructions, essentiellement basé
sur Type Theory and Formal Proof : An Introduction de R. Nederpelt et H. Geuvers.

Le code peut légèrement différer entre le rapport transmis et le code présent ici.

# Utilisation
`dune exec --profile debug ./main.exe -- test.tipe` ou `nix run .#tipe_debug -- test.tipe`

Le fichier `test.tipe` contient une liste de définition dans le calcul des constructions.
Le typechecker les vérifie une par une puis les ajoute au contexte et continue, et les affiche
après. S'il rencontre une définition mal typée, il lève une erreur. Le détail est disponible
dans le rapport.