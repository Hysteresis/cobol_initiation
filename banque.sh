cobc -free -m depot.cob
cobc -free -m retrait.cob
cobc -free -m virement.cob
cobc -free -m nouveau_solde.cob
cobc -free -x banque.cob

if [ $? -eq 0 ]; then
    ./banque
else
    echo "Erreur lors de la compilation. Arrêt du script."
fi