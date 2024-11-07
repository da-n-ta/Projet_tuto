```mermaid
graph LR
    X(("1ere reunion"))
    style X stroke:red,stroke-width:4px
    A(("`Martin`"))
    style A stroke:red,stroke-width:4px
    B(("`Analyses univariees`"))
    style B stroke:red,stroke-width:4px
    C(("`Alioune`"))
    D(("`Serge`"))
    I(("`Danielle`"))

    E(("`2eme reunion`"))
    style E stroke:red,stroke-width:4px
    F(("`Debut d'analyse bivariees`"))
    style F stroke:red,stroke-width:4px
    G(("`re-analyse univariee`"))
    H(("`♨️ 
    290 | 290`"))
    style H stroke:red,stroke-width:4px
    subgraph Assemblage
    X -- 25 --> A
    X -- 15 --> C
    X -- 10 --> D
    X --10 --> I
    A -- 30 --> B
    C -- 30 --> B
    D -- 30 --> B
    I -- 30 --> B
    end
    subgraph Mettre au frais
    B -- 180 --> E
    end
    subgraph Cuisson
    E -- 15 --> F

    E -- 4 --> G
    F -- 10 --> H
    G -- 30 --> H
    
    end
```
on doit s'interresser a une ou deux variables( **pluies**, temp, vent) en fonction de l'altitude et la longitude et latitude et prendre just une periode de l'annee par exemple l'hivers
pour la variable RR(pluies), on enleve les données manquantes
fextRemes (package R)
ismev (celui du livre, on ne va pas utiliser)
imputation de données ( pour les donnees manquantes)
voir les donnee manquantes, si c les premieres annees on pourra enlever les annees 50s

definir les stations qu'on garde, par ex garder 50 stations et faire des boxplots et faire des cartes (heatmap modeliser par rapport a la position , modeliser avec l'approche des valeurs extremes)

TO DO:
bien choisir quelles stations on prend et quelle variable on prend (+ stat descriptives)
modeliser le max de la variable pour chaque année par station de meteo
ajuster la loi des valeurs extremes aux donnés 
carte de france+ poser les stations sur la carte de france
si l'approche classique, on prend le max de chaque années, pour 50 années, puis on pourra extrapoler 
probleme: grande perte de données

