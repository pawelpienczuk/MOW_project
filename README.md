# MOW_project

19Z MOW FEIT WUT Project </br>
Package type project </br>
Roxygen skeleton added (<i>model_eval.R</i>) </br>
Ctrl+Shift+B - install+build </br>
Ctrl+Shift+D - generate documentation </br>
Documentation in <i>man</i> directory </br>

<b>R Tutorial:</b> </br>
https://www.tutorialspoint.com/r/index.htm </br>

Data collected from: </br>
https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction </br>

DMR packages and snippets found in link below: </br>
https://www.wiley.com//legacy/wileychi/cichosz/Rcode_snippet.html?type=SupplementaryMaterial </br>

Typical installation of packages: </br>
install.packages("Z:/Download/Packages/dmr.util_1.0.tar.gz", repos = NULL, type = "source") </br>
in RStudio Install-Package

Regression Trees Tutorial: </br>
https://uc-r.github.io/regression_trees </br>

Improving Regression Trees: </br>
rpart parameters -> minsplit, cp
randomForest, xgboost

Feature selection: </br>
Najprostszym zadowalającym rozwiązaniem jest zastosowanie pakietu randomForest w celu wyznaczenia predykcyjnej przydatności atrybutów, a następnie wybór (może w kilku wariantach) pewnej liczby najlepszych atrybutów (np. najlepsze 25%, 50% itp.). W tym przypadku zalecałbym użycie miary "mean decrease accuracy", co wymaga użycia w wywołaniu funkcji randomForest argumentu importance=TRUE, zaś w wywołaniu funkcji importance lub varImpPlot argumentu type=1. Inne bardziej zautomatyzowane podejścia można znaleźć w kilku pakietach wymienionych tutaj:
https://github.com/FrancisArgnR/R-FeatureSelection-Packages

Procedury oceny:
http://elektron.elka.pw.edu.pl/~pcichosz/mow/wyklad/mow-w7.pdf
Rozdział 3: holdout, k-CV, bootstrap
