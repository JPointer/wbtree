# wbtree

## Temat projektu
Funkcyjna implementacja stuktury drzewa o ograniczonym zrównoważeniu umożliwiająca:

* dodawania węzła
* usunięcia węzła
* wyszukiwania węzła
* operacją sumy oraz przecięcia drzew

## Opis projektu
  Drzewo o ograniczonym zrównoważeniu, to zrównoważone binarne drzewo poszukiwań (BST), które do zachowania zrównoważenia wykorzystuje informacje o ilości węzłów w lewym i prawym poddrzewie. Drzewo takie wykorzystuje dwie wartości - deltę, określającą czy należy wykonać rotację drzewa oraz gammę, określającą, czy należy wykonać rotację pojedynczą czy też podwójną. Drzewo jest zbalansowane, jeśli ciężar jednego poddrzewa jest mniejsza niż ciężar drugiego pomnożonego przez deltę. Drzewa takie nie są w pełni zrównoważone (w porównaniu do drzew czerwono-czarnych czy drzew AVL).
  
  W celu wykonania zrównoważenia należy wykonać rotację istniejącego drzewa. Rotacja drzewa – operacja polegająca na lokalnej zmianie struktury binarnego drzewa poszukiwań (BST) z zachowaniem porządku wierzchołków.
  
  ![Rotacja](https://upload.wikimedia.org/wikipedia/commons/2/23/Tree_rotation.png)

## Narzedzia budujące
  * sbt
  
## Dokumentacja
  API doc możliwa do wygenerowania przez polecienie sbt doc bądz dostępna w katalogu docs.

## Testy 
  Przykładowe testy zawarte w katalogu src/test możliwe do odpalenia w Idea IntelliJ  badż poleceniem sbt test.
