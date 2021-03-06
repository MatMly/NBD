# Nierelacyjne bazy danych (NBD)

## NBD - Scala1

1. Stwórz 7 elementową listę zawierającą nazwy dni tygodnia. Napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami korzystając z:
   - Pętli for
   - Pętli for wypisując tylko dni z nazwami zaczynającymi się na „P”
   - Pętli while
2. Dla listy z ćwiczenia 1 napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami korzystając z:
   - Funkcji rekurencyjnej
   - Funkcji rekurencyjnej wypisując elementy listy od końca
3. Stwórz funkcję korzystającą z rekurencji ogonowej do zwrócenia oddzielonego przecinkami stringa zawierającego elementy listy z ćwiczenia 1
4. Dla listy z ćwiczenia 1 napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami korzystając z:
   - Metody foldl
   - Metody foldr
   - Metody foldl wypisując tylko dni z nazwami zaczynającymi się na „P”
5. Stwórz mapę z nazwami produktów i cenami. Na jej podstawie wygeneruj drugą, z 10% obniżką cen. Wykorzystaj mechanizm mapowania kolekcji.
6. Zdefiniuj funkcję przyjmującą krotkę z 3 wartościami różnych typów i wypisującą je
7. Zaprezentuj działanie Option na dowolnym przykładzie (np. mapy, w której wyszukujemy wartości po kluczu)
8. Napisz funkcję usuwającą zera z listy wartości całkowitych (tzn. zwracającą listę elementów mających wartości różne od 0). Wykorzystaj rekurencję.
9. Zdefiniuj funkcję, przyjmującą listę liczb całkowitych i zwracającą wygenerowaną na jej podstawie listę, w której wszystkie liczby zostały zwiększone o 1. Wykorzystaj mechanizm mapowania kolekcji.
10. Stwórz funkcję przyjmującą listę liczb rzeczywistych i zwracającą stworzoną na jej podstawie listę zawierającą wartości bezwzględne elementów z oryginalnej listy należących do przedziału <-5,12>. Wykorzystaj filtrowanie.

## NBD - Scala2

1. Wykorzystaj Pattern Matching w funkcji przyjmującej parametr typu String. Dla stringów odpowiadających nazwom dni tygodnia funkcja ma zwrócić „Praca” i „Weekend” (odpowiednio dla dni roboczych i wolnych), dla pozostałych napisów „Nie ma takiego dnia”.
2. Zdefiniuj klasę KontoBankowe z metodami wplata i wyplata oraz własnością stanKonta - własność ma być tylko do odczytu. Klasa powinna udostępniać konstruktor przyjmujący początkowy stan konta oraz drugi, ustawiający początkowy stan konta na 0.
3. Zdefiniuj klasę Osoba z własnościami imie i nazwisko. Stwórz kilka instancji tej klasy. Zdefiniuj funkcję, która przyjmuje obiekt klasy osoba i przy pomocy Pattern Matching wybiera i zwraca napis zawierający przywitanie danej osoby. Zdefiniuj 2-3 różne przywitania dla konkretnych osób (z określonym imionami lub nazwiskami) oraz jedno domyślne.
4. Zdefiniuj funkcję przyjmującą dwa parametry - wartość całkowitą i funkcję operującą na wartości całkowitej. Zastosuj przekazaną jako parametr funkcję trzykrotnie do wartości całkowitej i zwróć wynik.
5. Zdefiniuj klasę Osoba i trzy traity: Student, Nauczyciel, Pracownik. Osoba powinna mieć własności read only: imie, nazwisko, podatek. Pracownik powinien mieć własność pensja (z getterem i seterem). Student i Pracownik powinni przesłaniać własność podatek – dla Studenta zwracamy 0, dla Pracownika 20% pensji. Nauczyciel powinien dziedziczyć z Pracownika, dla niego podatek zwraca 10% pensji. Stwórz obiekty z każdym z traitów, pokaż jak podatek działa dla każdego z nich. Stwórz obiekty z traitami Student i Pracownik, pokaż jak podatek zadziała w zależności od kolejności w jakiej te traity zostały dodane przy tworzeniu obiektu.
