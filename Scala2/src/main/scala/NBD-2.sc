/// NBD 2
/// ZAD 1

val DNI_PRACA = List("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela")

def matchdni(x: String): String = x match {
  case "Poniedzialek" => "Praca"
  case "Wtorek" => "Praca"
  case "Sroda" => "Praca"
  case "Czwartek" => "Praca"
  case "Piatek" => "Praca"
  case "Sobota" => "Weekend"
  case "Niedziela" => "Weekend"
  case _ => "Nie ma takiego dnia"
}

println(matchdni("Wtorek"))
println(matchdni("Sobota"))
println(matchdni("L4")) //// no nie wiem :)


/// ZAD 2

class KontoBankowe(saldo: Double) {
  private var _stanKonta: Double = saldo

  def stanKonta: Double = _stanKonta

  def this() {
    this(0)
  }

  def wplaty(kwota: Double): Double = {
    _stanKonta = _stanKonta + kwota
    _stanKonta
  }


  def rozchody(kwota: Double): Double = {
    _stanKonta = _stanKonta - kwota
    _stanKonta
  }
}

val saldo1 = new KontoBankowe(69)

println(saldo1.stanKonta) //Początkowe saldo klienta
println(saldo1.wplaty(100)) // dodatkowy przychód
println(saldo1.rozchody(1000)) // kosz

/// ZAD 3
case class Osoba(var Imie: String, var Nazwisko: String)

def show(osoba: Osoba): String = {
  osoba match {
    case Osoba("Itzan", "Escamilla") => "Hola"
    case Osoba("Philip", "Lham") => "Hallo"
    case Osoba("Jan", "Kowalski") => "Siema"
    case _ => "Hi"
  }
}

val p1 = Osoba("Itzan", "Escamilla")
val p2 = Osoba("Philip", "Lham")
val p3 = Osoba("Jan", "Kowalski")
val p4 = Osoba("Janek", "MałyPolakRodak")

println(show(p1))
println(show(p2))
println(show(p3))
println(show(p4))


// Zad 4
def zadanie4(number: Int, liczba: Int => Int): Int = {
  liczba(liczba(liczba(number)))
}

val number = 100
def minusik(num: Int): Int = num - 1
println(zadanie4(number, minusik))

// Zad 5


case class Osoba1(private val imie: String, private val nazwisko: String, val podatek: Double = 0d)

trait Pracownik extends Osoba1 {
  override val podatek: Double = 0.2

  def pensja: Double = 0
}

trait Student extends Osoba1 {
  override val podatek: Double = 0
}

trait Nauczyciel extends Pracownik {
  override val podatek: Double = 0.1
}

val ludek0 = new Osoba1("Robert", "Lewandowski") with Student
println(s"Podatek studenta wynosi: ${ludek0.podatek}%")

val ludek1 = new Osoba1("Jan", "Kowalski") with Pracownik
println(s"Podatek studenta wynosi: ${ludek1.podatek}%")

val ludek2 = new Osoba1("Janusz", "Kowalski") with Nauczyciel
println(s"Podatek studenta wynosi: ${ludek2.podatek}%")

val ludek3 = new Osoba1("Karol", "Kowalski") with Nauczyciel with Pracownik
println(s"Podatek nauczyciela pracownika wynosi: ${ludek3.podatek}%")

val ludek4 = new Osoba1("Pawel", "Kowalski") with Pracownik with Nauczyciel
println(s"Podatek pracownika nauczyciela wynosi: ${ludek4.podatek}%")


















