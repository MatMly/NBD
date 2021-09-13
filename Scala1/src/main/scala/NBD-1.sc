import org.graalvm.compiler.api.replacements.Fold
import org.graalvm.compiler.core.common.util.IntList

import scala.::
import scala.annotation.tailrec

// Zad1
val dniTygodnia = List("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela")

// a
def wypiszDniTygodniaFor(dniTygodnia: List[String]): String = {
  val result = new StringBuilder
  for (dzien <- dniTygodnia) {
    if (result.isEmpty) {
      result.append(dzien)
    } else {
      result.append(",")
      result.append(dzien)
    }
  }
  result.toString
}

println(wypiszDniTygodniaFor(dniTygodnia))

// b
def wypiszDniTygodniaForIf(dniTygodnia: List[String]): String = {
  val result = new StringBuilder
  for (dzien <- dniTygodnia.filter(_.startsWith("P"))) {
    if (result.isEmpty) {
      result.append(dzien)
    } else {
      result.append(",")
      result.append(dzien)
    }
  }
  result.toString
}

println(wypiszDniTygodniaForIf(dniTygodnia))

// c

def wypiszDniTygodniaWhile(dniTygodnia: List[String]): String = {
  val result = new StringBuilder(dniTygodnia.head)
  var i = 1
  while (i < dniTygodnia.size) {
    result.append(",")
    result.append(dniTygodnia(i))
    i += 1
  }
  result.toString
}

println(wypiszDniTygodniaWhile(dniTygodnia))

// Zad2/3


def wypiszDniTygodniaRec(myList: List[String]): String = {
  if (myList.isEmpty) ""
  else if (myList.size == 1) myList.head
  else myList.head + "," + wypiszDniTygodniaRec(myList.tail)
}

println(wypiszDniTygodniaRec(dniTygodnia))

def wypiszDniTygodniaRecReverse(myList: List[String]): String = {
  if (myList.isEmpty) ""
  else if (myList.size == 1) myList.head
  else wypiszDniTygodniaRecReverse(myList.tail) + "," + myList.head
}

println(wypiszDniTygodniaRecReverse(dniTygodnia))

// a b
@tailrec
def wypiszDniTygodniaTailRec(dniTygodnia: List[String], result: String = ""): String =
  dniTygodnia match {
    case head :: tail if result.isEmpty => wypiszDniTygodniaTailRec(tail, result + head)
    case head :: tail => wypiszDniTygodniaTailRec(tail, result + "," + head)
    case Nil => result
  }
println(wypiszDniTygodniaTailRec(dniTygodnia))

@tailrec
def wypiszDniTygodniaTailRecReverse(dniTygodnia: List[String], result: String = ""): String =
  dniTygodnia match {
    case head :: tail if result.isEmpty => wypiszDniTygodniaTailRecReverse(tail, head + result)
    case head :: tail => wypiszDniTygodniaTailRecReverse(tail, head + "," + result)
    case Nil => result
  }
println(wypiszDniTygodniaTailRecReverse(dniTygodnia))

// Zad4

def wypiszDniTygodniaFoldL(dniTygodnia: List[String]) =
  dniTygodnia.tail.foldLeft(dniTygodnia.head)((acc, dzien) => acc + "," + dzien)

println(wypiszDniTygodniaFoldL(dniTygodnia))

def wypiszDniTygodniaFoldR(dniTygodnia: List[String]) =
  dniTygodnia.init.foldRight(dniTygodnia.last)((acc, dzien) => acc + "," + dzien)

println(wypiszDniTygodniaFoldR(dniTygodnia))

def wypiszDniTygodniaFoldLFilter(dniTygodnia: List[String]) = {
  val p = dniTygodnia.filter(_.startsWith("P"))
  p.tail.foldLeft(p.head)((acc, dzien) => acc + "," + dzien)
}

println(wypiszDniTygodniaFoldLFilter(dniTygodnia))

//Zad5


val produkty = Map("PIWO" -> 50, "WODKA" -> 120, "BIMBER" -> 400, "WHISKY" -> 500)
val produktyObnizka = produkty.map { case (produkt, cena) => (produkt, cena * 0.9) }
println(produktyObnizka)

//Zad6
def Zad6fun(tup: (String, Int, Any)) = {
  println(tup._1)
  println(tup._2)
  println(tup._3)
}

val produkt = "BIMBER"
//Zad7
val price: Option[Double] = produktyObnizka.get(produkt)
price.map(cena => s"jest znizka na $produkt - cena $cena").foreach(println)

//Zad8
@tailrec
def removeZeros(input: List[Int], result: List[Int] = List[Int]()): List[Int] = input match {
  case head :: tail if head == 0 => removeZeros(tail, result)
  case head :: tail => removeZeros(tail, result :+ head)
  case Nil => result
}
println(removeZeros(List(1, 0, 2, 0, 3, 0, 4, 0, 5, 0)))


//// Zad9
val liczbyCalkowite = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
def dodajJeden(calkowite: List[Int]): List[Int] = calkowite.map(_ + 1)
println(liczbyCalkowite)
println(dodajJeden(liczbyCalkowite))

//// Zad10
val liczbyRzeczywiste = List(-10, -8, -6, -4, -2, -1, 1, 2, 3, 10, 85, 14)

def absMinus5ToPlus12(numbers: List[Int]) = numbers
  .filter(_ >= -5)
  .filter(_ <= 12)
  .map(_.abs)

println(absMinus5ToPlus12(liczbyRzeczywiste))



