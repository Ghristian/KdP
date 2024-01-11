/* Uebung 10 Abgabe zum 12.01.2024
Von Alexander Parotsidi und Christian Papenfuss
 Tutorium 04 Montag 10-12 Uhr
Tutor: Alex, Adrian-Maurice
 */


//Aufgabe 1a)

// Voraussetzung]: keine
// Effekt : keine (funktionale Programmierung)
// Ergebnis : Die Eingabeliste ist geprueft ob sie auf- oder absteigend sortiert sei, ein Boolean (True) ist gegeben wenn die Liste dies ist

/* Tests
checkSort(List(1,2,3,4,5), Ordering[Int]) = true
checkSort(List(5,4,3,2,1), Ordering[Int]) = true
checkSort(List(1,2,3,2,1), Ordering[Int]) = true
checkSort(List("a","b","c"), Ordering[String]) = true
checkSort(List("c","b","a"), Ordering[String]) = true


*/
def isitSorted[A](list: List[A], ordering: Ordering[A]): Boolean =
  def checkSorted(list2: List[A], compare: Ordering[A], increasing: Boolean): Boolean = list2 match
    case Nil => true
    case _ :: Nil => true
    case x :: y :: xs =>
      val comparison = compare.compare(x, y)
      val sorted =
        if (increasing) comparison <= 0
        else comparison >= 0

      sorted && checkSorted(y :: xs, compare, increasing)

  checkSorted(list, ordering, increasing = true) || checkSorted(list, ordering, increasing = false)


// Aufgabe 1b)

// Voraussetzung: w ist nicht leer
// Effekt : keine 
// Ergebnis  : Die Elemente einer Eingabeliste sind mit einer Eingabezahl summier, wenn diese kleiner als die 

/*Tests
sumSmaller(4,List(1,2,3,4,5))= 10
sumSmaller(0,List(1,2,3,4,5))= 0
sumSmaller(-0,List(-1,-2,-3,-4,-5)) = -15
sumSmaller(1.5,List(1.1,1.2,1.3,1.4,1.5)) = 6.5
*/

def sumSmaller[A: Numeric](w: A, l: List[A]): A = 
  val num = summon[Numeric[A]]
  import num.mkNumericOps

  def step(l: List[A], acc: A, w: A, cmp:Numeric [A]): A =
    l match 
      case head :: xs =>
        if (cmp.compare(head, w) <= 0) 
          step(xs, acc + head, w, cmp)
        else 
          step(xs, acc, w, cmp)
      case Nil => acc
    

  step(l, num.zero, w, num) 

// Aufgabe 1c)

enum EntwederOder[+A]:
  case Links(a: A)
  case Rechts(b: A)
  case Gleich(c: A)

def navi[A: Ordering](x1: A, x2: A): EntwederOder[Unit] = // Diese Funktion soll wie ein Navi funktionieren, in dem man 2 Koordinaten eingibt und eine Richtung zurueckbekommt 
  val ord = summon[Ordering[A]]                           // Hier werden die Eigenschaften von Ordering mit einer Variable definier
  import ord.compare                                      // Hier Importiert, sodass man x1 und x2 vergleichen kann
  (x1, x2) match                                          // Hier wird Patternmatching angewendet um herauszufinden wolang wie gefahrenwerden muss
    case (a, b) if compare(a, b) == 0 => EntwederOder.Gleich(()) // Sind die Koordinaten gleich faehrt man gleichmaessig weiter
    case (a, b) if compare(a, b) < 0 => EntwederOder.Links(())   // Ist a groesser als b faehrt man links weiter
    case (a, b) => EntwederOder.Rechts(())                       // Ansonsten nach rechts 


def wsis[A: Ordering](str1: A, str2: A): EntwederOder[String] = // Diese Funktion vergleich zwei Strings lexikografisch, und sagt an welcher String der kleinere ist
   val ord = summon[Ordering[A]]                                // Auch hier werden die Eigenschaften definiert
   import ord.compare                                           // Und hier importiert
   compare(str1, str2) match                                    // Hier wird das Pattermatching angewendet, in dem compare auf die zwei Eingabestrigns verwednet wird
    case 0 => EntwederOder.Gleich(s"$str1 ist gleich $str2")    // Ist das Ergebnis 0 sind beide Strings gleich
    case x if x < 0 => EntwederOder.Links(s"Der String ist der lexikografisch kleinere: $str1") // Ist das Ergebnis kleiner ist der Str1 (linke) der kleiner
    case _ => EntwederOder.Rechts(s"Der String ist der lexikografisch kleinere: $str2")         // Ansonsten ist Str2 (rechte) das kleinere

// Aufgabe 1d)

enum myBools:
  case True
  case False
  case Or(left: myBools, right: myBools)
  case And(left: myBools, right: myBools)
  case Not(inner: myBools)

def eval(a: myBools): Boolean =
    import myBools.*
     a match
      case True => true
      case False => false
      case Or(left, right) => eval(left) || eval(right)
      case And(left, right) => eval(left) && eval(right)
      case Not(inner) => !eval(inner)

def negateVals(a: myBools): myBools =
  import myBools.* 
  a match
    case True => False
    case False => True
    case Or(left, right) => Or(negateVals(left),negateVals(right))
    case And(left, right) => And(negateVals(left),negateVals(right))
    case Not(inner) => Not(negateVals(inner))
  
// Aufgabe 1e)

// Voraussetzung: i ist nicht leer 
// Effekt : keiner (funktionale Programmierung)
// Ergebnis : Die EIngabelist wird beim Index i in zwei Listen aufgeteilt

/* Tests
splitAt(List(1,2,3,4,5,6),3) = (List(1, 2, 3),List(4, 5, 6))
splitAt(List(1,2,3,4,5,6),3) = (List(),List(1, 2, 3, 4, 5, 6))
splitAt(List(1,2,3,4,5,6),-3) = (List(1, 2, 3, 4, 5, 6),List())

*/

def splitAt[A](list : List[A], i : Int) : (List[A], List[A]) = // Man gibt eine Liste und ein Parameter i an und die Liste wird in 2 geteilt die erste hat alle vin 0 bis i-1 und die andere i bis Ende
  (list, i) match                                              // Dieser wird dann gematcht
  case (Nil, _) => (Nil, Nil) // (*)                           // Wenn die Liste leer ist wird sie in 2 leere Listen geteilt
  case (_, 0) => (Nil, list) // (*)                            // Ist der Parameter i=0 wird die Liste am i-ten Element (0) geteil was die Liste an sich sit
  case (x::xs, i) =>                                           // Hier tritt der Basisfall auf
    val (left, right) = splitAt(xs, i-1)                       // Durch die Variabel left und right die die Funktion rekursiv aufrufen mit der liste xs und i-1 ist es moeglich die einzelnen Zahlen x auf left zu Speicher
    (x::left, right) // (*)                                    // Hier wird x and left geconst weshalb es am Beispiel List(1,2,3,4,5) mit i=2 => List(2,1) ist
                                                               // Die Listen kommen zustande wenn die Rekursionsanker auftretten:
                                                               // Wenn i = 0 erreicht werden die uebrigen Elemente mit right definiert
    
    




//Aufgabe 2a)

// Vorrasusetzungen: List ist bereits sortiert
// Effekt: keiner
// Ergebnis: Liste die aus beiden Eingabelisten besteht
def merge [T:Ordering] (left: List[T], right: List[T]): List[T] = 
    val ord = summon[Ordering[T]]
    import ord.mkOrderingOps
    (left, right) match 
    case(Nil, _) => right
    case(_, Nil) => left
    case(l :: leftlist, r :: rightlist) =>
        if (l < r) l::merge(leftlist, right)
        else r::merge(left, rightlist)
/* 
Tests: 
merge(List(1,2,3,4),List(4,5,6)) =  List(1, 2, 3, 4, 4, 5, 6)
merge(List(1,2,3,4),List(5,6)) =  List(1, 2, 3, 4, 5, 6)
merge(List(1,3,4,7,9),List(2,5,6)) =  List(1, 2, 3, 4, 5, 6, 7, 9)
merge(List("a","c"),List("b","d")) =  List(a, b, c, d)
 */

// Vorrasusetzungen: Alle Elemente vom selben Datentyp
// Effekt: keiner
// Ergebnis: sortierte Liste ist gegeben
def mergesort [T:Ordering] (liste: List[T]): List[T] = 
    val ord = summon[Ordering[T]]
    import ord.mkOrderingOps
    val n = liste.length / 2
    if (n == 0) liste
    else 
        val (left, right) = liste.splitAt(n)
        merge(mergesort(left), mergesort(right)) 
/* 
Tests:
mergesort(List(1,4,2,25,7,3,8,10,9)) = List(1, 2, 3, 4, 7, 8, 9, 10, 25)
mergesort(List("c","b","d","a")) =  = List(a, b, c, d)
 mergesort(List(1.3,1.1,1.2,1.8,1.7,1.29)) =  List(1.1, 1.2, 1.29, 1.3, 1.7, 1.8)
 */



 //Aufgabe 2b)

enum Vielleicht[+T]:
    case Nichts
    case Wert(t : T)

// Vorrasusetzungen: Erstes Element von Typ Vielleicht
// Effekt: keiner
// Ergebnis: Funktion auf Wert angewendet, wenn Element  "Nichts" dann  Ausgabe "Nichts"
def map[A,B](v : Vielleicht[A], f:A=>B) : Vielleicht[B] =
    import Vielleicht.*
    v match
        case Nichts => Nichts
        case Wert(t) => Wert(f(t))
/*
Tests:
map(Wert(12), (x: Int) => x + 2)   = Wert(14)
map(Wert(12),(x: Int) => x*2)    =  Wert(24)
map(Nichts, (x: Int) => x + 2)   = Nichts
 */

// Vorrasusetzungen: Eine Liste mit Elemente der Liste vom Typ Vielleicht ist eingegeben
// Effekt: keiner
// Ergebnis: Liste mit Elementen aus Wert(n) in Eingabeliste ist zurückgegeben
def extract[A](eingabeliste: List[Vielleicht[A]]): List[A] =
  eingabeliste.foldRight(List.empty[A])(
    (v, acc) => v match {
      case Vielleicht.Wert(t) => t :: acc
      case Vielleicht.Nichts   => acc
    }
  )
  /* 
  Tests:
  extract(List(Wert(1),Wert(2),Nichts,Wert(3)))   = List(1, 2, 3)
  extract(List(Wert(1),Wert(2),Nichts,Wert(3),Nichts,Wert(4),Nichts,Nichts))  = List(1, 2, 3, 4)
  */



//Aufgabe 2c)

enum Link:
    case G, S, P

enum Chain:
    case Empty
    case Join(left : Chain, l : Link, right : Chain)

def toList(chain: Chain): List[Link] =
    import  Link.* 
    import Chain.*
    chain match 
        case Chain.Empty  => List.empty
        case Chain.Join(l, link, r) => toList(l) ++ List(link) ++ toList(r)


val XatarsKette: Chain = Chain.Join ( Chain.Join (Chain.Empty, Link.G, Chain.Empty) ,Link.S,Chain.Join (Chain.Empty, Link.P, Chain.Empty) )
/* 
toList(XatarsKette) = List(G, S, P)
 */
