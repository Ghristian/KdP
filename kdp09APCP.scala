/*Abgabe 9 Abgabe zum 22.12.2023    
Von Alexander Parotsidi und Christian Papenfuss
 Tutorium 04 Montag 10-12 Uhr
Tutor: Alex, Adrian-Maurice
*/

// Aufgabe 1a mit map)

// Vorrasusetzungen: a,b und liste sind nicht leer
// Effekt: keiner
// Ergebnis: Alle Zahlen a aus der Liste sind durch b ersetzt.

/* Tests
replace(1,2,List(1,1,1,1)) => List(2,2,2,2)
replace(0,3,List(1,2,3,4)) => List(1,2,0,4)
...
*/

def replace [A](a: A, b: A, liste:List[A]) : List[A] =
	liste.map(element => if (element == a) b else element) 

// Aufgabe 1a mit Rekursion)
// Vorrasusetzungen: a,b und liste sind nicht leer
// Effekt: keiner, weil funktionelle Programmierung
// Ergebnis: Alle Zahlen a aus der Liste sind rekursiv durch b ersetzt.

/* Tests
replacerec(1,3,List(1,1,1,1)) => List(3,3,3,3)
replacerec(0,4,List(1,2,3,4)) => List(1,2,3,0)
replacerec(0,0,List(0)) => List(0)
...
*/

def replacerec [A](a: A, b:A, liste:List[A]) : List[A] =
	def step (aktlist: List[A]) : List[A] =
		aktlist match
			case Nil => Nil
			case kopf :: tail => if (kopf == a) then b :: step(tail) else kopf :: step(tail)
	
	step(liste)

// Aufgabe 1b)

// Vorrasusetzungen: Es ist eine Funktion gegeben 
// Effekt: keiner
// Ergebnis: Ein Boolean ist geliefert, wenn die Funktion angewandt auf die Liste stimmt

/* Tests
isSorted((_:Int)>(_:Int),List(1,2,3,4,5)) => false
isSorted((_:Int)>(_:Int),List(5,4,3,2,1)) => true
isSorted((_:Int)>(_:Int),List(5,3,4,2,1)) => false
isSorted((_:Int)>(_:Int),List()) => true
...
*/

def isSorted [A](vergl: (A,A) => Boolean, liste: List[A]) : Boolean =
	def step (aktlist:List[A]) : Boolean =
		aktlist match
			case Nil | _ :: Nil => true
			case zahl1 :: zahl2 :: tail => vergl(zahl1,zahl2) && step(zahl2::tail)
	step(liste)

// Aufgabe 1c)

// Vorrasusetzungen: n ist nicht leer
// Effekt: die Quersumme der n ersten Zahlen ist angezeigt
// Ergebnis: Die Quersumme der n ersten Zahlen aus der Liste ist geliefert

/* Tests
takeNumbers(1,List(1,2,3,4)) => Die Quersumme der ersten n Elementen ist : 1
takeNumbers(2,List(1,2,3,4,5,6)) => Die Quersumme der ersten n Elementen ist : 3
takeNumbers(5,List()) => Die Quersumme der ersten n Elementen ist : 0
takeNumbers(0,List(1,2,3,4,5,6)) => Die Quersumme der ersten n Elementen ist : 0
takeNumbers(_,List(1,2,3,4,5)) => Error ???
...
*/

def takeNumbers[A](n: Int, liste:List[A]) : Unit =
	val zahlen = liste.take(n)
	val quersumme = zahlen.map(_.toString.toInt).sum
	println("Die Quersumme der ersten n Elementen ist :"+ quersumme)



// Aufgabe 1d) 

// Vorrasusetzungen: keine 
// Effekt: die Rückwärtsliste ist angezeigt
// Ergebnis: Die Eingabeliste ist rekursiv rückwärts umgewandelt und ist gezeigt

/* Tests
reverese(List(1,2,3,4,5)) => Die neue Liste ist List(5,4,3,2,1)
reverese(List(1,2,3,4,5)) => Die neue Liste ist List(5,4,3,2,1)
reverse(List(0)) => Die neue Liste ist List(0)
reverse(List()) => Die neue Liste ist List()
reverse(List(_)) => Error
*/

def reverse [A](liste:List[A]) : Unit = 
    val listnew =liste.foldRight(List.empty)((head,tail ) => (tail :+ head))
    return
         val print = println("Die neue Liste ist "+listnew)



// Aufgabe 2a)

// Vorrasusetzungen: Liste ist nicht leer, Prädikat ist nicht leer
// Effekte: keine
// Ergebnis: Liste mit erfülltem Prädikat und Liste mit nicht erfülltem Prädikat 


def partition[A](p: A => Boolean, xs: List[A]): (List[A], List[A]) = 
  def partitionHelper(remaining: List[A], ErfuelltList: List[A], NichtList: List[A]): (List[A], List[A]) = 
    remaining match 
      case Nil => (ErfuelltList.reverse, NichtList.reverse)
      case head :: tail =>
        if (p(head))
          partitionHelper(tail, head :: ErfuelltList, NichtList)
        else
          partitionHelper(tail, ErfuelltList, head :: NichtList)
    
  

  partitionHelper(xs, Nil, Nil)

// Testliste mit mehreren Randfällen
val myList1 = List(1, 5, 3, 8, 2, 0, -1, 100, 101, 99)
val myList2 = List(1,2,3,4,5,6,7,8,9)
/* Tests
partition((x: Int) => x >= 5, myList2)
=(List(5, 6, 7, 8, 9),List(1, 2, 3, 4))

partition((x: Int) => x >= 5, myList1)
=(List(5, 8, 100, 101, 99),List(1, 3, 2, 0, -1))
*/

// Aufgabe 2b)

// Vorrasusetzungen: keine
// Effekt: keiner
// Ergebnis: Liste mit Elementen aller Eingabelisten ist geliefert

def lconcat[A](lists: List[List[A]]): List[A] =
  lists match 
    case Nil => Nil
    case head :: tail => head ::: lconcat(tail)
  
    /*
Tests
lconcat(List(List(1,2,3),List(4,5),List(6,7,8)))
= List(1, 2, 3, 4, 5, 6, 7, 8)

lconcat(List(List(1,2,3),List(4,5),Nil,List(6)))
 = List(1, 2, 3, 4, 5, 6)
 */


// Aufgabe 2c)

// Vorrasusetzungen: keine
// Effekt: keiner
// Ergebnis: Liste mit Elementen aller Eingabelisten ist geliefert

def argmax[A](f: A => Double, xs: List[A]): A =
  xs.filter(x => f(x) == xs.map(f).max).head

/*Tests
argmax((x: Int) => x-40, myList)
= 101

argmax((x: Int) => x*x, myList)
101

argmax((x: Int) => x-x*x, myList)
= 1

argmax((x:Int ) => 40-x*x, myList)
= 0

argmax((x:Int ) => -x, myList)
= -1
 */


//2d)
//siehe PDF


