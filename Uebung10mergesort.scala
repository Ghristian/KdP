/* Uebung 10 Abgabe zum 12.01.2024
Von Alexander Parotsidi und Christian Papenfuss
 Tutorium 04 Montag 10-12 Uhr
Tutor: Alex, Adrian-Maurice
 */

//2a)

// Vorrasusetzungen: Datentyp von Elementen in Ordering
// Effekt: Eine sortierte Liste ist ausgegeben
// Ergebnis:Eine sortierte Liste ist geliefert

//def merge [T:Ordering] (left : Int, right : Int, List[T])
    //val ord = summon[Ordering[T]]
    //import ord.mkOrderingOps


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
 */
