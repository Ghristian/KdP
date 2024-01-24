
// Queue mit Arrays implementiert

import scala.reflect.ClassTag

 
trait  MyQueue[A]:
// Objekte vom Typ A werden in Warteschlange gespeichert
    
    //Voraussetzung: Keine
    // Effekt: keiner
    // Ergebnis: x ist in Warteschlange eingereiht
    def enqueue(x:A) : Unit

    //Voraussetzung: Keine
    // Effekt: keiner
     // Ergebnis: Letztes Element aus Warteschlange ist entfernt
    def dequeue() : A 
    
    //Voraussetzung: Keine
    // Effekt: keiner
     // Ergebnis: Ueberpruefung of Warteschlange leer ist, Boolscher Wert ist zurueckgegeben
    def isEmpty : Boolean
    
    //Voraussetzung: Keine
    // Effekt: keiner
     // Ergebnis: Laenge der Warteschlange ist gegeben
    def size : Int
 
//Voraussetzung: Keine
// Effekt: keiner
//Ergebnis: Leerer Stack mit maximal Kapazität max(1,capacity) ist gegeben
class ArrayQueue[A: ClassTag] (capacity : Int) extends MyQueue [A] :
    //Verwaltungskopd
    private val n : Int = (if capacity < 1 then 1 else 
        capacity) + 1
    private val array : Array[A] = new Array [A] (n)
    private var front : Int = 0
    private var back : Int = 0

    def enqueue(x: A): Unit = 
        val newBack : Int = (back + 1 ) % n
        if newBack != front then // Wenn nicht voll
            array(back) = x
            back = newBack
        else throw new Exception ("Queue ist voll")

    def dequeue(): A = 
        if !isEmpty then 
            val result : A = array(front)
            array(front) = null.asInstanceOf[A]
            front = (front + 1) % 10
            result
        else throw new Exception ("Queue ist leer")

    def isEmpty: Boolean = front == back
    def size: Int = (n+back-front) % n

@main
def test () : Unit =
    val testQueue : MyQueue[Int] = new ArrayQueue[Int] (11)
    for i <- 0 to 10 do
        testQueue.enqueue(i)
    while !testQueue.isEmpty do
        println(testQueue.dequeue())
    for i <- 0 to 10 do
        testQueue.enqueue(i)
    while !testQueue.isEmpty do
        println(testQueue.dequeue())