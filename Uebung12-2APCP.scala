//Uebung 12

import scala.reflect.ClassTag
//2b)
trait MyQueue[A]:
  def enqueue(x: A): Unit
  def dequeue(): A
  def isEmpty: Boolean
  def size: Int

//Voraussetzung: /
// Effekt: /
// Ergebnis: Leerer Stack  mit dynamischer groesse ist gegeben
class DynamicArrayQueue[A: ClassTag] extends MyQueue[A]:
  private var array: Array[A] = new Array[A](1)
  private var front: Int = 0
  private var back: Int = 0
    
    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: (array.lenght/4 <= amount < array.length) wird erfüllt
  private def resize(): Unit =
    val cap: Int = array.length
    if cap / 4 <= size && size < cap then return
    //alloziert einen neuen Array mit halber oder doppelter Groesse des alten Arrays:
    val newArray: Array[A] = new Array[A](if cap / 4 > size then cap / 2 else cap * 2)
    for i <- 0 until size do
      newArray(i) = array((front + i) % cap)

    array = newArray
    front = 0
    back = size

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: x ist in Warteschlange hinzugefuegt
  def enqueue(x: A): Unit =
    array(back) = x
    back = (back + 1) % array.length
    resize()

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: letztes Element ist in Warteschlange entfernt gegebenfalls resize   
  def dequeue(): A =
    if !isEmpty then
      val result: A = array(front)
      array(front) = null.asInstanceOf[A]
      front = (front + 1) % array.length
      resize()
      result
    else throw new NoSuchElementException("Queue is empty")
    
    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: Boolean ob Warteschlange leer ist ist gegeben
  def isEmpty: Boolean = front == back
    
    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: Groesse der Warteschlange ist gegeben
  def size: Int =
  if front <= back then back - front
  else array.length + back - front

//Testmethode:
@main
def dyArTest(): Unit =
  val testQueue1: MyQueue[Int] = new DynamicArrayQueue[Int]

    for i <- 1 to 25 do
      testQueue1.enqueue(i)

    while !testQueue1.isEmpty do
      println(testQueue1.dequeue())

    for i <- 1 to 10 do
      testQueue1.enqueue(i)

    while !testQueue1.isEmpty do
      println(testQueue1.dequeue())

//  1 bis 25 wird in Warteschlange einfeguegt, dann alle entfernt und 1 bis 10 eingefuegt

//2d)

//Voraussetzung: Keine
// Effekt: keiner
//Ergebnis: Leerer Stack mit maximal Kapazität max(1,capacity) und var Full im Verwaltungskopf ist gegeben
class FullArrayQueue[A: ClassTag] (capacity : Int) extends MyQueue [A] :
    //Verwaltungskopd
    private val n : Int = (if capacity < 1 then 1 else 
        capacity)
    private val array : Array[A] = new Array [A] (n)
    private var front : Int = 0
    private var back : Int = 0
    private var Full : Boolean = false
    private var counter : Int = 0

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: x ist in Warteschlange hinzugefuegt
    def enqueue(x: A): Unit = 
      if !Full then
        val newBack: Int = (back + 1) % n
        counter = counter + 1
        if newBack != front then
          array(back) = x
          back = newBack
        else
          array(back) = x
          back = front
          Full = true
      else
        throw new Exception("Queue ist voll")

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: letztes Element ist in Warteschlange entfernt
    def dequeue(): A = 
        if !isEmpty then 
            val result : A = array(front)
            array(front) = null.asInstanceOf[A]
            front = (front + 1) % 10
            counter = counter - 1
            Full = counter == n - 1 // Setzt Full auf false wenn queue nach entfernen nicht voll ist
            result
        else throw new Exception ("Queue ist leer")

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: Boolean ob Warteschlange leer ist ist gegeben
    def isEmpty: Boolean = !Full && counter == 0

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: Groesse der Warteschlange ist gegeben
    def size: Int = 
      if !Full then 
        (n + back - front) % n
      else n

//Testmethode:     
@main
def FullArTest(): Unit =
  val testQueue2: MyQueue[Int] = new FullArrayQueue[Int] (10)

    for i <- 1 to 10 do
     testQueue2.enqueue(i)

    while !testQueue2.isEmpty do
      println(testQueue2.dequeue())

    for i <- 1 to 10 do
      testQueue2.enqueue(i)

    while !testQueue2.isEmpty do
      println(testQueue2.dequeue())
//  2 mal 1-10 ist ausgegeben


//2e)

//Voraussetzung: Keine
// Effekt: keiner
//Ergebnis: Leerer Stack mit maximal Kapazität max(1,capacity) und Prioritaeten bei enqueue() dequeue() ist gegeben

trait VipElement

class PrioQueue[A: ClassTag](capacity: Int) extends MyQueue[A]:
    private val n: Int = (if capacity < 1 then 1 else capacity) + 1
    private val array: Array[A] = new Array[A](n)
    private var front: Int = 0
    private var back: Int = 0

    // VIP-Warteschlange für Elemente mit hoher Priorität
    private var vipFront: Int = 0
    private var vipBack: Int = 0

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: x ist in Warteschlange hinzugefuegt
    def enqueue(x: A): Unit =
        val newBack: Int = (back + 1) % n
        if newBack != front then // Wenn nicht voll
            // Prüft ob  Element hohe Priorität hat
            if isVip(x) then
                array(vipBack) = x
                vipBack = (vipBack + 1) % n
            else
                array(back) = x
                back = newBack
        else throw new Exception("Queue ist voll")

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: letztes Element ist in Warteschlange entfernt
    def dequeue(): A =
        if !isEmpty then
            // Wenn die VIP-Warteschlange nicht leer, hole ein VIP-Element
            if !isVipEmpty then
                val resultVip: A = array(vipFront)
                array(vipFront) = null.asInstanceOf[A]
                vipFront = (vipFront + 1) % n
                resultVip
            else
                val result: A = array(front)
                array(front) = null.asInstanceOf[A]
                front = (front + 1) % n
                result
        else throw new Exception("Queue ist leer")

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: Boolean ob Warteschlange leer ist ist gegeben
    def isEmpty: Boolean = front == back && vipFront == vipBack

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: Groesse der Warteschlange ist gegeben
    def size: Int = (n + back - front) % n + (n + vipBack - vipFront) % n

    //Voraussetzung: /
    // Effekt: /
    // Überprüfe, ob die VIP-Warteschlange leer ist
    private def isVipEmpty: Boolean = vipFront == vipBack

    //Voraussetzung: /
    // Effekt: /
    // Überprüfe, ob ein Element eine hohe Priorität hat (Beispiel: ist eine Instanz von VipElement)
    private def isVip(x: A): Boolean = x.isInstanceOf[VipElement]

@main
def Priotest () : Unit =
    val testQueue3 : MyQueue[Int] = new PrioQueue[Int] (11)
    for i <- 1 to 10 do
        testQueue3.enqueue(i)
    while !testQueue3.isEmpty do
        println(testQueue3.dequeue())
    for i <- 1 to 10 do
        testQueue3.enqueue(i)
    while !testQueue3.isEmpty do
        println(testQueue3.dequeue())

//Test: Priotest() gibt 2 mal 1-10 aus
