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

  def enqueue(x: A): Unit =
    array(back) = x
    back = (back + 1) % array.length
    resize()

  def dequeue(): A =
    if !isEmpty then
      val result: A = array(front)
      array(front) = null.asInstanceOf[A]
      front = (front + 1) % array.length
      resize()
      result
    else throw new NoSuchElementException("Queue is empty")

  def isEmpty: Boolean = front == back
  def size: Int = if front <= back then back - front else array.length - (front - back)


def dyArTest(): Unit =
  val testQueue: MyQueue[Int] = new DynamicArrayQueue[Int]

  for i <- 0 to 10 do
    testQueue.enqueue(i)

  while !testQueue.isEmpty do
    println(testQueue.dequeue())

  for i <- 0 to 10 do
    testQueue.enqueue(i)

  while !testQueue.isEmpty do
    println(testQueue.dequeue())



//2d)

//Voraussetzung: Keine
// Effekt: keiner
//Ergebnis: Leerer Stack mit maximal Kapazität max(1,capacity) ist gegeben
class FullArrayQueue[A: ClassTag] (capacity : Int) extends MyQueue [A] :
    //Verwaltungskopd
    private val n : Int = (if capacity < 1 then 1 else 
        capacity)
    private val array : Array[A] = new Array [A] (n)
    private var front : Int = 0
    private var back : Int = 0
    private var Full : Boolean = false

    def enqueue(x: A): Unit = 
        val newBack : Int = (back + 1 ) % n
        if newBack != front then // Wenn nicht voll
            array(back) = x
            back = newBack
        else 
          array(back)= x
          back = front
          Full = true
    def dequeue(): A = 
        if !isEmpty then 
            val result : A = array(front)
            array(front) = null.asInstanceOf[A]
            front = (front + 1) % 10
            result
        else throw new Exception ("Queue ist leer")

    def isEmpty: Boolean = !(Full)
    def size: Int = 
      if !Full then 
        (n+back-front) % n
      else n

def FullArTest(): Unit =
  val testQueue: MyQueue[Int] = new DynamicArrayQueue[Int]

  for i <- 0 to 10 do
    testQueue.enqueue(i)

  while !testQueue.isEmpty do
    println(testQueue.dequeue())

  for i <- 0 to 10 do
    testQueue.enqueue(i)

  while !testQueue.isEmpty do
    println(testQueue.dequeue())

