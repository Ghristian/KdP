//KdP Uebung 13 Alexander Parotsidi und Christian Papenfuss
//Abgabe zum 02.02.2024
//Tutor: Adrian-Maurice Alex

// Aufgabe 1a)

trait PairPrioQueue[K : Ordering, V]:

  def insert(key : K, value : V) : Unit // Precondition: None, Effect: The element key-value pair is added to the priority queue., Result: None

  def extractMin() : V // Precondition: The queue is not empty., Effect: A smallest element is removed from the priority queue., Result: A value corresponding to the smallest element of the priority queue is returned.
    
  def isEmpty : Boolean // Precondition: None, Effect: None, Result: true is returned if and only if the priority queue has no elements.

  def save (key: K, value: V): Unit =  // Precondition: None, Effect: The element key-value pair is added to the priority queue., Result: None
    insert(key, value)




// Aufgabe 1b)
//   |  import math.Ordered.orderingToOrdered
import math.Ordering.Implicits.infixOrderingOps

class PairPrioQueueLN[K : Ordering, V] extends PairPrioQueue[K, V] :
  private class Node(val key: K, val value: V, var next: Node)
  private val dummy: Node = new Node(null.asInstanceOf[K], null.asInstanceOf[V], null)
  private var head: Node = dummy
  private val order = summon[Ordering[K]]
  def insert(key: K, value: V): Unit =  // Precondition: None, Effect: The element key-value pair is added to the priority queue., Result: None
    var prev: Node = dummy
    var current: Node = head
    while (current != null && key > current.key)
      prev = current
      current = current.next
    prev.next = new Node(key, value, current)
  
  
  def extractMin(): V = 
    if (isEmpty)
      throw new NoSuchElementException("Die PrioQueue ist leer")
    val minNode = head.next
    head.next = minNode.next
    minNode.value
  
  
  def isEmpty: Boolean = head.next == null



  
// Aufgabe 1c)

trait Stack[A: Ordering]:
  private val orderin = summon[Ordering[A]]
  def push(element: A): Unit // Precondition: None,Result: None, Effect: x is now the most recent element in the stack.


  def pop(): Option[A] // Precondition: Stack is not empty., Result: The most recent element is returned., Effect: The most recent element is removed from the stack.


  def top(): A // Precondition: Stack is not empty, Effect: None, Result: The top element of the stack is returned without removing it.

  def isEmpty: Boolean // Precondition: None, Effect: None, Result: true is returned if and only if the stack has no elements.

abstract class StackImpl[A: Ordering] extends Stack[A]:
  private val queue: PairPrioQueue[Int, A] = new PairPrioQueueLN[Int, A]
  private var counter = 0

  def push(value: A): Unit =  // Precondition: None,Result: None, Effect: x is now the most recent element in the stack.
    queue.insert(counter, value)
    counter += 1
  
  override def pop(): Option[A] = // Precondition: Stack is not empty., Result: The most recent element is returned., Effect: The most recent element is removed from the stack.
    if (isEmpty) None
    else Some(queue.extractMin())
  
  def isEmpty: Boolean = queue.isEmpty // Precondition: None, Effect: None, Result: true is returned if and only if the stack has no elements.

  def hminqueue : Int = counter // Precondition: None, Effect: None, Result: The number of elements in the stack is returned.
    



//Aufgabe 2a und 2b (und 2d) in der PDF.
//Aufgabe 2c)
import scala.reflect.ClassTag

trait MyPrioQueue[K : Ordering] :
    //Vorraussetzung: Key gehört zu K
    //Effekt: Keiner
    //Ergebnis: k wird in TertiaerBaum eingefuegt.
    def insert(key : K) : Unit
    
    //Vorraussetzung: Keine
    //Effekt: Keiner
    //Ergebnis: Minimum aus TertiaerBaum ist geliefert.    
    def extractMin() : K
    //Vorraussetzung: Keine
    //Effekt: Keiner
    //Ergebnis: Ueberprueft ob TertiaerBaum leer ist.    
    def isEmpty : Boolean


    //Vorraussetzung: Alle Elemente geören zur Typklasse K
    //Effekt: Keiner
    //Ergebnis: Tertiarbaum min. der Größe 2 wird erstellt.    
class TertiaryTree[ K : Ordering : ClassTag] (capacity : Int) extends MyPrioQueue [K] :
    private val ord = summon[Ordering[K]]
    import ord.mkOrderingOps
    private val array : Array[K]  = new Array[K] (if capacity < 2 then 2 else capacity)
    private var last : Int = -1

// parent = Elternknoten  lchild = linker Knoten  mchild = mittlerer Knoten  rchild = rechter Knoten
    private def parent(i : Int) : Int = (i - 1) / 3
    private def lchild (i : Int) : Int = 3 * i + 1
    private def mchild (i : Int) : Int = 3 * i + 2
    private def rchild (i : Int) : Int = 3 * i + 3


    //Vorraussetzung: Key gehört zu K
    //Effekt: Keiner
    //Ergebnis: k wird in TertiaerBaum eingefuegt.
    def insert(key: K): Unit = 
        last += 1
        array(last) = key
        BubbleUp(last)

    //Vorraussetzung: Zwei Indices der Größe kleiner als array.length
    //Effekt: Keiner
    //Ergebnis: Element an Position i wird mit Element an Position j getauscht.   
    def swap ( i : Int, j : Int) : Unit =
            val temp = array(i)
            array(i) = array(j)
            array(j)= temp


    //Vorraussetzung: Keine
    //Effekt: Keiner
    //Ergebnis: Ueberprueft ob TertiaerBaum leer ist.
    def isEmpty : Boolean = last == -1

    //Vorraussetzung: Keine
    //Effekt: Keiner
    //Ergebnis: Minimum aus TertiaerBaum ist geliefert.   
    def extractMin(): K = 
        if (isEmpty) throw new Exception("Queue ist leer")
        val min = array(0)
        array(0) = array(last)
        last -= 1
        BubbleDown(0)
        min

    //Vorraussetzung: Index i kleiner als array.length
    //Effekt: Keiner
    //Ergebnis: Element an Position i wird so lang Hochgeblubbert bis Heapeigenschaften erfüllt sind.
    def BubbleUp(i : Int): Unit = 
        var currentIndex = i
        while (currentIndex > 0 && array(currentIndex) < array(parent(currentIndex))) 
            swap(currentIndex, parent(currentIndex))
            currentIndex = parent(currentIndex)


    //Vorraussetzung: Index i kleiner als array.length
    //Effekt: Keiner
    //Ergebnis: Element an Position i wird so lang Runtergeblubbert bis Heapeigenschaften wieder erfüllt sind.            
    def BubbleDown(i : Int): Unit = 
        var currentIndex = i
        while (lchild(currentIndex) <= last)
    // Index des kleinsten Kindes ermitteln, dazu Sequenz aus Indices lchild, mchild, rchild. Dann filtern aller Indices die größer als last sind. Dann mit minBy Minimum finden.
            val minChildIndex = Seq(lchild(currentIndex), mchild(currentIndex), rchild(currentIndex)).filter(i => i <= last).minBy(i => array(i))

    // Swap mit kleinstem Kind falls current Knoten größer ist
            if (array(currentIndex) > array(minChildIndex)) 
                swap(currentIndex, minChildIndex)
                currentIndex = minChildIndex
            else 
            return // Ordnungseigenschaft ist erfüllt



// Test: fügt 1-10 in den TertiaryTree ein und gibt diese dann auf Konsole aus.
@main
    def testTriTree() : Unit =
        val capacity = 10
        val priorityQueue = new TertiaryTree[Int](capacity)
        for (i <- 1 to 10) 
            priorityQueue.insert(i)
            println(s" $i")

testTriTree()


//Aufgabe 2d in PDF