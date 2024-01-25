/* Uebung 12
 Christian Papenfuss & 
 Tutorium 04 Montag 10-12 Uhr
Tutor: Alex, Adrian-Maurice
*/
// Aufgabe 1a
/* 
- Mehrfachvererbung kann zu komplexen Hierarchien führen, die schwer zu verstehen sind
- Schwierigkeiten beim Versetehen vom Code
- Aufgrund des Diamant-Problem kann es zu Problemen kommen, wenn eine Klasse von zwei Klassen erbt, die von einer Klasse erben, die dieselbe Methode implementiert
- Traits ermöglichen es, mehrere Eigenschaften zu definieren, die dieselbe Methode implementieren, ohne dass es zu Problemen kommt
- Traits unterstützen die Interoperabilität zwischen verschiedenen Klassen (Scala und Java)
*/

// Aufgabe 1b

trait Bracket
object RoundBracket extends Bracket
object CurlyBracket extends Bracket
object SquareBracket extends Bracket

trait MyStack[A]:
  def push(a: A): MyStack[A]
  def pop(): MyStack[A]
  def isEmpty: Boolean
  def top: A
  def multipush(items: List[A]): MyStack[A]

class MyStacks[A] extends MyStack[A]:
  var stack = List[A]()
  def push(a: A): MyStack[A] = // Vorraussetzung: a ist nicht null, Effekt: keins, Ergebnis: Element a wird auf den Stack gelegt
    stack = a :: stack
    this
   def pop(): MyStack[A] = // Vorraussetzung: Stack ist nicht leer, Effekt: keins, Ergebnis: oberstes Element wird vom Stack entfernt
    if (!isEmpty)
      stack = stack.tail
    this
  def isEmpty: Boolean = stack.isEmpty // Vorraussetzung: keins, Effekt: keins, Ergebnis: true, wenn der Stack leer ist, sonst false
  def top: A = // Vorraussetzung: Stack ist nicht leer, Effekt: keins, Ergebnis: oberstes Element wird zurückgegeben
    if (!isEmpty) stack.head
    else throw new Exception("Der Stack ist leer")
  def multipush(items: List[A]): MyStack[A] = // Vorraussetzung: items ist nicht null, Effekt: keins, Ergebnis: alle Elemente aus items werden auf den Stack gelegt
    for (item <- items)
      stack = item :: stack
    this

// Aufgabe 1b
/* 
Vorraussetzung: exp ist nicht leer und besteht nur aus den Zeichen '(', ')', '{', '}', '[', ']'
Effekt: keiner
Ergebnis: true ist zurückgegeben, wenn die Klammern in exp korrekt geschachtelt sind, sonst false
 */

def isCorrect(exp: String): Boolean =
  var stack: MyStack[Bracket] = new MyStacks[Bracket] // Stack mit Klammern
  for (char <- exp) // Für jedes Zeichen in exp
    char.toString match // Matche das Zeichen
      case "(" => stack = stack.push(RoundBracket) // Wenn es eine öffnende Klammer ist, füge sie dem Stack hinzu
      case ")" => if (stack.isEmpty || stack.top != RoundBracket) throw new Exception("Fehler") else stack = stack.pop() // Wenn es eine schließende Klammer ist, entferne die oberste Klammer vom Stack, wenn sie eine öffnende Klammer ist
      case "{" => stack = stack.push(CurlyBracket) // Wenn es eine öffnende Klammer ist, füge sie dem Stack hinzu
      case "}" => if (stack.isEmpty || stack.top != CurlyBracket) throw new Exception("Fehler") else stack = stack.pop() // Wenn es eine schließende Klammer ist, entferne die oberste Klammer vom Stack, wenn sie eine öffnende Klammer ist
      case "[" => stack = stack.push(SquareBracket) // Wenn es eine öffnende Klammer ist, füge sie dem Stack hinzu 
      case "]" => if (stack.isEmpty || stack.top != SquareBracket) throw new Exception("Fehler") else stack = stack.pop() // Wenn es eine schließende Klammer ist, entferne die oberste Klammer vom Stack, wenn sie eine öffnende Klammer ist
      case _   => throw new Exception("Fehler") // Wenn es kein Zeichen ist, das in exp vorkommen darf, wirf eine Exception
  stack.isEmpty // Wenn der Stack leer ist, sind alle Klammern korrekt geschachtelt, sonst nicht

def testAufgabe1b(): Unit =
  // Gültige Eingaben (Aus der Aufgabe)
  println("Für das erste Beispiel aus der Aufgabe: ([{}])(){{()[]}}, kommt: " + isCorrect("([{}])(){{()[]}}") + " raus")
  // Ungültige Eingaben (Aus der Aufgabe)
  println("Für das zweite Beispiel aus der Aufgabe: ([)] kommt: " + isCorrect("([)]") + " raus")

// Aufgabe 1c

// Erweiterung mit Linked Nodes
class LinkedNodesStack[A] extends MyStack[A]:
  class Node(val item: A, val next: Node)
  var topNode: Node = null
  var _size: Int = 0
  def push(item: A): MyStack[A] =
    topNode = new Node(item, topNode)
    _size = _size + 1
    this
  def pop(): MyStack[A] =
    if (!isEmpty)
      topNode = topNode.next
      _size = _size - 1
    this
  def top: A =
    if (!isEmpty) topNode.item
    else throw new Exception("Der Stack ist leer")
  def isEmpty: Boolean = topNode == null
  def size: Int = _size
  def multipush(items: List[A]): MyStack[A] =
    var newStack: MyStack[A] = this
    for (item <- items)
      newStack = newStack.push(item)
    newStack

// Erweiterung mit Array statischer Größe
import scala.reflect.ClassTag
class ArrayStack[A: ClassTag](capacity: Int) extends MyStack[A]:
  val array: Array[A] = new Array[A](if (capacity < 1) 1 else capacity)
  var amount: Int = 0
  def push(x: A): MyStack[A] =
    if (amount < array.length)
      array(amount) = x
      amount = amount + 1
    this
  def pop(): MyStack[A] =
    if (!isEmpty)
      amount = amount - 1
      array(amount) = null.asInstanceOf[A]
    this
  def top: A =
    if (!isEmpty) array(amount - 1)
    else throw new Exception("Der Stack ist leer")
  def isEmpty: Boolean = amount == 0
  def size: Int = amount
  def multipush(items: List[A]): MyStack[A] =
    val newArray = new ArrayStack[A](capacity)
    for (item <- items.reverse)
      newArray.push(item)
    newArray

// Aufgabe 1d
class ArrayStackDyn[A: ClassTag](capacity: Int) extends MyStack[A]:
  var array: Array[A] = new Array[A](1)
  var amount: Int = 0
  def resize(): Unit = // Vorraussetzung: keins, Effekt: keins, Ergebnis: Wenn der Stack zu 25% gefüllt ist, wird die Größe des Arrays geviertelt, wenn der Stack voll ist, wird die Größe des Arrays vervierfacht
    val cap: Int = array.length
    if (cap / 4 <= amount && amount < cap) return
    val newArray: Array[A] = new Array[A](if (cap / 4 > amount) cap / 16 else cap * 4)
    for (i <- 0 until amount)
      newArray(i) = array(i)
    array = newArray
  def push(x: A): MyStack[A] =
    array(amount) = x
    amount = amount + 1
    resize()
    this
  def pop(): MyStack[A] =
    if (!isEmpty)
      amount = amount - 1
      array(amount) = null.asInstanceOf[A]
      resize()
    this
  def top: A =
    if (!isEmpty) array(amount - 1)
    else throw new Exception("Stack is empty")
  def isEmpty: Boolean = amount == 0
  def size: Int = amount
  def multipush(items: List[A]): MyStack[A] =
    var newStack: MyStack[A] = this
    for (item <- items)
      newStack = newStack.push(item)
    newStack
