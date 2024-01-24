//Stack in Form eines dynamischen Arrays

import scala.reflect.ClassTag

//Voraussetzung: /
// Effekt: /
// Ergebnis: Leerer Stack ist gegeben
class DynamicArrayStack[A: ClassTag] :
    //Header:
    private  var array : Array[A] = new Array[A] (1)
    private var amount : Int = 0

    //Voraussetzung: /
    // Effekt: /
    // Ergebnis: (array.lenght/4 <= amount < array.length) wird erfüllt
    private def resize () : Unit =
        val cap : Int = array.length 
        if cap / 4 <= amount && amount < cap then return // Invariante 

        //alloziert einen neuen Array mit halber oder doppelter Groesse des alten Arrays:
        val newArray : Array[A]  = new Array[A] (if cap/4 > amount then cap / 2 else cap*2)

        //Kopiert alle Elemente ins neueArray:
        for i <- 0 to amount-1 do 
            newArray(i) = array(i)
        array = newArray // Altes Array wird entfernt neues Array heißt wie altes Array

    def push(x : A ) : Unit = 
        array ( amount ) = x
        amount = amount + 1
        resize()

    def pop() : A = 
        if !isEmpty then 
            val result : A = array(amount - 1)
            array (amount -1 ) = null.asInstanceOf[A]
            amount = amount - 1
            resize()
            result
        else throw new Exception ("Stack ist leer")

    def top : A =
        if !isEmpty then array(amount - 1)
        else throw new Exception (" Stack ist leer")

    def isEmpty : Boolean = amount == 0
    def size : Int = amount
