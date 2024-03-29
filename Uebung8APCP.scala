//KdP Uebung 8 Alexander Parotsidi und Christian Papenfuss
//Abgabe zum 15.12.2023



// Aufgabe 1a)

// def windchill (t: Double, v: Double) : Unit 
// Voraussetzung: t und v sind nicht leer
// Effekt: Windchill-Temperatur ist angezeigt
// Ergebnis: t und v sind gefragt und wc wird berechnet
/*Tests
windchill(0, 0) = 13.12
windchill(35,25) = 39.069
windchill (-20, 100) = -39.63
*/

import scala.io.StdIn.readLine



def windchill () : Unit =
    val t : Double = readLine ("Geben Sie eine Temperatur an ").toDouble
    val v : Double = readLine ("Geben Sie eine Windgeschwindigkeit an ").toDouble
    val wc = 13.12 + 0.6215 * t + (0.3965 * t - 11.37) * math.pow(v,0.16)
    println("Die Windchill-Temperatur beträgt " + wc)

// Aufgabe 1b)
//def backwards(n: Int): Unit = 
// Voraussetzung: n>=0
// Effekt: Die Rückwärtsdezimalzahl ist angezeigt
// Ergebnis: Es wird eine Dezimalzahl n eingegeben (in Funktion) und mittels arithmethischer Operationen zur Rückwärtsdezimalzahl verwandelt
/* Tests
backwards(1) = 1
backwards(12) = 21
backwards(47142) = 24171
*/

def backwards(n: Int): Int = 
  def step(acc: Int, n: Int): Int =
    n match 
      case n if (n < 0) => throw Exception("Fehler, keine negativen Zahlen!")
      case 0 => acc // Anker
      case n if (n > 0) =>
        val lastnr = n % 10
        val bwnr = acc * 10 + lastnr
        step(bwnr, n / 10)
    
  
  step(0, n)



// Aufgabe 1c)
// def countAsTroll(n: Int): String =
// Vorraussetzung= n>0
// Effekt: Zahl in Troll-Sprache ist geliefert als String
// Ergebnis: Eingabezahl wird in Troll-Sprache mit matching umgewandelt
/*Test
countAsTroll(1)= "One"
countAsTroll(0)= "What is this ??"
countAsTroll(8)= Many-Many
countAsTroll(15)= Many-Many-Many-Three
*/
@main
def countAsTroll(n: Int): String =
  def step(acc: String, n: Int): String =
    n match
      case 0 if n== 0 => "What is this ??"
      case n if n==1 && n%1== 0 => "One"
      case n if n==2 && n%2== 0=> "Two"
      case n if n==3 && n%3== 0 => "Three"
      case 4 => "Many"
      case _ if 5 to 7 contains n => s"Many-${step("", n - 4)}"
      case 8 => "Many-Many"
      case _ if 9 to 11 contains n => s"Many-Many-${step("", n - 8)}"
      case 12 => "Many-Many-Many"
      case _ if 13 to 15 contains n => s"Many-Many-Many-${step("", n - 12)}"
      case 16 => "Lots"
      case _ if 17 to 19  contains n =>  s"Lots-${step("", n - 16)}"
      case 20 => "Lots-Lots"
      case _ if (n<0) => "I can not count with few or less ;(" // Anker
      

  val aacc = ""
  step(aacc, n)

//1d) siehe PDF


//2a)

// Vorraussetzung: jahr >= 0
// Effekt: Keine
// Ergebnis: Es ist gegeben ob das Jahr ein LeapYear/ Schaltjahr  ist.
// Tests:
    /* 
isleapy(1900) = false
isleapy(1904) = true
isleapy(1905) = false
 */
import scala.io.StdIn

@main
def isleapy (n : Long) : Boolean =
	if n % 400 == 0 then 
			true 
	else if n % 4 == 0 && n % 100 == 0 then
			false
	else if n % 4 == 0 then
			true
	else
			false

//2b)
// Vorraussetzung: n >= 0
// Effekt: Keine
// Ergebnis: Die Quersumme von n ist gegeben.
def quersumme(n: Int): Int =
    def quersummeHelper(n: Int, acc: Int): Int =
        if (n == 0) acc // Rekursionsanker
        else quersummeHelper(n / 10, acc + n % 10)
    quersummeHelper(n, 0)
/*  Tests:
 quersumme(0) = 0
 quersumme(111) = 3
 quersumme(47142) = 18
 Beispiel mit n = 47142
    quersummeHelper(47142, 0) n ist erste Zahl, zweite ist gespeicherte Quersumme im Akkumulator
    quersummeHelper(4714,2) 47142 ganzzahlig durch 10 geteilt ist 4714, 0 + 47142 modulu 10 = 0 + 2 = 2
    quersummeHelper(471,6)  4714 ganzzahlig durch 10 geteilt ist 471, 2 + 4714 modulu 10 = 2 + 4 = 6 
    quersummeHelper(47,7)   471 ganzzahlig durch 10 geteilt ist 47, 6 + 471 modulu 10 =  6 + 1 = 7
    quersummeHelper(4, 7 + 7 = 14)47 ganzzahlig durch 10 geteilt ist 4, 6 + 47 modulu 10 =  7 + 7 = 14
    quersummeHelper(0, 14 + 4 = 18)4 ganzzahlig durch 10 geteilt ist 0, 14 + 4 modulu 10 =  14 + 4 = 18
    return acc = 18
*/ 


//2c)
// Eigener Datentyp Uhrzeit 1. Int ist Stunden, Zweiter ist Minuten und 3. Int ist Sekunden
type Uhrzeit = (Int, Int, Int)

//Vorraussetzung: keine
//Effekt: keine
//Ergebnis: überprüft, ob gegebenes Tripel ganzer Zahlen eine gültige Uhrzeit ist
def istUhrzeit (g: Uhrzeit) : Boolean =
    val (stunden,min,sek) = g
    if stunden >= 0 && stunden <=23 &&  min >= 0 && min <= 59 && sek >= 0 && sek <= 59 then
        true
    else
        false
/*Tests:
        istUhrzeit(8,15,0) = true
        istUhrzeit(24,90,90) = false
        istUhrzeit(11,11,00) = true
        */

//Vorraussetzung: gültige Uhrzeit
//Effekt: keine
//Ergebnis: liefert die Uhrzeit eine Sekunde später
def tick (b:Uhrzeit) : Uhrzeit =
     val (stunden,min,sek) = b
      // Berechne die neue Sekunde, Minute und Stunde nach dem Hinzufügen einer Sekunde
     val newSek = (sek + 1) % 60
     val newMin = (min + (sek + 1) / 60) % 60
     val newSt = (stunden + (min + (sek + 1) / 60) / 60) % 24
  // Erstelle ein neues Triplet mit den neuen Werten
     (newSt, newMin, newSek)
/* Tests
    tick(11,11,11) = (11,11,12)
    tick(10,55,59) = (10,56,0)
    tick(23,59,59) = (0,0,0)
    */


//Vorraussetzung: gültige Uhrzeit
//Effekt: keine
//Ergebnis: liefert die Uhrzeit eine Sekunde früher
def kcit  (c:Uhrzeit) : Uhrzeit =
     var (stunden,minuten,sekunden) = c
     if (sekunden > 0) {
        (stunden, minuten, sekunden - 1)
  } else if (minuten > 0) {
        (stunden, minuten - 1, 59)
  } else if (stunden > 0) {
         (stunden - 1, 59, 59)
  } else {
        (0, 0, 0)
  }


/* Tests
    kcit(11,11,11) = (11,11,10)
    kcit(10,55,00) = (10,54,0)
    kcit(13,00,00) = (12,59,59)
    */    





//Vorraussetzung: gültige Uhrzeit
//Effekt: keine
//Ergebnis: liefert die Uhrzeit Input Sekunden später
def addSekunden(stunden: Int, minuten: Int, sekunden: Int, hinzufuegen: Int): (Int, Int, Int) = {
  val gesamtSekunden = stunden * 3600 + minuten * 60 + sekunden + hinzufuegen
  (gesamtSekunden / 3600, (gesamtSekunden % 3600) / 60, gesamtSekunden % 60)
}
/*Tests
addSekunden(11,11,11,50) = (11,12,1)
addSekunden(11,59,50,30) = (12,0,20)
*/


//Vorraussetzung: gültige Uhrzeit
//Effekt: keine
//Ergebnis: liefert die Uhrzeit Input Minuten später
def addMinuten(stunden: Int, minuten: Int, sekunden: Int, hinzufuegen: Int): (Int, Int, Int) = {
  val gesamtMinuten = stunden * 60 + minuten + hinzufuegen
  (gesamtMinuten / 60, gesamtMinuten % 60, sekunden)
}

/*Tests
addMinuten(1,40,33,35) = (2,15,33)
addSekunden(11,59,50,30) = (12,29,50)
*/


//Vorraussetzung: gültige Uhrzeit, Input + stunden<= 24
//Effekt: keine
//Ergebnis: liefert die Uhrzeit Input Stunden später
def addStunden(stunden: Int, minuten: Int, sekunden: Int, hinzufuegen: Int): (Int, Int, Int) = {
  (stunden + hinzufuegen, minuten, sekunden)
}

/*Tests
addStunden(1,0,0,3) = (4,0,0)
addStunden(14,50,50,2) = (16,50,50)
*/

//2d) siehe PDF
