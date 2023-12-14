//KdP Uebung 8 Alexander Parotsidi und Christian Papenfuss
//Abgabe zum 15.12.2023





//2a)

// Vorraussetzung: jahr >= 0
// Effekt: Keine
// Ergebnis: Es ist gegeben ob das Jahr ein LeapYear/ Schaltjahr  ist.
// Testfälle:
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

