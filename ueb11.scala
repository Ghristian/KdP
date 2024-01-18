//KdP Uebung 8 Alexander Parotsidi und Christian Papenfuss
//Abgabe zum 19.11.2024
//Tutor: Adrian-Maurice Alex


//1a und 1b in der PDF

//1c)
import scala.math.Pi

class Geom3D (val name: String) :
// Standardimplementierungen in Geom3D
    def volume(): Double = 0.0
    def surfaceArea(): Double = 0.0

class Wuerfel(seitenlaenge : Double) extends Geom3D ("Wuerfel"):
    // Voraussetzung: Wert zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: Volumen fuer Wuerfel ist berechnet
    override def volume() : Double =
        val v : Double = seitenlaenge*seitenlaenge*seitenlaenge
        v
    // Voraussetzung: Wert zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: SurfaceArea ist berechnet und ausgegeben
    override def surfaceArea() : Double =
        val sa : Double =  seitenlaenge*seitenlaenge*4
        sa

class Quader(l : Double, b : Double, h : Double) extends Geom3D ("Quader"):
    // Voraussetzung:  Werte zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: Volumen fuer Quader ist berechnet 
    override def volume() : Double =
        val v : Double = l * b * h
        v
    // Voraussetzung:  Werte zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: SurfaceArea fuer Quader ist berechnet  
    override def surfaceArea() : Double =
        val sa : Double =  2 * (l * b + l * h + b * h) 
        sa

class Kugel(radius: Double) extends Geom3D ("Kuegel"):
    // Voraussetzung:  Wert zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: Volumen fuer Kugel ist berechnet
    override def volume() : Double =
        val v : Double = (4.0 / 3.0) * Pi * radius * radius * radius
        v
    // Voraussetzung: Wert zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: SurfaceArea fuer Kugel ist berechnet 
    override def surfaceArea() : Double =
        val sa : Double = 4 * Pi * radius * radius
        sa

class Tetraeder(l : Double) extends Geom3D ("Tetraeder"):
    // Voraussetzung: Wert zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: Volumen fuer Tetraeder ist berechnet
    override def volume() : Double = 
        val v : Double = (math.sqrt(2) / 12) * l * l * l
        v
    // Voraussetzung: Wert zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: /
    // Ergebnis: SurfaceArea fuer Kugel ist berechnet 
    override def surfaceArea() : Double = 
        val sa : Double = l * l * math.sqrt(3)
        sa


    // Voraussetzung:  Werte zwischen 4.94065645841246544e-324 und 1.79769313486231570e+308
    // Effekt: Gesamtoberflaeche und Gesamtvolumen ist ausgegeben
    // Ergebnis: Gesamtoberflaeche und Gesamtvolumen ist berechnet 
@main 
def geom3Demo(): Unit =
    val geometricBodies = Array(
      new Wuerfel(seitenlaenge = 2),
      new Quader(l = 3, b = 4, h = 5),
      new Kugel(radius = 1.1),
      new Tetraeder(l = 3.5)
    )

    val totalVolume = geometricBodies.map(_.volume()).sum
    val totalSurfaceArea = geometricBodies.map(_.surfaceArea()).sum

    println(s"Total volume: $totalVolume")
    println(s"Total Surface Area: $totalSurfaceArea")
/* Test:
geom3Demo()
Total volume: 85.31914745649352
Total Surface Area: 153.86279115042802
*/



// Aufgabe 2a

/* 
Durch das Importieren der Bibliothek scala.io.StdIn 
und der Verwendung der Methode Stdln.readLine() kann der User Etwas eingeben, dass mit dem
value userInput gespeichert.
*/

import scala.io.StdIn

def read(): Unit = 
  println("Ihre Eingabe bitte:")
  val userInput = StdIn.readLine()
  println("Sie haben folgendes eingegeben: " + userInput)





/*
Definition: nrgame() : Unit = 
Vorraussetzung: keine, da keine Parameter übergeben werden.
Effekt: Texte sind ausgegeben, Spieler wird aufgefordert eine Zahl einzugeben.
Ergebnis: Spiel, bei dem der Spieler eine Zahl erraten muss, es sind Hinweise gegeben, wie groß oder klein die Zahl ist, die der Spieler eingegeben hat.
          Am Ende wird die Anzahl der Versuche ausgegeben.
*/ 

import scala.io.StdIn
import scala.util.Random

def nrgame() : Unit = // Keine Pameter notwendig
  val number = Random.nextInt(100) // Zufallszahl zwischen 1 und 100
  var guess = 0                    // Variable für die Eingabe des Spielers
  var tries = 0                    // Variable für die Anzahl der Versuche
  while (guess != number) do       // Solange die Zahl nicht erraten wurde
    println("Bitte geben Sie eine Zahl zwischen 1 und 100 ein:") // Aufforderung zur Eingabe
    guess = StdIn.readInt()                                      // Eingabe des Spielers
    tries += 1                                                   // Anzahl der Versuche wird erhöht
    if guess < number then                                       // Ausgabe von Hinweis wenn die Zahl zu klein ist
      println("Die gesuchte Zahl ist größer als " + guess)
    else if guess > number then                                  // Ausgabe von Hinweis wenn die Zahl zu groß ist
      println("Die gesuchte Zahl ist kleiner als " + guess)
    else if guess == number then                                 // Ausgabe wenn die Zahl erraten wurde 
      println("Sie haben die Zahl " + number + " erraten!")
      println("Sie haben " + tries + " Versuche benötigt.")
    else  
      throw Exception("Fehler")                                  // Fehlermeldung wenn etwas schief läuft


// Aufgabe 2b,c,d

class Fahrzeug(private var baujahr: Int, private var farbe: String, private var ps: Int, private var preis: Int):
  private var nrSchild : Int = Fahrzeug.nrSchildc
  Fahrzeug.nrSchildc = Fahrzeug.nrSchildc + 1

   def lackieren(farbe: String): Unit =
    this.farbe = farbe 
   def tuning(ps: Int): Unit = // Vorraussetzung: PS müssen als Int übergeben werden, Effekt: PS des Autos werden erhöht, Ergebnis: PS des Autos sind erhöht
    this.ps += ps 
   def reparieren(preis: Int): Unit = // Vorraussetzung: Preis muss als String übergeben werden, Effekt: Preis des Autos wird geändert, Ergebnis: Preis des Autos ist geändert
    this.preis = preis
   def getps(): Int = ps // Vorraussetzung: keine, Effekt: keine, Ergebnis: PS des Autos werden ausgegeben
   def getfarbe(): String = farbe // Vorraussetzung: keine, Effekt: keine, Ergebnis: Farbe des Autos wird ausgegeben
   def getbaujahr(): Int = baujahr   // Vorraussetzung: keine, Effekt: keine, Ergebnis: Baujahr des Autos wird ausgegeben  
   def getpreis(): Int = preis // Vorraussetzung: keine, Effekt: keine, Ergebnis: Nummernschild des Autos wird ausgegeben
   def fahrzeugInfo(): Unit =  // Vorraussetzung: keine, Effekt: keine, Ergebnis: Informationen über das Auto werden ausgegeben
    println("Baujahr: " + baujahr)
    println("Farbe: " + farbe)
    println("PS: " + ps)
    println("Preis: " + preis +"€")
   def getnr() : Int = nrSchild // Vorraussetzung: keine, Effekt: keine, Ergebnis: Nummernschild des Autos wird ausgegeben.

class PKW(baujahr: Int, farbe: String, ps: Int, preis: Int, private var sitze: Int) extends Fahrzeug(baujahr, farbe, ps, preis):
  def getSitze(): Int = sitze
  override def fahrzeugInfo(): Unit = 
  println("Baujahr: " + baujahr)
  println("Farbe: " + farbe)
  println("PS: " + ps)
  println("Sitze: " + sitze)
  println("Preis: " + preis + "€" )

class Motorrad(baujahr: Int, farbe: String, ps: Int, preis: Int, private var hubraum: Int) extends Fahrzeug(baujahr, farbe, ps, preis):
  def getHubraum(): Int = hubraum // Vorraussetzung: keine, Effekt: keine, Ergebnis: Hubraum des Autos wird ausgegeben
  override def fahrzeugInfo(): Unit = // Vorraussetzung: keine, Effekt: keine, Ergebnis: Informationen über das Auto werden ausgegeben
  println("Baujahr: " + baujahr)
  println("Farbe: " + farbe)
  println("PS: " + ps)
  println("Hubraum: " + hubraum)
  println("Preis: " + preis + "€" )

class LKW(baujahr: Int, farbe: String, ps: Int, preis: Int, private var ladeflaeche: Double) extends Fahrzeug(baujahr, farbe, ps, preis):
  def getLadeflaeche(): Double = ladeflaeche // Vorraussetzung: keine, Effekt: keine, Ergebnis: Ladefläche des Autos wird ausgegeben
  override def fahrzeugInfo(): Unit = // Vorraussetzung: keine, Effekt: keine, Ergebnis: Informationen über das Auto werden ausgegeben
  println("Baujahr: " + baujahr)
  println("Farbe: " + farbe)
  println("PS: " + ps)
  println("Ladefläche: " + ladeflaeche)
  println("Preis: " + preis + "€" )


object Fahrzeug:
   private var nrSchildc: Int = 100
   
// Um von den Exeplaren die Attribute zu checken muss man: Objectname.Variabelname.funktionsname() eingeben 
object BMW: // Zur Referenzsemantik: Hier wird diese verdeutlicht, da die Variabeln Referenzen auf dem Objekt speichern, anstatt an das Objekt selbst. 
    val bmw = new Fahrzeug(2019, "Blau", 200,100000)
  
object Audi:
    val audi = new Fahrzeug(2020, "Silber", 180,50000)

object Opel:
    val opel = new Fahrzeug(2018, "Rot", 250, 10000)

object Mercedes:
    val mercedes = new Fahrzeug(2017, "Schwarz", 300,20000)
