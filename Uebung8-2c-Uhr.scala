
// Eigener Datentyp Uhrzeit 1. Int ist Stunden, Zweiter ist Minuten und 3. Int ist Sekunden
type Uhrzeit = (Int, Int, Int)
val z : Uhrzeit = (8, 15, 0) // Startzeit der Vorlesung


// überprüft, ob gegebenes Tripel ganzer Zahlen eine gültige Uhrzeit ist
def istUhrzeit (Uhrzeit) : Boolean =

//bekommt eine Uhrzeit und liefert die Uhrzeit eine Sekunde später
def tick (Uhrzeit) : Uhrzeit =

//Umkehrfunktion von tick
def kcit  (Uhrzeit) : Uhrzeit =


/*Die Funktionen addSekunden, addMinuten und addStunden bekommen eine
gültige Uhrzeit und eine ganze Zahl und addieren diese ganze Zahl auf
die Sekunden, Minuten bzw. Sekunden*/

def addSekunden (Uhrzeit:Int) : Uhrzeit =

def addMinuten (Uhrzeit:Int) : Uhrzeit =

def addStunden (Uhrzeit:Int) : Uhrzeit =
