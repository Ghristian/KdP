object ArtistRanking:

     private var ranking: Array[String] = new  Array[String](5)

     def getRank(position: Int, artist: String): Unit = 
        if (position >= 1 && position <= 5) 
            ranking(position - 1) = artist
        else 
            throw new IllegalArgumentException("Invalid position. Position must be between 1 and 5.")
        
    

     def changeArtist(position: Int): String = 
        if (position >= 1 && position <= 5) 
            ranking(position - 1)
        else 
            throw new IllegalArgumentException("Invalid position. Position must be between 1 and 5.")
        
    

     def wholeRanking(): Array[String] = 
        ranking
    
// Aufgabe 2

class Buch (val titel: String, val autor: String, val erscheinungsjahr: Int) :

     private var ausgeliehen: Boolean = false
     private var ausleihCount: Int = 0
    

     def bookInfo(): Unit = 
        val status = if (ausgeliehen) "ausgeliehen" else "verfügbar"
        println(s"Titel: $titel")
        println(s"Autor: $autor")
        println(s"Erscheinungsjahr: $erscheinungsjahr")
        println(s"Status: $status")
        println(s"Ausleihen: $ausleihCount")
    

     def read(seitenanzahl: Int): Unit = 
        println(s"Es wurde 1 Kapitel mit $seitenanzahl Seiten gelesen.")
    

     def ausleihen(): Unit = 
        if (ausgeliehen) 
            println("Das Buch ist bereits ausgeliehen.")
        else 
            ausgeliehen = true
            ausleihCount += 1
            println("Das Buch wurde ausgeliehen.")
        
    

     def zurueckgeben(): Unit = 
        if (ausgeliehen) 
            ausgeliehen = false
            println("Das Buch wurde zurückgegeben.")
        else 
              println("Das Buch ist bereits verfügbar.")

object Buch:
     val x = new Buch ("Harry Potter", "J.K. Rowling", 1997)
     val y = new Buch ("Harry Potter 2 ", "J.K. Rowling", 1998)
     val z = new Buch ("Harry Potter 3", "J.K. Rowling", 1999)
    

class WissenschaftsBuch(titel: String, autor: String, erscheinungsjahr: Int, val isbn: String) extends Buch(titel, autor, erscheinungsjahr):
     private var kopien: Int = 0

     def createCopy(): Unit =
        if (kopien < 1000)
            kopien += 1
            println("Eine Kopie wurde erstellt.")
        else
            println("Es können keine weiteren Kopien erstellt werden.")

object Lehninger: 
    val len = new WissenschaftsBuch ("Lehninger Biochemie", "Nelson, Cox", 2017, "978-3-527-34233-0")



def createCopiesInLoop(): Unit = {
        for (_ <- 1 to 1000) {
            Lehninger.len.createCopy()
        }
    }

// Aufgabe 3

/* 
Eine sinnvolle Hierarchie ist:

Lebewesen:
    - Pflanze
    - Säugetier
        - Hund
        - Katze

1. geht 
2. geht 
3. geht nicht
4. geht nicht
 */
