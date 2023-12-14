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
