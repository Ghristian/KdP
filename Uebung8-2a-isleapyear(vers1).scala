// Checks if input is a leap year or not
// Updated version
import scala.io.StdIn

@main
def isleapy (x : Long) : Boolean =
	print("Gebe ein Jahr > 0 ein: ")
	val n : Long = StdIn.readLong()
	if n % 400 == 0 then 
			true 
	else if n % 4 == 0 && n % 100 == 0 then
			false
	else if n % 4 == 0 then
			true
	else
			false
