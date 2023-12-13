// Checks if input is a leap year or not


import scala.io.StdIn


@main
def isleapy (n : Long) : String =
	print("Gebe ein Jahr > 0 ein: ")
	val n : Long = StdIn.readLong()
	if n % 400 == 0 then 
			"True" 
	else if n % 4 == 0 && n % 100 == 0 then
			"False"
	else if n % 4 == 0 then
			"True"
	else
			"False"
