// Checks if input is a leap year or not

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

