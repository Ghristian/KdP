#Uebung 2

#1b) 
 #Beispiel fuer Rundungfehler bei floats
x = 1.0
print(type(x))
while x <= 1.5:
  x = x + 0.1
  print (x)
  #Nach erster Adiition kommt es zu einem Fehler da 1.2000000000000002 und 1.2 mit der selben Approximation dargestellt werden.


