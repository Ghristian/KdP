#2e)
#quersumme(int) : int
#Voraussetzung: Die Eingabe ist nicht negativ
#Ergebnis: Die Quersumme der Eingabezahl ist geliefert. Bei negativen Zahlen wird 
    #die Eingabe zurueckgegeben.
#Effekt: Fragt nach Eingabe auf dem Bildschirm und gibt dort auch Quersumme aus.
Eingabe = int(input("Gib deine Zahl ein, ich berechne die Quersumme:  "))

def quersumme(e):
  # Eingabe ist Einstellig
  if e < 10:
    return e
  # Rekursiver Teil
  #Quersumme = Letzte Ziffer von Eingabe + quersumme(Eingabe ohne letzte Ziffer)
  else:
    return e % 10 + quersumme(e // 10)
  
print(quersumme(Eingabe))
'''Tests:
quersumme(0) == 0
quersumme(2) == 2
quersumme(153) == 9
quersumme(-1) == -1
quersumme(-56) == -56
'''
