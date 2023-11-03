#KdP Uebung 2 Alexander Parotsidi und Christian Papenfuss
#Abgabe zum 03.11.2023

#1a)
#Siehe PDF

#1b) 
#Fuer Text siehe PDF
x = 1.0
print(type(x))
while x <= 1.5 :
    x = x + 0.1
    print(x)
# Schon nach der ersten Addition kommt es zu Fehlern da 1.2000000000000002 und 1.2 im Rechner gleich dargestellt werden.

#1c)

print("Wilkommen zum Histogramm Programm!")
Eingabe = str(input("Gib deinen text ein:  "))
Histogramm = {}

for Symbol in Eingabe:
    #Wenn Symbol im Dictionary ist dann Addiere 1
    if Symbol in Histogramm:
        Histogramm[Symbol] = Histogramm[Symbol] + 1
    #Ansonsten fuege Symbol hinzu mit der Anzahl 1
    else:
        Histogramm[Symbol] = 1

print(Histogramm)


#2a)

import random
#Liste der 4 nukleotidbasen
Nukleotidbasen = ["A","C","T","G"]

# Leere Liste in die später zufallssequenz kommt:
Zufallssequenz = []

# Packe jeweils 6 einzelne Nukleotidbasen in die Liste
for i in range(6):
    Zufallssequenz.append(random.choice(Nukleotidbasen))

#Jetzt muessen die einzelnen durch " " getrennten Basen zusammengefuegt werden:
Zufallssequenz = "".join(Zufallssequenz)
print(Zufallssequenz)

#2b)

print("Wilkommen DNA Sequenz Vergleichs Programm.")
print("Bitte gib 2 Sequenzen in der Form 'CCTGAC'.")
DNA1 = str(input("Gib deine erste (6-stellige) DNA Sequenz ein: "))
DNA2 = str(input("Gib deine zweite (6-stellige) DNA Sequenz ein: "))
Nukleotide = ["A", "C", "G", "T"]
FalschesSymbol1=0
FalschesSymbol2=0
try:
    for i in range(6):
        if DNA1[i] not in Nukleotide:
            FalschesSymbol1 = FalschesSymbol1 + 1
            if i ==5:
                print("Die erste Sequenz enthaelt",FalschesSymbol1,"  unerlaubte Symbole.")

        elif DNA2[i] not in Nukleotide:
            FalschesSymbol2 = FalschesSymbol2 + 1
            if i ==5:
                print("Die zweite Sequenz enthält ",FalschesSymbol2," unerlaubte Symbole.")
    
    if len(DNA1) == 6 and len(DNA2) == 6:
        Uebereinstimmungen=0
        for i in range(6):
            if DNA1[i] == DNA2[i]:
             Uebereinstimmungen = Uebereinstimmungen + 1 
        print("Die Sequenzen",DNA1,"und die Sequenz",DNA2,"haben",Uebereinstimmungen,"Uebereinstimmung(en).")  

    elif len(DNA1)<=5:
        print("Die erste Sequenz ist zu kurz.")

    elif len(DNA2)<=5:
         print("Die zweite Sequenz ist zu kurz.")

    elif len(DNA1)>=7:
         print("Die erste Sequenz ist zu lang.")

    elif len(DNA2)>=7:
         print("Die zweite Sequenz ist zu lang.")
   
except:
  print("Ein unbekannterFehler ist aufgetreten")


#2c)

print("Wilkommen DNA Sequenz Vergleichs Programm.")
print("Bitte gib 2 Sequenzen in der Form 'CCTGAC'.")
DNA1 = str(input("Gib deine erste (6-stellige) DNA Sequenz ein: "))
DNA2 = str(input("Gib deine zweite (6-stellige) DNA Sequenz ein: "))
Nukleotide = ["A", "C", "G", "T"]
Dictionary1 = {}
Dictionary2 = {}
Gleichnisse=0
try:

    if len(DNA1) == 6 and len(DNA2) == 6  :
        for i in range(6):
            if DNA1[i] not in Nukleotide or DNA2[i] not in Nukleotide:
                print("Eine Sequenz enthält ein unerlaubtes Symbol")
                break
        #Buchstaben zum jeweiligen Dictionary hinzufügen oder 1 addieren:
        for char in DNA1:
            if char in Dictionary1:
                Dictionary1[char] =Dictionary1[char] + 1
            else:
                Dictionary1[char] = 1
    
        for char in DNA2:
            if char in Dictionary2:
                Dictionary2[char] =Dictionary2[char] + 1
            else:
                 Dictionary2[char] = 1
        #Vergleich der Anzahlen zwischen den Dictionarys
        for key in Dictionary1:
            if key in Dictionary2:
                if Dictionary1[key] == Dictionary2[key]:


                     Gleichnisse += 1

       
        print(Gleichnisse)         
    elif len(DNA1)<=5:
        print("Die erste Sequenz ist zu kurz.")

    elif len(DNA2)<=5:
         print("Die zweite Sequenz ist zu kurz.")

    elif len(DNA1)>=7:
         print("Die erste Sequenz ist zu lang.")

    elif len(DNA2)>=7:
         print("Die zweite Sequenz ist zu lang.")
   
except:
  print("Ein unbekannterFehler ist aufgetreten")


#2d)

#Random 6 Stellen Basenstrang erzeugen:
import random
Nukleotidbasen = ["A","C","T","G"]
Gleichnisse = 0
Zufallssequenz = []
for i in range(6):
    Zufallssequenz.append(random.choice(Nukleotidbasen))
Zufallssequenz = "".join(Zufallssequenz)
print(Zufallssequenz)

print("Wilkommen beim DNA-Raten.")
print("Ich denke mir eine 6 stellige DNA Sequenz aus.")
Raten = str(input("Gib eine (6-stellige) DNA Sequenz ein: "))
runden=1
try: #Schleife 
    while True:
        Gleichnisse=0
        DictEingabe={}
        DictRandSeq={}
        Keinfehlerfund= True
        if len(Raten)<6:
            print("Eingabe zu kurz.")
        if len(Raten)>6:
            print("Eingabe zu lang.")
        for i in range(6):
            if Raten[i] not in Nukleotidbasen:
                Keinfehlerfund = False 
        if not Keinfehlerfund: #Printed sobald ein Fehler gefunden wurde den Fehlertext aus
            print("Eingabe enthaelt unerlaubtes Symbol!")
            print("Eingabe darf nur A,C,G und T enthalten.")
        else:       
         if Raten != Zufallssequenz:
            print("Nicht korrekt!")
#Dictionary mit Anzahl und Vorkommen aller Basen in der Eingabe auffuellen
            for char in Raten:
               if char in DictEingabe:
                DictEingabe[char] =DictEingabe[char] + 1
            else:
                DictEingabe[char] = 1
#Dictionary mit Anzahl und Vorkommen aller Basen in der Zufallssequenz auffuellen
            for char in Zufallssequenz:
               if char in DictRandSeq:
                DictRandSeq[char] =DictRandSeq[char] + 1
            else:
                DictRandSeq[char] = 1
#Anzahl gleicher Haufigkeiten berechnen und dann printen
            for key in DictEingabe:
                if key in DictRandSeq:
                    if DictEingabe[key] == DictRandSeq[key]:
                     Gleichnisse += 1

                print("Anzahl gleicher Haufigkeiten betraegt: ",Gleichnisse)
            # Vergleichen ob es Uebereinstimmungen zwischen Raten und Zufallssequenz gibt und diese dann printen            
            for i in range(6)  :  
                if Zufallssequenz[i] == Raten[i]:
                    
                    print("Die ", i,". Position ist Richtig")

         
            Raten = str(input("Gib eine (6-stellige) DNA Sequenz ein: "))
            runden+= 1
        
# Dieser Code wird ausgefuehrt wenn Raten == Zufallssequenz ist:
         else:
            print("Richtige Sequenz",Zufallssequenz, "erraten.")
            print("Du hast ", runden, " Runden gebraucht.")
            break
except:
    print("Fehler")


