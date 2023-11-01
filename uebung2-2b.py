#2b
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