print ("Geben Sie das Gewicht des Pakets an")

gewicht= float(input("Das Paket wiegt...)"))

if gewicht <= 2 : 
    print ("Der Versand kostet 5 Euro")

elif gewicht > 2 and gewicht <= 5 : 
    print ("Der Versand kostet 7 Euro")
    
elif gewicht > 5 and gewicht <= 10 :
    print ("Der Versand kostet 10 Euro")

elif gewicht > 10 :
    print ("Der Versand kostet 15 Euro")

else :
    print ("Eingabe inkorrekt")