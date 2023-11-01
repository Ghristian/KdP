#Uebung 1 Abgabe am 27.10.23

#Alex und Christian Papenfuss




#Aufgabe 1

#1a)

print ("Gib deine Grundseitenlaenge und Dreickshoehe ein, ")
print ("vergiss nicht nach jeder Zahl Enter zu druecken.")

grundseite = int(input("Gib zuerst die Länge der Grundseite ein: "))
hoehe = int(input("Gib jetzt die Dreieckshoehe ein: "))

A = (grundseite * hoehe) / 2

print (A)

#1b)




#1c




#1d




#1e



#Aufgabe 2

#2a)


print("Wilkommen im Python Kino!")
print ("Bitte geben Sie ihr Alter ein")

age=int(input("Alter:"))

if age < 12 : 
    print ("Ihr Ticket kostet 10 Euro")

elif age >= 12 and age < 18 : 
    print ("Ihr Ticket kostet 12 Euro")
    
elif age >= 18 and age < 65  :
    print ("Ihr Ticket kostet 14 Euro")

elif age >= 65 :
    print ("Ihr Ticket kostet 12 Euro")

else :
    print ("Fehler, bitte Programm neu laden")


#2b)

#Mann kann if (fall1):
#              do something1
#          elif (fall2):
#               do something2
#           else:
#                do another thing
# auch nur mit if Statement schreiben und zwar so:
#           if (fall1):
#               do something1
#           if fall1=False and fall2 :
#               do something2
#           if  fall1=False and fall2=False :
#               do another thing


#2c)




#2d)


print ("Gib deine Zahlen ein und drücke nach jeder Zahl Enter.")

zahla = int(input("Gib die erste Zahl ein: "))
zahlb = int(input("Gib die zweite Zahl ein: "))
zahlc = int(input("Gib die dritte Zahl ein: "))

if zahla == zahlb and zahlb == zahlc :
   print(" 0 unterschiedliche Zahlen eingeben.")
elif zahla == zahlb or zahlb == zahlc or zahla == zahlc :
   print ("2 gleiche Zahlen und 1 unterschiedliche Zahl eingegeben.")
else :
    print("3 unterschiedliche Zahlen eingegeben.")

#2e)
