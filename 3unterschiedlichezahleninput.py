
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
