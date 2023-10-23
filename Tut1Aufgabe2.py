print ("Gib deine Zahlen mit , ein.")

zahla = int(input("Gib die erste Zahl ein: "))
zahlb = int(input("Gib die zweite Zahl ein: "))
zahlc = int(input("Gib die dritte Zahl ein: "))

list = [zahla, zahlb, zahlc]
list.sort(reverse=True)
for x in list[:1]:

  print (x)
