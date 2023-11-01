#1c)
print("Wilkommen zum Histogramm Programm.")
Eingabe = str(input("Gib deinen Text ein: "))
Histogramm = {}

for Symbol in Eingabe:
  # Wenn das Symbol schon im Dictionary ist, +1
  if Symbol in Histogramm:
    Histogramm[Symbol] =Histogramm[Symbol] + 1
  # Wenn Symbol noch nicht im Dictionary ist füge es ein mit Zahl 1
  else:
    Histogramm[Symbol] = 1

print(Histogramm)