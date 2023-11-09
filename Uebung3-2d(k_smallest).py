#2d)
# k_smallest(Liste[int],int) : int
#Voraussetzung: keine
# Ergebnis: Das k- kleinste Element aus der Liste xs wird geliefert
# Effekt: keiner
def k_smallest(xs, k):
    n = len(xs)
 #Wenn Liste nur ein Element lang ist
    if n == 1:
        return xs[0]
 #Wenn das 1_kleinste Element gesucht ist
    elif n != 1 and k == 1:
        for i in range(0,n):
           kleinstes = xs[0]
           if kleinstes > xs[i]:
                kleinstes = xs[i]
                return kleinstes
           else: 
                print("Error")
