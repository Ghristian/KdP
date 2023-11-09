#2c)
# dna2rna1(Liste[char]) : Liste[char]
#Voraussetzung: DNA enthaelt nur die Nukleotide = ["A","T","C","G"]
# Ergebnis: Die RNA Sequenz der DNA Sequenz wird gefliefert.
# Effekt: Gibt "DNA darf nur A,T,C und G enthalten." auf Bildschirm aus, wenn
# DNA andere Symbole beinhaltet.
def dna2rna1(DNA):
  Nukleotide = ["A","T","C","G"]
  # leere Liste für RNA Sequenz
  RNA = []
  # Gehe jedes Element der DNA-Sequenz durch
  for Base in DNA:
    # Wenn BAse T ist packe U in RNA
    if Base == "T":
      RNA.append("U")
    # Sonst Base hinzufuegen
    else:
      if Base not in Nukleotide:
         print("DNA darf nur A,T,C und G enthalten.")
      else:
        RNA.append(Base)
  # Gib die RNA-Sequenz als Rückgabewert zurück
  return RNA


# dna2rna2(Liste[char]) : Liste[char]
#Voraussetzung: keine
# Ergebnis: Die DNA Sequenz wird in RNA umgewandelt.
# Effekt: keiner

def dna2rna2(DNA):
  n = len(DNA)
  for i in range(0,n):
    # Wenn das Element T ist, ersetze es mit U in der DNA-Sequenz
    if DNA[i] == "T":
      DNA[i] = "U"
  # Gib keinen Ausgabewert zurück
  return None
