#Binärsuche
#binary_search(List[U], U) : int
#Vorraussetzung: xs ist absteigend sortiert
#Effekt: printed Position von k in der Liste xs auf den Bildschirm
#Result: Eine Stelle an der k in xs auftaucht 
#ist geliefert.
# Ist k nicht in xs enthalten ist len(xs) geliefert.

def binary_search(xs, k):
    n = len(xs)
    #Hilfsfunktion:----------------------------
    def bin_search(left, right):
        if left>=right: #Rekursionsanker
            return n
        m_pos = (left + right) // 2
        m = xs[m_pos]
        if k == m:
            print(m_pos)
            return m_pos
        elif k < m:
            return bin_search(m_pos, right)
        else: #Wenn k > m:
            
            return bin_search(left, m_pos)
    #-------------------------------------------
    return bin_search(0,n)

a= [289, 244, 243, 130, 128, 64, 42, 32, 16, 8, 4, 2, 1]
binary_search(a,16)