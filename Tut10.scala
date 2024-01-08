// Aufgabe 1 

def multi [A:Numeric](n:A,list: List[A]) : List[A] =
     val num = summon[Numeric[A]]
     import num.mkNumericOps
     list match
        case Nil => Nil 
        case (x::xs) => n*x :: multi(n,xs)

// Aufgabe 2 

enum Trafficlight:
    case Rot,Gelb,Grün

def nextState (n:Trafficlight) : Trafficlight =
        import Trafficlight.* 
            n match
                case Rot => Grün
                case Gelb => Rot
                case Grün => Gelb

def messagelight(n:Trafficlight): Unit =
    import Trafficlight.* 
    n match
        case Rot => println("Stehen bleiben")
        case Gelb => println ("Achtung")
        case Grün =>println("Los geht es!")

enum Fahrzeug:
    case AMG (max_speed:"325 km/h", baujahr:"Baujahr 2022", ps:"730")
    case Beamer (max_speed:"310 km/h", baujahr:"Baujahr 2021", ps:"617")
    case Lada (max_speed:"130 km/h",baujahr:"1977", ps:"75")



    