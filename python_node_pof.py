from pyswip import Prolog
prolog = Prolog()
prolog.consult("block_world.pl")
while True:
    select = input("1) Mostrare blocchi\n2) Creare pilastro\n3) Uscire\n")
    match select:
        case "1":
            result = list(prolog.query("listing(block/13)"))
        case "2":
            print("Inserisci le coordinate del pilastro:")
            x = input("x: ")
            y = input("y: ")
            z = input("z: ")
            h = input("Inserisci l'altezza del pilastro: ")
            w = input("Inserisci la larghezza del pilastro: ")
            d = input("Inserisci la profondità del pilastro: ")

            result = list(prolog.query("once(pillar(" + x + "," + y + "," + z + "," + h + "," + w + "," + d + ", Actions))"))
                        
            if result:
                i = 1
                print("--------------------")
                print("Azioni da eseguire:")
                for action in result[0]["Actions"]:
                    print(i, "- " + action)
                    i += 1
                print("--------------------")
            else:
                print("Impossibile creare il pilastro")
        case "3":
            break
        case _:
            print("Scelta non valida")
