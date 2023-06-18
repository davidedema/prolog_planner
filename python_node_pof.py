from pyswip import Prolog
import re

patternRot = "rotate\((\w+),\s(\d+),\s(\d+),\s(\d+),\s(\d+)\)"
patternMov = "move\((\w+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+)\)"
patternLink = "link\((\w+),\s(\w+)\)"

prolog = Prolog()
prolog.consult("block_world.pl")

def muovi(id, x, y, z, xf, yf, zf):
    print("INVIO MESSAGGIO MUOVI A ROBOT")
    print("Block ID: " , id)
    print("Block X: " , x)
    print("Block Y: " , y)
    print("Block Z: " , z)
    print("Block XF: " , xf)
    print("Block YF: " , yf)
    print("Block ZF: " , zf)

def rotate(id, x, y, z, no):
    print("INVIO MESSAGGIO ROTATE A ROBOT")
    print("Block ID: " , id)
    print("Block X: " , x)
    print("Block Y: " , y)
    print("Block Z: " , z)
    print("Block NO: " , no)

def link(id1, id2):
    print("INVIO MESSAGGIO LINK A ROBOT")
    print("Block ID1: " , id1)
    print("Block ID2: " , id2)

def main():
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

                result = list(prolog.query(
                    "once(pillar(" + x + "," + y + "," + z + "," + h + "," + w + "," + d + ", Actions))"))

                if result:
                    i = 1
                    print("--------------------")
                    print("Azioni da eseguire:")
                    for action in result[0]["Actions"]:
                        # print(i, "- " + action)
                        # i += 1
                        if "rotate" in action:
                            match = re.match(patternRot, action)
                            if match:
                                rotate(match.group(1), int(match.group(2)), int(match.group(3)), int(match.group(4)), int(match.group(5)))
                        if "move" in action:
                            match = re.match(patternMov, action)
                            if match:
                                muovi(match.group(1), int(match.group(2)), int(match.group(3)), int(match.group(4)), int(match.group(5)), int(match.group(6)), int(match.group(7)))
                        if "link" in action:
                            match = re.match(patternLink, action)
                            if match:
                                link(match.group(1), match.group(2))
                    print("--------------------")
                else:
                    print("Impossibile creare il pilastro")
            case "3":
                break
            case _:
                print("Scelta non valida")

if __name__ == "__main__":
    main()