from pyswip import Prolog
from block import Block
import tkinter as tk
import tkinter.ttk as ttk
import tkinter.messagebox as messagebox
import re

patternRot = "rotate\((\w+),\s(\d+),\s(\d+),\s(\d+),\s(\d+)\)"
patternMov = "move\((\w+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+)\)"
patternLink = "link\((\w+),\s(\w+)\)"
patternGet = "block\((\w+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\d+),\s(\w+),\s(\w+),\s(\w+),\s\[([^\]]+)\],\s(\d+)\)"

prolog = Prolog()
prolog.consult("block_world.pl")

blocks = []

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

def drawCanvas(canvas):
    for block in blocks:
        rect = canvas.create_rectangle(block.X * 50, block.Y * 50, (block.X + block.W) * 50, (block.Y + block.D) * 50, fill="red")
        canvas.create_text(block.X * 50 + 25, block.Y * 50 + 25, text=block.ID, font=("Arial", 20))
        canvas.itemconfig(rect, tags=("block", block.ID))
        canvas.tag_bind(rect, "<Button-1>", on_enter)
def show_block_info(block):
    # Mostra le informazioni del blocco in una finestra di dialogo
    messagebox.showinfo("Informazioni Blocco", f"ID: {block.ID}\nX: {block.X}\nY: {block.Y}\nZ: {block.Z}\nW: {block.W}\nH: {block.H}\nD: {block.D}\nO: {block.O}\nTL: {block.TL}\nTH: {block.TH}\nS: {block.S}\nMB: {block.MB}\nL: {block.L}")

def on_enter(event):
    tag = event.widget.gettags("current")[1]
    block = next((x for x in blocks if x.ID == tag), None)
    show_block_info(block)

def main(): 
    while True:
        print("--------------------")
        select = input("1) Mostrare blocchi\n2) Creare pilastro\n3) Uscire\n")
        print("--------------------")
        match select:
            case "1":
                list.clear(blocks)
                result = list(prolog.query("get_blocks(Blocks)"))
                for block in result[0]["Blocks"]:
                    match = re.match(patternGet, block)
                    if match:
                        blocks.append(Block(match.group(1), int(match.group(2)), int(match.group(3)), int(match.group(4)), int(match.group(5)), int(match.group(6)), int(match.group(7)), int(match.group(8)), match.group(9), match.group(10), match.group(11), match.group(12), int(match.group(13))))

                for block in blocks:
                    print(block)      
                window = tk.Tk()
                window.title("Block World")
                canvas = tk.Canvas(window, width=1000, height=1000)
                canvas.pack()
                drawCanvas(canvas)
                window.mainloop()        
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