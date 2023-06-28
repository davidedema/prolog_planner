class Block:
    def __init__(self, ID, X, Y, Z, W, H, D, O, TL, TH, S, MB, L):
        self.ID = ID
        self.X = X
        self.Y = Y
        self.Z = Z
        self.W = W
        self.H = H
        self.D = D
        self.O = O
        self.TL = TL
        self.TH = TH
        self.S = S
        self.MB = MB
        self.L = L

    def __str__(self):
        return f"Block(ID: {self.ID}, X: {self.X}, Y: {self.Y}, Z: {self.Z}, W: {self.W}, H: {self.H}, D: {self.D}, O: {self.O}, TL: {self.TL}, TH: {self.TH}, S: {self.S}, MB: {self.MB}, L: {self.L})"
