import torch.nn as nn

# definizione cnn per classificazione cifre MNIST
# rete consiste in due parti princilapi:
# encoder: sequenza di 2 strati convoluzionali:
# 1) 1 canale in input, 6 canali in output, kernel 5x5
# 2) 6 canali in input, 16 canali in output, kernel 5x5
# Ogni strato viene seguito da un maxpooling 2x2 -> kernel 2x2, stride 2 quindi un solo valore per regione 2x2
# E infine da una ReLU -> funzione di attivazione, prende in input un valore e restituisce 0 se negativo, il valore stesso se positivo.
# serve per introdurre non linearità nella rete
# classifier: 3 strati fully connected con ReLU e Softmax
# nn.Linear mappa input di dimensione n in output di dimensione m, utilizzato per creare fully connected layers
# in questo caso ReLU viene comandata da inplce, questo viene settato da Linear
# nn.Softmax(1) -> viene spesso applicata all'ultimo layer di una rete neurale per ottenere una distribuzione di probabilità
# rispetto alle classi di output, in questo caso 10 classi
# 1 significa che viene applicata lungo la seconda dimensione, che è quella delle classi 
# (se fosse stato 0 sarebbe stato lungo la prima dimensione, che è quella dei batch)

class MNIST_Net(nn.Module):
    def __init__(self):
        super(MNIST_Net, self).__init__()
        self.encoder = nn.Sequential(
            nn.Conv2d(1, 6, 5),
            nn.MaxPool2d(2, 2),  # 6 24 24 -> 6 12 12
            nn.ReLU(True),
            nn.Conv2d(6, 16, 5),  # 6 12 12 -> 16 8 8
            nn.MaxPool2d(2, 2),  # 16 8 8 -> 16 4 4
            nn.ReLU(True),
        )
        self.classifier = nn.Sequential(
            nn.Linear(16 * 4 * 4, 120),
            nn.ReLU(),
            nn.Linear(120, 84),
            nn.ReLU(),
            nn.Linear(84, 10),
            nn.Softmax(1),
        )

    def forward(self, x):
        x = self.encoder(x)
        x = x.view(-1, 16 * 4 * 4)
        x = self.classifier(x)
        return x
