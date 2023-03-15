import torch

from datas import MNISTImages, AdditionDataset
from deepproblog.dataset import DataLoader
from deepproblog.engines import ExactEngine
from deepproblog.model import Model
from deepproblog.network import Network
from deepproblog.train import train_model
from network import MNIST_Net

# Crea the oggetto MNIST_Net
network = MNIST_Net()
# Crea oggetto Network con il modello MNIST_Net
net = Network(network, "mnist_net", batching=True)
# Imposta ottimizzatore per il modello
net.optimizer = torch.optim.Adam(network.parameters(), lr=1e-3)
# Crea il modello che contiene la logica e il modello di rete neurale
model = Model("/home/davide/Scrivania/progetto_tesi/minimal/addition.pl", [net])
# Imposta il motore di inferenza
model.set_engine(ExactEngine(model))
# Aggiunge i tensori di input con i dati sia train che test
model.add_tensor_source("train", MNISTImages("train"))
model.add_tensor_source("test", MNISTImages("test"))

# Crea il dataset con i dati di train
dataset = AdditionDataset("test")

# Train the model
# Crea il dataloader con il dataset e il batch size di 2 e senza shuffle
loader = DataLoader(dataset, 2, False)
# Esegue il training del modello 
# train_model(model, loader, 1, log_iter=100, profile=0)
# Salva lo stato del modello
# model.save_state("snapshot/trained_model.pth")

# model.load_state(torch.load("/home/davide/Scrivania/progetto_tesi/snapshot/trained_model.pth"))
model.load_state("/home/davide/Scrivania/progetto_tesi/snapshot/trained_model.pth")



# Query the model
query = dataset.to_query(0)
result = model.solve([query])[0]
print(result)
