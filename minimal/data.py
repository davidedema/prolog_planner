from typing import Mapping, Iterator    # import interfaccia Mapping e iterator)
                                        # Mapping agisce come dizionario, iterator serve per iterare    

import torch
import torchvision
import torchvision.transforms as transforms
from problog.logic import Term, Constant

from deepproblog.dataset import Dataset
from deepproblog.query import Query

# trasformazione che deve essere applicata alle immagini prima di addestrare la rete neurale.
# transform.ToTensor() converte le immagini in tensori (torch), immagini in scala di grigi in un tensore a una dimensione
# transform.Normalize() normalizza il tensore precedentemente creato, tensore risulta in un tensore con media 0 e varianza 1

transform = transforms.Compose(
    [transforms.ToTensor(), transforms.Normalize((0.5,), (0.5,))]
)

# datasets è una lista chiave-valore
# definisce due dataset: train e test
# chiave sono le stringhe "train" e "test", valori sono i dataset

datasets = {
    "train": torchvision.datasets.MNIST(
        root='data/', train=True, download=True, transform=transform
    ),
    "test": torchvision.datasets.MNIST(
        root='data/', train=False, download=True, transform=transform
    ),
}

# Questa classe rappresenta un set di immagini MNIST usate per il modello di machine learning
# Implementa l'interfaccia Mapping, che agisce come un dizionario, mappa Term a Tensori

class MNISTImages(Mapping[Term, torch.Tensor]):

    # restituisce un iteratore che itera su tutti gli elementi del dataset corrente    

    def __iter__(self) -> Iterator:
        for i in range(self.dataset):
            yield self.dataset[i][0]

    # restituisce il numero di elementi del dataset corrente

    def __len__(self) -> int:
        return len(self.dataset)

    # crea istanza della classe MNISTImages, subset è una stringa che può essere "train" o "test"
    # dataset è una porzione di dataset, che può essere "train" o "test"

    def __init__(self, subset):
        self.subset = subset
        self.dataset = datasets[self.subset]

    # restituisce il tensore corrispondente alla chiave key

    def __getitem__(self, item):
        return self.dataset[int(item[0])][0]


class AdditionDataset(Dataset):

    # crea istanza della classe AdditionDataset, subset è una stringa che può essere "train" o "test"
    # dataset è una porzione di dataset, che può essere "train" o "test"

    def __init__(self, subset):
        self.subset = subset
        self.dataset = datasets[subset]

    # restituisce il numero di elementi del dataset corrente
    # diviso per 2, perchè ogni esempio di addizione ha due immagini

    def __len__(self):
        return len(self.dataset) // 2

    # Metodo chiamato quando l'oggetto AdditionDataset viene utilizzato in un modello deepproblog
    # in particolare genera due immagini da sommare (image1 e image2) e calcola il risultato (label)
    # restituisce una Query, che è un insieme di termini con anche il risulato atteso

    def to_query(self, i: int) -> Query:
        image1 = Term("tensor", Term(self.subset, Constant(i * 2)))
        image2 = Term("tensor", Term(self.subset, Constant(i * 2 + 1)))
        label = Constant(int(self.dataset[i*2][1] + self.dataset[i*2+1][1]))
        term = Term('addition', image1, image2, label)
        return Query(term)
