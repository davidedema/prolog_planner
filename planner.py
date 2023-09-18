import networkx as nx
from matplotlib import pyplot as plt
from sys import exit

from Planner.BT.BTNode import *
from Planner.Prolog import prolog as PrologLib
from Planner.STN import SimpTempNet
from Planner.BT import BehaviourTree


def add_nodes_edges_BT(G, BT, parent = -1):
    if type(BT) is not list:
        G.add_nodes_from([(len(G.nodes), {'label' : str(BT)})])
        if parent != -1:
            G.add_edge(parent, len(G.nodes)-1)
        return len(G.nodes)-1
    else:
        old_parent = parent
        for node in BT:
            parent = add_nodes_edges_BT(G, node, parent)
        return old_parent

def add_nodes_BT(G, BT, hash_map):
    for node in BT:
        if type(node) is list:
            add_nodes_BT(G, node, hash_map)
        else:
            G.add_nodes_from([(len(G.nodes), {'label' : str(node)})])
            hash_map[hash(str(node))] = len(G.nodes)-1

def add_edges_BT(G, BT, hash_map):
    for node in BT:
        if type(node) is list:
            add_edges_BT(G, node, hash_map)
        elif node.get_parent():
            u = hash_map[hash(str(node.get_parent()))]
            v = hash_map[hash(str(node))]
            G.add_edge(u, v)




def print_BT(BT):
    for node in BT:
        if type(node) is list:
            print_BT(node)
        else:
            print(node, "to", node.get_parent())

def main():
    Actions, Times = PrologLib.execTest("test1")

    stn = SimpTempNet(Actions, Times)

    if stn.checkConsistency():
        print("No negative cycle, so the graph is fine")
    else:
        exit(-1)

    stn.draw()

    bt = BehaviourTree(stn)
    print("Printing BT")
    print_BT(bt.getFlow())
    bt.draw()



if __name__ == "__main__":
    main()
