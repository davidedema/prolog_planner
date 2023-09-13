from pyswip import Prolog, Functor, Variable, Query
import pyswip
import re
import networkx as nx
from matplotlib import pyplot as plt
from sys import exit

prolog = Prolog()
prolog.consult("planner_easy.pl")

def drawSTN(G):
    drawSTNMatplotLib(G)
    # drawSTNBokeh(G)

def drawSTNBokeh(G):
    pass
#    plot = figure(width = 1600, height = 1200)
#    layout = nx.nx_agraph.graphviz_layout(G, prog='dot')
#
#    # Extract edge attributes
#    edge_start = [edge[0] for edge in G.edges]
#    edge_end = [edge[1] for edge in G.edges]
#    edge_weights = {(u,v) : }

def drawSTNMatplotLib(G):
    plt.figure(figsize = (25,20))
    pos = nx.nx_agraph.graphviz_layout(G, prog="dot")
    # pos = nx.circular_layout(G)
    # pos = nx.spring_layout(G)
    nx.draw_networkx_nodes(G, pos, node_color='#FFFFFF')
    nx.draw_networkx_edges(G, pos, edge_color='#AAAAAA', connectionstyle='arc3, rad = 0.2')
    nx.draw_networkx_labels(G, pos, labels = {n : G.nodes[n]['label'] for n in G.nodes})
    nx.draw_networkx_edge_labels(G, pos, edge_labels = {(u,v) : d['weight'] for u,v,d in G.edges(data=True) if d['weight'] != 0})
    plt.savefig("name.png")
    # plt.show()

def addDurations(G, Actions, Times):
    import re
    GRIP_ONT = {'lower' : 3,  'upper' : 5}
    GRIP_BUF = {'lower' : 6,  'upper' : 10}
    GRIP_ON  = {'lower' : 4,  'upper' : 7}
    MOVE_ONT = {'lower' : 10, 'upper' : 20}
    MOVE_BUF = {'lower' : 10, 'upper' : 20}
    MOVE_ON  = {'lower' : 15, 'upper' : 30}
    RELEASE  = {'lower' : 5,  'upper' : 10}

    for i in range(len(Actions)):
        m = re.search(r'(?P<actionName>[a-zA-Z_]+)_start\((?P<args>.*)\).*', str(Actions[i]))
        if m:
            print(m['actionName'], m['args'])
            j = i + 1
            n = None
            while j < len(Actions) and n == None:
                if "grip" in m['actionName']:
                    n = re.search(r'grip_end\({}\).*'.format(m['args']), str(Actions[j]))
                    if n:
                        if "ontable" in m['actionName']:
                            G.add_edge(i+1, j+1, weight = GRIP_ONT['upper'])
                            G.add_edge(j+1, i+1, weight = -1 * GRIP_ONT['lower'])
                        elif "buffer" in m['actionName']:
                            G.add_edge(i+1, j+1, weight = GRIP_BUF['upper'])
                            G.add_edge(j+1, i+1, weight = -1 * GRIP_BUF['lower'])
                        else:
                            G.add_edge(i+1, j+1, weight = GRIP_ON['upper'])
                            G.add_edge(j+1, i+1, weight = -1 * GRIP_ON['lower'])
                if "move" in m['actionName']:
                    n = re.search(r'move_block_to_table_end\({}.*'.format(m['args']), str(Actions[j]))
                    if n:
                        G.add_edge(i+1, j+1, weight = MOVE_ONT['upper'])
                        G.add_edge(j+1, i+1, weight = -1 * MOVE_ONT['lower'])
                    else:
                        n = re.search(r'move_block_to_on_end\({}.*'.format(m['args']), str(Actions[j]))
                        if n:
                            G.add_edge(i+1, j+1, weight = MOVE_ON['upper'])
                            G.add_edge(j+1, i+1, weight = -1 * MOVE_ON['lower'])
                        else:
                            n = re.search(r'move_block_to_buffer_end\({}.*'.format(m['args']), str(Actions[j]))
                            if n:
                                G.add_edge(i+1, j+1, weight = MOVE_BUF['upper'])
                                G.add_edge(j+1, i+1, weight = -1 * MOVE_BUF['lower'])
                if "release" in m['actionName']:
                    n = re.search(r'release_end\({}\).*'.format(m['args']), str(Actions[j]))
                    if n:
                        G.add_edge(i+1, j+1, weight = RELEASE['upper'])
                        G.add_edge(j+1, i+1, weight = -1 * RELEASE['lower'])
                j += 1

            if n == None and j == len(Actions):
                raise Exception("Actions {}({}) did not end".format(m['actionName'], m['args']))

    # print(G.get_edge_data(2,1), type(G.get_edge_data(1,2)))
    # print(G.get_edge_data(1,2), type(G.get_edge_data(1,2)))
    # print(G.nodes[1]['label'], G.nodes[2]['label'], G.get_edge_data(1, 2)['weight'], G.get_edge_data(2, 1)['weight'])
    # edge_labels = {(u,v) : d['weight'] for u,v,d in G.edges(data=True) if d['weight'] != 0} 
    # print(edge_labels)

def checkConsistency(G):
    def w(u, v, d):
        return d['weight']
    
    found = False
    for n in G.nodes:
        try:
            res = nx.find_negative_cycle(G, n, w)
            found = True
            print("Found negative cycle: ")
            for e in res:
                print(e)
            break
        except Exception:
            print("No negative cycle from", G.nodes[n]['label'])
            
    return not found
        


def main(): 
    test0 = Functor("test7", 2)
    A = Variable()
    T = Variable()

    sol = Query(test0(A, T))

    sol.nextSolution()
    Actions = list(reversed(list(A.get_value())))
    Times = list(reversed(list(T.get_value())))

    G = nx.DiGraph()
    # Add nodes 
    G.add_nodes_from([(0, {'label' : 'init'})])
    for a in range(len(Actions)):
        G.add_nodes_from([(a+1, {'label' : "[{}] {}\n{}".format(a+1, str(Actions[a]), Times[a])})])

    # Add edges
    for it in range(len(Times)):
        t = min([int(time) for time in Times[it]])
        G.add_edge(t, it+1, weight=0, label = "from {} to {}".format(t, it+1))
        # for t in list(Times[it]):
        #     G.add_edge(t, it+1, weight = 0, label = "from {} to {}".format(t, it+1))

    addDurations(G, Actions, Times)

    drawSTN(G)
    if checkConsistency(G):
        print("No negative cycle, so the graph is fine")
    else:
        exit(-1)
    


if __name__ == "__main__":
    main()
