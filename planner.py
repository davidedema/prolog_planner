from pyswip import Prolog, Functor, Variable, Query
import pyswip
import re
import networkx as nx
from matplotlib import pyplot as plt
from sys import exit

from BT import *

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
                        attr = G.nodes[i+1]
                        attr['related'] = (j+1, G.nodes[j+1])
                        attr = G.nodes[j + 1]
                        attr['related'] = (i+1, G.nodes[i+1])

                if "move" in m['actionName']:
                    n = re.search(r'move_block_to_table_end\({}.*'.format(m['args']), str(Actions[j]))
                    succ = False
                    if n:
                        G.add_edge(i+1, j+1, weight = MOVE_ONT['upper'])
                        G.add_edge(j+1, i+1, weight = -1 * MOVE_ONT['lower'])
                        succ = True
                    else:
                        n = re.search(r'move_block_to_on_end\({}.*'.format(m['args']), str(Actions[j]))
                        if n:
                            G.add_edge(i+1, j+1, weight = MOVE_ON['upper'])
                            G.add_edge(j+1, i+1, weight = -1 * MOVE_ON['lower'])
                            succ = True
                        else:
                            n = re.search(r'move_block_to_buffer_end\({}.*'.format(m['args']), str(Actions[j]))
                            if n:
                                G.add_edge(i+1, j+1, weight = MOVE_BUF['upper'])
                                G.add_edge(j+1, i+1, weight = -1 * MOVE_BUF['lower'])
                                succ = True
                    if succ:
                        attr = G.nodes[i+1]
                        attr['related'] = (j+1, G.nodes[j+1])
                        attr = G.nodes[j + 1]
                        attr['related'] = (i+1, G.nodes[i+1])
                if "release" in m['actionName']:
                    n = re.search(r'release_end\({}\).*'.format(m['args']), str(Actions[j]))
                    if n:
                        G.add_edge(i+1, j+1, weight = RELEASE['upper'])
                        G.add_edge(j+1, i+1, weight = -1 * RELEASE['lower'])
                        attr = G.nodes[i+1]
                        attr['related'] = (j+1, G.nodes[j+1])
                        attr = G.nodes[j + 1]
                        attr['related'] = (i+1, G.nodes[i+1])
                j += 1

            if n == None and j == len(Actions):
                raise Exception("Actions {}({}) did not end".format(m['actionName'], m['args']))


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

def drawBTBokeh(BT):
    from bokeh.plotting import figure, show
    from bokeh.models import Circle, HoverTool, ColumnDataSource, MultiLine, LabelSet, HTMLLabelSet, CustomJS
    from bokeh.transform import factor_cmap
    from bokeh.palettes import Category10
    from bokeh.models.annotations import Label, BoxAnnotation

    G = nx.DiGraph()

    hash_map = dict()
    add_nodes_BT(G, BT, hash_map)
    add_edges_BT(G, BT, hash_map)

    min_node_size = 70000  # Minimum node size
    max_node_size = 10000  # Maximum node size

    # Create a Bokeh plot
    plot = figure(
        width=1600, height=1200
    )

    # Create a layout using from_networkx
    layout = nx.nx_agraph.graphviz_layout(G, prog="dot")  # You can use other layout algorithms as well
    # layout = nx.spring_layout(G)

    # Extract edge endpoints
    edge_start = [edge[0] for edge in G.edges]
    edge_end = [edge[1] for edge in G.edges]

    # Extract node attributes
    Xs = []
    Ys = []
    node_labels = []
    label_lengths = []
    node_sizes = []

    for node in G.nodes:
        pos = list(layout.values())[node]
        Xs.append(pos[0])
        Ys.append(pos[1])
        node_labels.append(G.nodes[node].get('label', ''))
        label_lengths.append(len(node_labels[-1]))

    for labelLen in label_lengths:
        node_sizes.append((labelLen / max_node_size) * min_node_size)

    # Create a data source for nodes
    node_source = ColumnDataSource(data=dict(
        x=Xs,
        y=Ys,
        labels=node_labels,
        sizes=node_sizes,
    ))

    # Create a data source for edges
    edge_source = ColumnDataSource(data=dict(
        xs=[[layout[start][0], layout[end][0]] for start, end in zip(edge_start, edge_end)],
        ys=[[layout[start][1], layout[end][1]] for start, end in zip(edge_start, edge_end)]
    ))

    # Customize node rendering with dynamically calculated sizes
    nodes = plot.ellipse('x', 'y', width='sizes', height=7, source=node_source, color='white')

    # Display 'label' attribute inside the nodes using LabelSet
    labels = HTMLLabelSet(x='x', y='y', text='labels', source=node_source, level='glyph',
                               text_align='center', text_baseline='middle', text_color='black')

    # Customize edge rendering
    edges = plot.multi_line('xs', 'ys', source=edge_source, line_color='#AAAAAA', line_width=1)

    # Add tooltips to display 'title' attributes for nodes
    # hover = HoverTool()
    # hover.tooltips = [("Label", "@labels")]

    # Remove tooltips for edges
    edges.hover_glyph = None
    edges.nonselection_glyph = None

    # plot.add_tools(hover)

    # Hide Bokeh axes and grid
    plot.axis.visible = False
    plot.grid.visible = False

    # Add nodes, labels, and edge to the same renderers list to ensure proper layering
    plot.renderers.extend([nodes, labels, edges])

    # Show the plot
    show(plot)

def drawBT(BT):
    G = nx.DiGraph()

    hash_map = dict()
    add_nodes_BT(G, BT, hash_map)

    add_edges_BT(G, BT, hash_map)

    plt.close()
    plt.figure(1, figsize=(25, 20), dpi = 400)

    pos = nx.nx_agraph.graphviz_layout(G, prog="dot")
    #pos = nx.circular_layout(G)
    #pos = nx.spring_layout(G)
    nx.draw_networkx_nodes(G, pos, node_color='#FFFFFF')
    nx.draw_networkx_edges(G, pos, edge_color='#AAAAAA')
    nx.draw_networkx_labels(G, pos, labels = {n : G.nodes[n]['label'] for n in G.nodes})

    # from pyvis.network import Network
    #
    # nt = Network('1600px', '1200px')
    # nt.from_nx(G)
    # nt.show("nx.html", notebook=False)


    plt.savefig("BT.png")

    return G

def getEnd(STN, node_id):
    return STN.nodes[node_id]['related'][0]


def toBTRec(STN, action_id, used, level, parent):
    action = STN.nodes(data=True)[action_id]

    if action in used:
        return BT_WAIT_ACTION(action, level, parent)

    used.append(action)

    action_children = STN.out_edges(action_id)
    if len(action_children) == 0:
        return BT_EXEC_END(action, level, parent)

    flow = []

    # Add information for the current action
    if action['type'] != "init":
        flow.append(BT_SEQ_START(action, level, parent))
    else:
        flow.append(BT_INIT_START(action, level, parent))

    new_parent = flow[-1]

    if action['type'] == "start":
        flow.append(BT_EXEC_START(action, level+1, new_parent))
    elif action['type'] == "end":
        flow.append(BT_EXEC_END(action, level+1, new_parent))

    # Check if the children nodes should be run in parallel
    recursive_level = 0
    # TODO FIX
    if len(action_children) > 1:
        flow.append(BT_PAR_START(action, level + 1, new_parent))
        recursive_level = 1
        new_parent = flow[-1]

    # Add the corresponding child(ren) to the BT
    end_node_id = None
    if action['type'] == "start":
        end_node_id = getEnd(STN, action_id)
        flow.append(toBTRec(STN, end_node_id, used, level + recursive_level + 1, new_parent.get_parent()))
    for new_node_id in action_children:
        if not end_node_id or (end_node_id and end_node_id != new_node_id[1]):
            flow.append(toBTRec(STN, new_node_id[1], used, level+recursive_level+1, new_parent))

    # If the action is parallel, then close it
    # TODO FIX
    if len(action_children) > 1:
        flow.append(BT_PAR_END(action, level + 1, new_parent.get_parent()))

    # Close sequential flow
    if action['type'] != "init":
        flow.append(BT_SEQ_END(action, level, new_parent))

    return flow

def fromFlowToBT(G, Flow):
    for node in Flow:
        if type(node) is list:
            fromFlowToBT(G, node)
        else:
            node.add_child()

def toBT(STN):
    return toBTRec(STN, 0, [], 1, None)

def print_BT(BT):
    for node in BT:
        if type(node) is list:
            print_BT(node)
        else:
            print(node, "to", node.get_parent())

def main(): 
    test0 = Functor("test2", 2)
    A = Variable()
    T = Variable()

    sol = Query(test0(A, T))

    sol.nextSolution()
    Actions = list(reversed(list(A.get_value())))
    Times = list(reversed(list(T.get_value())))

    G = nx.DiGraph()
    # Add nodes 
    G.add_nodes_from([(0, {
        'label' : 'init',
        'type'  : 'init'
    })])
    for a in range(len(Actions)):
        G.add_nodes_from([(a+1, {
            'label' : "[{}] {} {}".format(a+1, str(Actions[a]), Times[a]),
            'type'  : "start" if "start" in str(Actions[a]) else "end"
        })])

    addDurations(G, Actions, Times)

    # Add edges
    for it in range(len(Times)):
        t = min([int(time) for time in Times[it]])
        if not G.has_edge(t, it+1):
            G.add_edge(t, it+1, weight=0, label = "from {} to {}".format(t, it+1))
        # for t in list(Times[it]):
        #     G.add_edge(t, it+1, weight = 0, label = "from {} to {}".format(t, it+1))

    if checkConsistency(G):
        print("No negative cycle, so the graph is fine")
    else:
        exit(-1)

    drawSTN(G)

    # Remove negative edges
    edges_to_remove = []
    for edge in G.edges(data=True):
        if edge[2]['weight'] < 0:
            edges_to_remove.append(edge)
    for edge in edges_to_remove:
        G.remove_edge(edge[0], edge[1])

    BT = toBT(G)
    print("Printing BT")
    print_BT(BT)

    drawBTBokeh(BT)


    


if __name__ == "__main__":
    main()
