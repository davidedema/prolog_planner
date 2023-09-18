from . import BTNode
from .BTUtils import toFlowfromSTN, add_nodes, add_edges
from ..STN import SimpTempNet
import networkx as nx
import matplotlib.pyplot as plt
from bokeh.plotting import figure, show
from bokeh.models import ColumnDataSource, HTMLLabelSet

class BehaviourTree:
    def __init__(self, stn : SimpTempNet):
        self.flow = toFlowfromSTN(stn)

    def getFlow(self):
        return self.flow

    def toBT(self, stn : SimpTempNet):
        pass

    def __str__(self):
        pass

    def draw(self):
        self.drawBokeh()

    def drawBokeh(self):
        G = nx.DiGraph()

        hash_map = dict()
        add_nodes(self.flow, G, hash_map)
        add_edges(self.flow, G, hash_map)

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

    def drawMatplotlib(self,):
        G = nx.DiGraph()

        hash_map = dict()
        add_nodes(self.flow, G, hash_map)

        add_edges(self.flow, G, hash_map)

        plt.close()
        plt.figure(1, figsize=(25, 20), dpi=400)

        pos = nx.nx_agraph.graphviz_layout(G, prog="dot")
        # pos = nx.circular_layout(G)
        # pos = nx.spring_layout(G)
        nx.draw_networkx_nodes(G, pos, node_color='#FFFFFF')
        nx.draw_networkx_edges(G, pos, edge_color='#AAAAAA')
        nx.draw_networkx_labels(G, pos, labels={n: G.nodes[n]['label'] for n in G.nodes})

        # from pyvis.network import Network
        #
        # nt = Network('1600px', '1200px')
        # nt.from_nx(G)
        # nt.show("nx.html", notebook=False)

        plt.savefig("BT.png")

        return G