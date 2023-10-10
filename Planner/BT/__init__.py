from . import BTNode
from Planner.STN import SimpTempNet
from Planner.BT.BTNode import *
import networkx as nx
import matplotlib.pyplot as plt

class BehaviourTree(nx.DiGraph):
    def __init__(self, stn : SimpTempNet):
        super(BehaviourTree, self).__init__()
        toBTfromSTN(self, stn)

    def add_bt_node(self, node : BTNode):
        if node:
            assert node not in self.nodes, "Node {} already in BT".format(node)
            self.add_node(node)
            if node.get_parent() is not None:
                self.add_edge(node.get_parent(), node)
                node.get_parent().add_child(node)

    def tick(self):
        list(self.nodes.keys())[0].tick()

    def __str__(self):
        pass

    def draw(self):
        self.drawBokeh()

    def drawBokeh(self) -> None:
        return
        from bokeh.plotting import figure, show, output_file
        from bokeh.models import ColumnDataSource, HTMLLabelSet

        min_node_size = 70000  # Minimum node size
        max_node_size = 10000  # Maximum node size

        # Create a Bokeh plot
        plot = figure(
            width=1600, height=1200
        )

        # Create a layout using from_networkx
        layout = nx.nx_agraph.graphviz_layout(self, prog="dot")  # You can use other layout algorithms as well
        # layout = nx.spring_layout(self)

        # Extract edge endpoints
        edge_start = [edge[0] for edge in self.edges]
        edge_end = [edge[1] for edge in self.edges]

        # Extract node attributes
        Xs = []
        Ys = []
        node_labels = []
        label_lengths = []
        node_sizes = []

        for node in range(len(self.nodes)):
            pos = list(layout.values())[node]
            Xs.append(pos[0])
            Ys.append(pos[1])
            node_labels.append(str(list(self.nodes.keys())[node]))
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
        output_file(filename="BT.html")
        show(plot)

    def drawMatplotlib(self,):
        plt.close()
        plt.figure(1, figsize=(25, 20), dpi=400)

        pos = nx.nx_agraph.graphviz_layout(self, prog="dot")
        # pos = nx.circular_layout(self)
        # pos = nx.spring_layout(self)
        nx.draw_networkx_nodes(self, pos, node_color='#FFFFFF')
        nx.draw_networkx_edges(self, pos, edge_color='#AAAAAA')
        nx.draw_networkx_labels(self, pos, labels={n: self.nodes[n]['label'] for n in self.nodes})

        plt.savefig("BT.png")


def toBTfromSTNRec(bt : BehaviourTree, stn : SimpTempNet, action_id, used, level, parent) -> None:
    """
    @brief This returns a flow, that is a list of lists of nodes
    """
    action = stn.nodes(data=True)[action_id]

    if action in used:
        # bt.add_bt_node(BT_WAIT_ACTION(action, level, parent))
        return

    used.append(action)

    action_children = stn.out_edges(action_id)
    action_parents  = stn.in_edges(action_id)
    if len(action_children) == 0:
        bt.add_bt_node(BT_EXEC_END(action, level, parent))
        return

    # Add information for the current action
    new_parent = BT_SEQ_START(action, level, parent)
    if action['type'] == "init":
        new_parent = BT_INIT_START(action, level, parent)
    bt.add_bt_node(new_parent)

    if len(action_parents) > 1:
        for p in action_parents:
            if stn.nodes(data=True)[p[0]] != parent.get_STN_node():
                bt.add_bt_node(BT_WAIT_ACTION(stn.nodes(data=True)[p[0]], level + 1, new_parent))

    if action['type'] == "start":
        bt.add_bt_node(BT_EXEC_START(action, level+1, new_parent))
    elif action['type'] == "end":
        bt.add_bt_node(BT_EXEC_END(action, level+1, new_parent))
        if "a1" in action['label'] and "grip" in action["label"]:
            bt.add_bt_node(BT_WAIT_ACTION(stn.nodes(data=True)[7], level + 1, new_parent))

    # Check if the children nodes should be run in parallel
    recursive_level = 0
    if len(action_children) > 1:
        new_parent = BT_PAR_START(action, level + 1, new_parent)
        bt.add_bt_node(new_parent)
        recursive_level = 1

    # Add the corresponding child(ren) to the BT
    end_node_id = None
    if action['type'] == "start":
        end_node_id = stn.getEnd(action_id)
        bt.add_bt_node(toBTfromSTNRec(bt, stn, end_node_id, used, level + recursive_level + 1, new_parent))
    for new_node_id in action_children:
        if not end_node_id or (end_node_id and end_node_id != new_node_id[1]):
            bt.add_bt_node(toBTfromSTNRec(bt, stn, new_node_id[1], used, level+recursive_level+1, new_parent))

def toBTfromSTN(bt : BehaviourTree, stn : SimpTempNet):
    # Remove negative edges
    edges_to_remove = []
    for edge in stn.edges(data=True):
        if edge[2]['weight'] < 0:
            edges_to_remove.append(edge)
    for edge in edges_to_remove:
        stn.remove_edge(edge[0], edge[1])

    return toBTfromSTNRec(bt, stn, 0, [], 1, None)