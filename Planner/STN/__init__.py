import networkx as nx
import re
import matplotlib.pyplot as plt
import json

class SimpTempNet(nx.DiGraph):
    def __init__(self, actions, times):
        super(SimpTempNet, self).__init__()

        # Add nodes
        super(SimpTempNet, self).add_nodes_from([(0, {
            'label': 'init',
            'type': 'init'
        })])
        for a in range(len(actions)):
            super(SimpTempNet, self).add_nodes_from([(a + 1, {
                'label': "[{}] {} {}".format(a + 1, str(actions[a]), times[a]),
                'type': "start" if "start" in str(actions[a]) else "end"
            })])

        self.addDurations(actions, times)

        # Add edges
        for it in range(len(times)):
            t = 0
            if len(times[it]) > 0:
                t = max([int(time) for time in times[it]])
            if not super(SimpTempNet, self).has_edge(t, it + 1):
                super(SimpTempNet, self).add_edge(t, it + 1, weight=0, label="from {} to {}".format(t, it + 1))

    def getEnd(self, node_id):
        return self.nodes[node_id]['related'][0]

    def addDurations(self, Actions, Times):
        durationsJSON = None
        with open("examples/medical/durations.json", "r") as json_file:
            durationsJSON = json.load(json_file)

        for i in range(len(Actions)):
            matchStart = re.search(r'(?P<actionName>[a-zA-Z_]+)_start\((?P<args>.*)\).*', str(Actions[i]))
            if matchStart:
                # Check that the starting action is in the JSON
                if matchStart["actionName"] not in durationsJSON.keys():
                    print(durationsJSON)
                    raise Exception("Cannot find {} in the JSON file".format(matchStart["actionName"]))
                # Set the ending regex
                if "re_end" in durationsJSON[matchStart['actionName']]:
                    endRegex = durationsJSON[matchStart['actionName']]["re_end"]
                else:
                    endRegex = r'{}_end(\{\}).*'.format(matchStart['actionName'])

                print(matchStart['actionName'], matchStart['args'], endRegex)
                j = i + 1
                n = None
                # Then look for the ending action
                while j < len(Actions) and n == None:
                    n = re.search(endRegex.format(matchStart['args']), str(Actions[j]))

                    if "grip" in matchStart['actionName']:
                        matchEnd = re.search(r'grip_end\({}\).*'.format(matchStart['args']), str(Actions[j]))
                        if matchEnd:
                            if "ontable" in matchStart['actionName']:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = GRIP_ONT['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * GRIP_ONT['lower'])
                            elif "buffer" in matchStart['actionName']:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = GRIP_BUF['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * GRIP_BUF['lower'])
                            else:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = GRIP_ON['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * GRIP_ON['lower'])
                            attr = super(SimpTempNet, self).nodes[i+1]
                            attr['related'] = (j+1, super(SimpTempNet, self).nodes[j+1])
                            attr = super(SimpTempNet, self).nodes[j + 1]
                            attr['related'] = (i+1, super(SimpTempNet, self).nodes[i+1])

                    if "move" in matchStart['actionName']:
                        matchEnd = re.search(r'move_block_to_table_end\({}.*'.format(matchStart['args']), str(Actions[j]))
                        succ = False
                        if matchEnd:
                            super(SimpTempNet, self).add_edge(i+1, j+1, weight = MOVE_ONT['upper'])
                            super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * MOVE_ONT['lower'])
                            succ = True
                        else:
                            matchEnd = re.search(r'move_block_to_on_end\({}.*'.format(matchStart['args']), str(Actions[j]))
                            if matchEnd:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = MOVE_ON['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * MOVE_ON['lower'])
                                succ = True
                            else:
                                matchEnd = re.search(r'move_block_to_buffer_end\({}.*'.format(matchStart['args']), str(Actions[j]))
                                if matchEnd:
                                    super(SimpTempNet, self).add_edge(i+1, j+1, weight = MOVE_BUF['upper'])
                                    super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * MOVE_BUF['lower'])
                                    succ = True
                        if succ:
                            attr = super(SimpTempNet, self).nodes[i+1]
                            attr['related'] = (j+1, super(SimpTempNet, self).nodes[j+1])
                            attr = super(SimpTempNet, self).nodes[j + 1]
                            attr['related'] = (i+1, super(SimpTempNet, self).nodes[i+1])
                    if "release" in matchStart['actionName']:
                        matchEnd = re.search(r'release_end\({}\).*'.format(matchStart['args']), str(Actions[j]))
                        if matchEnd:
                            super(SimpTempNet, self).add_edge(i+1, j+1, weight = RELEASE['upper'])
                            super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * RELEASE['lower'])
                            attr = super(SimpTempNet, self).nodes[i+1]
                            attr['related'] = (j+1, super(SimpTempNet, self).nodes[j+1])
                            attr = super(SimpTempNet, self).nodes[j + 1]
                            attr['related'] = (i+1, super(SimpTempNet, self).nodes[i+1])
                    j += 1

                if matchEnd == None and j == len(Actions):
                    raise Exception("Actions {}({}) did not end".format(matchStart['actionName'], matchStart['args']))



    def addDurationsBlocks(self, Actions, Times):
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
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = GRIP_ONT['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * GRIP_ONT['lower'])
                            elif "buffer" in m['actionName']:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = GRIP_BUF['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * GRIP_BUF['lower'])
                            else:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = GRIP_ON['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * GRIP_ON['lower'])
                            attr = super(SimpTempNet, self).nodes[i+1]
                            attr['related'] = (j+1, super(SimpTempNet, self).nodes[j+1])
                            attr = super(SimpTempNet, self).nodes[j + 1]
                            attr['related'] = (i+1, super(SimpTempNet, self).nodes[i+1])

                    if "move" in m['actionName']:
                        n = re.search(r'move_block_to_table_end\({}.*'.format(m['args']), str(Actions[j]))
                        succ = False
                        if n:
                            super(SimpTempNet, self).add_edge(i+1, j+1, weight = MOVE_ONT['upper'])
                            super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * MOVE_ONT['lower'])
                            succ = True
                        else:
                            n = re.search(r'move_block_to_on_end\({}.*'.format(m['args']), str(Actions[j]))
                            if n:
                                super(SimpTempNet, self).add_edge(i+1, j+1, weight = MOVE_ON['upper'])
                                super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * MOVE_ON['lower'])
                                succ = True
                            else:
                                n = re.search(r'move_block_to_buffer_end\({}.*'.format(m['args']), str(Actions[j]))
                                if n:
                                    super(SimpTempNet, self).add_edge(i+1, j+1, weight = MOVE_BUF['upper'])
                                    super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * MOVE_BUF['lower'])
                                    succ = True
                        if succ:
                            attr = super(SimpTempNet, self).nodes[i+1]
                            attr['related'] = (j+1, super(SimpTempNet, self).nodes[j+1])
                            attr = super(SimpTempNet, self).nodes[j + 1]
                            attr['related'] = (i+1, super(SimpTempNet, self).nodes[i+1])
                    if "release" in m['actionName']:
                        n = re.search(r'release_end\({}\).*'.format(m['args']), str(Actions[j]))
                        if n:
                            super(SimpTempNet, self).add_edge(i+1, j+1, weight = RELEASE['upper'])
                            super(SimpTempNet, self).add_edge(j+1, i+1, weight = -1 * RELEASE['lower'])
                            attr = super(SimpTempNet, self).nodes[i+1]
                            attr['related'] = (j+1, super(SimpTempNet, self).nodes[j+1])
                            attr = super(SimpTempNet, self).nodes[j + 1]
                            attr['related'] = (i+1, super(SimpTempNet, self).nodes[i+1])
                    j += 1

                if n == None and j == len(Actions):
                    raise Exception("Actions {}({}) did not end".format(m['actionName'], m['args']))

    def checkConsistency(self):
        def w(u, v, d):
            return d['weight']

        found = False
        for n in self.nodes:
            try:
                res = nx.find_negative_cycle(self, n, w)
                found = True
                print("Found negative cycle: ")
                for e in res:
                    print(e)
                break
            except Exception:
                pass

        return not found

    def draw(self):
        # self.drawMatplotLib()
        self.drawBokeh()

    def drawBokeh(self):
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
        # layout = nx.spring_layout(G)

        # Extract edge endpoints
        edge_start = [edge[0] for edge in self.edges]
        edge_end = [edge[1] for edge in self.edges]

        # Extract node attributes
        Xs = []
        Ys = []
        node_labels = []
        label_lengths = []
        node_sizes = []

        for node in self.nodes:
            pos = list(layout.values())[node]
            Xs.append(pos[0])
            Ys.append(pos[1])
            node_labels.append(self.nodes[node].get('label', ''))
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
        output_file(filename="STN.html")
        show(plot)

    def drawMatplotLib(self):
        plt.figure(figsize=(25, 20))
        pos = nx.nx_agraph.graphviz_layout(self, prog="dot")
        # pos = nx.circular_layout(G)
        # pos = nx.spring_layout(G)
        nx.draw_networkx_nodes(self, pos, node_color='#FFFFFF')
        nx.draw_networkx_edges(self, pos, edge_color='#AAAAAA', connectionstyle='arc3, rad = 0.2')
        nx.draw_networkx_labels(self, pos, labels={n: self.nodes[n]['label'] for n in self.nodes})
        nx.draw_networkx_edge_labels(self, pos, edge_labels={(u, v): d['weight'] for u, v, d in self.edges(data=True) if
                                                          d['weight'] != 0})
        plt.savefig("name.png")
        # plt.show()

