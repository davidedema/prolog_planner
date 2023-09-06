import re 
import networkx as nx
# from networkx.nx_agraph import graphviz_layout
from bokeh.models import Circle
from bokeh.plotting import figure, from_networkx, show

chunks = []

moveRe = r'.*action\((?P<action>[a-zA-Z\_]+)\(.*'
succRe = r':\s\(\d+\)\sstack\(.+\)'
addedActionsRe = r'stack\([a-zA-Z]+\(.*\), \[.*\], \[(?P<newActions>.*)\])'
newPlanRe = r'Call: \(\d+\) plan\(\[(?P<newState>[^\[\]]*)\],\s\[.*\],\s\[.*\],\s\[.*\]\)'
checkConditionsRe = r'conditions_met\(\[(?P<toMeet>[^\[\]]*)\],\s\[(?P<state>[^\[\]]*)\].*'
condToMeetRe = r'(?P<actionName>[a-zA-Z]+)\([^\(\)]+\)'

def parseChunk(chunk):
    succ = False
    m = re.search(succRe, chunk)
    if m:
        succ = True

    actionName = ""
    m = re.match(moveRe, chunk)
    if m:
        actionName = m["action"]
    else:
        raise Exception("Name could not be parsed:\n", chunk)

    if succ:
        newState = ""
        m = re.search(newPlanRe, chunk)
        if m:
            newState = m['newState']
        else:
            raise Exception("Could not find newState:\n", chunk)

        print("\x1b[5;30;42m{} -> S -> [{}]\x1b[0m".format(actionName, newState))

    else:
        notMet = []
        toMeet = ""
        state = ""
        m = re.search(checkConditionsRe, chunk)
        if m:
            toMeet = m["toMeet"]
            state = m["state"]
        else: 
            raise Exception("Could not find conditions:\n", chunk)


        predInState = []
        for pred in re.finditer(condToMeetRe, state):
            predInState.append(pred["actionName"]+'(')

        for condTmp in re.finditer(condToMeetRe, toMeet):
            cond = condTmp["actionName"].split("(")[0]+'('
            if cond not in predInState:
                notMet.append(cond[:-1])
        
        if len(notMet) == 0:
            notMet = ["inposition"]


        missing = ""
        for i in notMet:
            missing += i+", "
        missing = missing[:-2]

        print("\x1b[0;30;41m{} -> F -> [{}]\x1b[0m".format(actionName, missing))


def readFile(filename):
    global chunks
    with open(filename) as file:
        newline = ""
        for line in file:
            m = re.match(moveRe, line)
            if m:
                chunks.append(newline)
                newline = ""
            newline+=line
        chunks.append(newline)

    chunks = chunks[1:]

def add_edge (G, parent, lastLevel, level, nodeId):
    if  lastLevel == 0 or lastLevel == level - 1:
        print("Adding edge between", parent[0], "and", nodeId[0])
        G.add_edge(parent, nodeId)
        lastLevel = level
        parent = nodeId

    else: # lastLevel > level - 1:
        parent = G.nodes[parent]['parent']
        add_ege(G, parent, lastLevel, level)


actionRe = r'Exit:\s+\((?P<level>\d+)\)\s+action\((?P<actionName>[a-zA-Z_]+)\(.*'
actionArgsRe = r'Call:\s+\((?P<level>\d+)\)\s+stack\({}\((?P<args>([^\[\]]*))\).*\)'
initialStateRe = r'Call:\s+\((?P<level>\d+)\)\s+conditions_met\(\[.*\],\s*(?P<IS>\[.*\])\)'
finalStateRe = r'Redo:\s+\((?P<level>\d+)\)\s+plan\((?P<FS>\[.*\])\)'

def createGraph(chunks):
    G = nx.DiGraph()
    G.add_nodes_from([(0, {'label' : "init", 'color' : 'blue', 'parent' : -1, 'level' : 0, 'title' : 'init'})])
    parent = 0
    n = 0
    for chunk in chunks:
        if G.nodes[parent]['level'] == 18: 
            break

        actionName = ""
        level = 0
        arguments = ""
        succ = False
        IS = ""
        FS = ""

        m = re.search(actionRe, chunk)
        if m:
            actionName = m['actionName']
            level = int(m['level'])
        else:
            print(chunk)
            raise Exception("Could not parse actionName")

        m = re.search(actionArgsRe.format(actionName), chunk)
        if m:
            if int(m['level']) != level:
                raise Exception("Level error in this chunk")
            arguments = m['args']

        # To check success I'll check that plan is called twice and that the last line is action with a higher level. 
        lines = chunk.split('\n')
        
        if len(lines) > 3: 
            if lines[-1].strip() == "":
                lines = lines[-4:]
            else:
                lines = lines[-3:]
            m = re.search(r'Call:\s+\((?P<level>\d+)\)\s+plan\(.*', lines[0])
            if m and int(m['level']) == level:
                m = re.search(finalStateRe, lines[1])
                if m and int(m['level']) == level:
                    if m['FS']:
                        FS = m['FS'].split('],')[0]
                    m = re.search(r'Call:\s+\((?P<level>\d+)\)\s+action\(.*', lines[2])
                    if m and int(m['level']) - 1 == level:
                        succ = True

        m = re.search(initialStateRe, chunk)
        if m and level == int(m['level']):
            IS = m['IS']
        else:
            print(chunk)
            raise Exception("Cannot parse IS for the above chunk")

        nodeId = len(G.nodes)
        G.add_nodes_from([(
            nodeId,
            {
                'color' : 'blue' if succ else 'red',
                'label' : actionName+"()",
                'title' : "[{}] ".format(level)+actionName+"({})".format(arguments),
                'IS'    : IS,
                'FS'    : FS,
                'parent': parent,
                'level' : level
            }
        )])   
        
        lastLevel = G.nodes[parent]['level']
        while lastLevel != 0 and lastLevel >= level:
            parent = G.nodes[parent]['parent']
            lastLevel = G.nodes[parent]['level']

        if lastLevel == 0 or lastLevel == level - 1:
            print("Adding edge between", parent, "and", nodeId)
            G.add_edge(parent, nodeId)
            lastLevel = level
            parent = nodeId

    return G

    # graph_renderer = from_networkx(G, nx.nx_agraph.graphviz_layout(G, prog="dot"))

    # color_mapper = linear_cmap(field_name='index', palette=Viridis256, low = 0, high = 1)
    # graph_renderer.node_renderer.glyph = Circle(size=15, fill_color=color_mapper)
    # graph_renderer.node_renderer.data_source.data['index'] = list(reversed(range(len(G))))
    # graph_renderer.node_renderer.data_source.data['colors'] = node_colors
    # plot.renderers.append(graph_renderer)
    # show(plot)


    # # networkx X matplotlib
    # plt.figure(figsize=(120,120))
    # nx.draw(G, 
    #         with_labels = True, 
    #         pos = nx.nx_agraph.graphviz_layout(G, prog="twopi"),
    #         labels = labels, 
    #         node_color = colors, 
    #         node_shape='s', 
    #         node_size = sizes,
    #         font_size = 4
    # )
    # plt.savefig("graph.png", dpi=200)

    # # pyvis 
    # from pyvis.network import Network
    # net = Network(notebook = True)
    # net.from_nx(G)
    # net.show('example.html')

def testCreateGraph():
    # Create a sample NetworkX graph with node attributes
    G = nx.Graph()
    G.add_node(1, color='red', label='Node 1', title='This is Node 1')
    G.add_node(2, color='blue', label='Node 2', title='This is Node 2')
    G.add_node(3, color='green', label='Node 3', title='This is Node 3')
    G.add_edge(1, 2)
    G.add_edge(1, 3)
    
    return G
    show(plot)

def drawGraph(G):
    from bokeh.plotting import figure, show
    from bokeh.models import Circle, HoverTool, ColumnDataSource, MultiLine, LabelSet, HTMLLabelSet
    from bokeh.transform import factor_cmap
    from bokeh.palettes import Category10
    from bokeh.models.annotations import Label, BoxAnnotation

    # Extract node attributes
    ISs = [node_data.get('IS', '') for node_data in G.nodes.values()]  # Default to 'blue' if 'color' not present
    FSs = [node_data.get('FS', '') for node_data in G.nodes.values()]  # Default to 'blue' if 'color' not present
    node_colors = [node_data.get('color', 'blue') for node_data in G.nodes.values()]  # Default to 'blue' if 'color' not present
    node_labels = [node_data.get('label', '') for node_data in G.nodes.values()]  # Default to empty string if 'label' not present
    node_titles = [node_data.get('title', '') for node_data in G.nodes.values()]  # Default to empty string if 'title' not present
    # Calculate node sizes based on label lengths
    label_lengths = [len(label) for label in node_labels]
    x_offsets = [llabel/2 for llabel in label_lengths]
    max_label_length = max(label_lengths)
    min_node_size = 100  # Minimum node size
    max_node_size = 250  # Maximum node size

    # Scale node sizes proportionally to label lengths
    node_sizes = [(size * max_node_size) / max_label_length if max_label_length > 0 else min_node_size for size in label_lengths]

    # Extract edge endpoints
    edge_start = [edge[0] for edge in G.edges]
    edge_end = [edge[1] for edge in G.edges]

    # Create a Bokeh plot
    plot = figure(
        width=3200, height=1200
    )

    # Create a layout using from_networkx
    layout = nx.nx_agraph.graphviz_layout(G, prog="dot")  # You can use other layout algorithms as well
    # layout = nx.spring_layout(G)

    # Create a data source for nodes
    node_source = ColumnDataSource(data=dict(
        x=[pos[0] for pos in layout.values()],
        y=[pos[1] for pos in layout.values()],
        colors=node_colors,
        labels=node_labels,
        titles=node_titles,
        ISs=ISs,
        FSs=FSs,
        sizes=node_sizes,
        x_offsets = x_offsets,
    ))

    # Create a data source for edges
    edge_source = ColumnDataSource(data=dict(
        xs=[[layout[start][0], layout[end][0]] for start, end in zip(edge_start, edge_end)],
        ys=[[layout[start][1], layout[end][1]] for start, end in zip(edge_start, edge_end)]
    ))

    # Define a categorical color mapper based on the 'color' attribute
    color_mapper = factor_cmap(field_name='colors', palette=Category10[10], factors=list(set(node_colors)))

    # Customize node rendering with dynamically calculated sizes
    nodes = plot.ellipse('x', 'y', width='sizes', height=1, source=node_source, color=color_mapper, legend_field='labels')

    # Display 'label' attribute inside the nodes using LabelSet
    labels = HTMLLabelSet(x='x', y='y', text='labels', source=node_source, level='glyph',
                      text_align = 'center', text_baseline='middle', text_color='black')

    # Customize edge rendering
    edges = plot.multi_line('xs', 'ys', source=edge_source, line_color='gray', line_width=1)

    # Add tooltips to display 'title' attributes for nodes
    hover = HoverTool()
    hover.tooltips = [("Title", "@titles"), ("IS", "@ISs"), ("FS", "@FSs")]

    # Remove tooltips for edges
    edges.hover_glyph = None
    edges.nonselection_glyph = None

    plot.add_tools(hover)

    # Hide Bokeh axes and grid
    plot.axis.visible = False
    plot.grid.visible = False

    # Add nodes, labels, and edge to the same renderers list to ensure proper layering
    plot.renderers.extend([nodes, labels, edges])

    # Show the plot
    show(plot)


def main():
    readFile("file.txt")
    G = createGraph(chunks)
    drawGraph(G)

    #test()

    # for chunk in chunks:
    #     parseChunk(chunk)

if __name__ == "__main__":
    main()
