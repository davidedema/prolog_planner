import re 
import networkx as nx
# from networkx.nx_agraph import graphviz_layout
from bokeh.models import Circle
from bokeh.plotting import figure, from_networkx, show

chunks = []

moveRe = r'.*Exit.*action\((?P<action>[a-zA-Z\_]+)\(.*'
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


actionRe = r'Exit:\s+\((?P<level>\d+)\)\s+action\((?P<actionName>[a-zA-Z_]+)\(.*'
actionArgsRe = r'Call:\s+\((?P<level>\d+)\)\s+stack\({}\((?P<args>([^\[\]]*))\).*\)'
initialStateRe = r'Call:\s+\((?P<level>\d+)\)\s+conditions_met\(\[.*\],\s*(?P<IS>\[.*\])\)'
finalStateRe = r'Call:\s+\((?P<level>\d+)\)\s+plan\((?P<FS>\[.*\])\)'

def createGraph(chunks):
    G = nx.DiGraph()
    G.add_nodes_from([(0, {'label' : "init", 'color' : 'blue', 'parent' : -1, 'level' : 0, 'title' : 'init'})])
    parent = 0
    n = 0
    for chk in chunks:
        chunk = ""
        if re.search(r'[\sa-zA-Z]+\[\d+\]\s+((Exit)|(Call)|(Fail)):', chk):
            lines = chk.split('\n')
            for line in lines:
                m = re.search(r'[\sa-zA-Z]+\[(?P<level>\d+)\]\s+(?P<result>(Redo:)|(Exit:)|(Call:)|(Fail:))(?P<rest>.*)', line)
                if m:
                    newline = "{} ({}){}\n".format(m['result'], m['level'], m['rest'])
                    chunk+=newline
        else:
            chunk = chk

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

        # To check success I'll check that plan is called twice and that the last line is action with a higher level. 
        lines = chunk.split('\n')
        
        index = 0
        while index < len(lines) and not succ:
            m = re.search(r'Call:\s+\((?P<level>\d+)\) stack\(.*\)', lines[index])
            if m:
                index += 1
                m = re.search(r'Exit:\s+\((?P<level>\d+)\) stack\(.*\)', lines[index])
                if m:
                    index += 1
                    m = re.search(finalStateRe, lines[index])
                    if m:
                        FS = m['FS'].split('],')[0]
                        succ = True
            index += 1

        if succ:
            m = re.search(actionArgsRe.format(actionName), chunk)
            if m:
                arguments = m['args']


        # Check if the action is not the last one
        if not succ:
            m = re.search("actions are", chunk)
            if m:
                succ = True
                m = re.search(r'Call:\s+\((?P<level>\d+)\)\s+plan\((?P<FS>\[.*\])\)', chunk)
                if m:
                    if m['FS']:
                        FS = m['FS'].split('],')[0]


        m = re.search(initialStateRe, chunk)
        if m and level == int(m['level']):
            IS = m['IS']
        else:
            IS = "Could not verify ground conditions"
            print(chunk)
            print("Cannot parse IS for the above chunk")

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

        G.add_edge(parent, nodeId)
        parent = nodeId

    return G


def newDrawGraph(G):
    from bokeh.plotting import figure, show
    from bokeh.models import Circle, HoverTool, ColumnDataSource, MultiLine, LabelSet, HTMLLabelSet, CustomJS
    from bokeh.transform import factor_cmap
    from bokeh.palettes import Category10
    from bokeh.models.annotations import Label, BoxAnnotation

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
    Xs = {'S' : [], 'F' : []}
    Ys = {'S' : [], 'F' : []}
    ISs = {'S' : [], 'F' : []}
    FSs = {'S' : [], 'F' : []}
    node_labels = {'S' : [], 'F' : []}
    node_titles = {'S' : [], 'F' : []}
    node_colors = {'S' : [], 'F' : []}
    label_lengths = {'S' : [], 'F' : []}
    node_sizes = {'S' : [], 'F' : []}

    for node in G.nodes:
        if G.nodes[node]['color'] == 'blue':
            pos = list(layout.values())[node]
            Xs['S'].append(pos[0])
            Ys['S'].append(pos[1])
            ISs['S'].append(G.nodes[node].get('IS', ''))
            FSs['S'].append(G.nodes[node].get('FS', ''))
            node_labels['S'].append(G.nodes[node].get('label', ''))
            node_titles['S'].append(G.nodes[node].get('title', ''))
            node_colors['S'].append('blue')
            label_lengths['S'].append(len(node_labels['S'][-1]))
        else:
            pos = list(layout.values())[node]
            Xs['F'].append(pos[0])
            Ys['F'].append(pos[1])
            ISs['F'].append(G.nodes[node].get('IS', ''))
            FSs['F'].append('')
            node_labels['F'].append(G.nodes[node].get('label', ''))
            node_titles['F'].append(G.nodes[node].get('title', ''))
            node_colors['F'].append('red')
            label_lengths['F'].append(len(node_labels['F'][-1]))

    for labelLen in label_lengths['S']:
        node_sizes['S'].append((labelLen/max_node_size)*min_node_size)
    for labelLen in label_lengths['F']:
        node_sizes['F'].append((labelLen/max_node_size)*min_node_size)

    # Create a data source for nodes
    node_source_succ = ColumnDataSource(data=dict(
        x=Xs['S'],
        y=Ys['S'],
        colors=node_colors['S'],
        labels=node_labels['S'],
        titles=node_titles['S'],
        ISs=ISs['S'],
        FSs=FSs['S'],
        sizes=node_sizes['S'],
    ))

    node_source_fail = ColumnDataSource(data=dict(
        x=Xs['F'],
        y=Ys['F'],
        colors=node_colors['F'],
        labels=node_labels['F'],
        titles=node_titles['F'],
        ISs=ISs['F'],
        FSs=FSs['F'],
        sizes=node_sizes['F'],
    ))

    # Create a data source for edges
    edge_source = ColumnDataSource(data=dict(
        xs=[[layout[start][0], layout[end][0]] for start, end in zip(edge_start, edge_end)],
        ys=[[layout[start][1], layout[end][1]] for start, end in zip(edge_start, edge_end)]
    ))

    # Customize node rendering with dynamically calculated sizes
    nodes_succ = plot.ellipse('x', 'y', width='sizes', height=7, source=node_source_succ, color='white', legend_field='labels')

    # Display 'label' attribute inside the nodes using LabelSet
    labels_succ = HTMLLabelSet(x='x', y='y', text='labels', source=node_source_succ, level='glyph',
                      text_align = 'center', text_baseline='middle', text_color='blue')

    # Customize node rendering with dynamically calculated sizes
    nodes_fail = plot.ellipse('x', 'y', width='sizes', height=7, source=node_source_fail, color='white', legend_field='labels')

    # Display 'label' attribute inside the nodes using LabelSet
    labels_fail = HTMLLabelSet(x='x', y='y', text='labels', source=node_source_fail, level='glyph',
                      text_align = 'center', text_baseline='middle', text_color='red')

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
    plot.renderers.extend([nodes_succ, nodes_fail, labels_succ, labels_fail, edges])

    # Show the plot
    show(plot)


def main():
    readFile("file.txt")
    G = createGraph(chunks)
    newDrawGraph(G)

    #test()

    # for chunk in chunks:
    #     parseChunk(chunk)

if __name__ == "__main__":
    main()
