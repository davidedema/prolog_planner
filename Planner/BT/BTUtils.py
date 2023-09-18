from Planner.STN import SimpTempNet
from Planner.BT.BTNode import *

def toBTFromFlow(G, Flow):
    for node in Flow:
        if type(node) is list:
            toBTFromFlow(G, node)
        else:
            node.add_child()


def toFlowFromSTNRec(stn : SimpTempNet, action_id, used, level, parent):
    """
    @brief This returns a flow, that is a list of lists of nodes
    """
    action = stn.nodes(data=True)[action_id]

    if action in used:
        return BT_WAIT_ACTION(action, level, parent)

    used.append(action)

    action_children = stn.out_edges(action_id)
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
        end_node_id = stn.getEnd(action_id)
        flow.append(toFlowFromSTNRec(stn, end_node_id, used, level + recursive_level + 1, new_parent))
    for new_node_id in action_children:
        if not end_node_id or (end_node_id and end_node_id != new_node_id[1]):
            flow.append(toFlowFromSTNRec(stn, new_node_id[1], used, level+recursive_level+1, new_parent))

    # If the action is parallel, then close it
    # # TODO FIX
    # if len(action_children) > 1:
    #     flow.append(BT_PAR_END(action, level + 1, new_parent.get_parent()))

    # Close sequential flow
    # if action['type'] != "init":
    #     flow.append(BT_SEQ_END(action, level, new_parent))

    return flow

def add_nodes(BT, G, hash_map):
    for node in BT:
        if type(node) is list:
            add_nodes(node, G, hash_map)
        else:
            G.add_nodes_from([(len(G.nodes), {'label': str(node)})])
            hash_map[hash(str(node))] = len(G.nodes) - 1

def add_edges(BT, G, hash_map):
    for node in BT:
        if type(node) is list:
            add_edges(node, G, hash_map)
        elif node.get_parent():
            u = hash_map[hash(str(node.get_parent()))]
            v = hash_map[hash(str(node))]
            G.add_edge(u, v)

def toFlowfromSTN(stn : SimpTempNet):
    # Remove negative edges
    edges_to_remove = []
    for edge in stn.edges(data=True):
        if edge[2]['weight'] < 0:
            edges_to_remove.append(edge)
    for edge in edges_to_remove:
        stn.remove_edge(edge[0], edge[1])

    return toFlowFromSTNRec(stn, 0, [], 1, None)