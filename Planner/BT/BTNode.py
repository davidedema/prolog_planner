def exec_action(stn_node : dict):
    print(stn_node["label"])

class BT_NODE:
    def __init__(self, STN_node, level, type, parent):
        self.STN_node = STN_node
        self.level = level
        self.type = type
        self.parent = parent
        self.children = []

    def __str__(self):
        return "[{}] {}({})".format(
            self.level,
            self.type,
            self.STN_node['label'].split("] ")[-1].split(" [")[0]
            # self.parent
        )

    def get_STN_node(self):
        return self.STN_node

    def get_parent(self):
        return self.parent

    def get_child(self, index):
        return self.children[index]

    def add_child(self, child):
        self.children.append(child)

    def __hash__(self):
        return hash(str(self))

    def tick(self, par = False):
        pass

class BT_INIT_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        assert parent is None, "Trying to add {} as parent to INIT, but INIT cannot have a parent".format(parent)
        super().__init__(STN_node, level, "INIT", parent)

    def tick(self, par = False):
        assert len(self.children) == 1, "INIT cannot have more than one child"
        self.children[0].tick()

    def get_STN_node(self):
        return super().get_STN_node()

    def get_parent(self):
        return super().get_parent()

    def add_child(self, child):
        super().add_child(child)

    def get_child(self, index):
        return super().get_child(index)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return super().__str__()

    def __hash__(self):
        return super(BT_INIT_START, self).__hash__()

class BT_SEQ_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "SEQ", parent)

    def get_STN_node(self):
        return super().get_STN_node()

    def get_parent(self):
        return super().get_parent()

    def add_child(self, child):
        super().add_child(child)

    def get_child(self, index):
        return super().get_child(index)

    def tick(self, par = False):
        for child in self.children:
            child.tick()

    def __hash__(self):
        return super(BT_SEQ_START, self).__hash__()

    def __repr__(self):
        return str(self)

    def __str__(self):
        return super().__str__()

class BT_PAR_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "PS", parent)

    def get_STN_node(self):
        return super().get_STN_node()

    def get_parent(self):
        return super().get_parent()

    def add_child(self, child):
        super().add_child(child)

    def get_child(self, index):
        return super().get_child(index)

    def tick(self, par = False):
        for child in self.children:
            child.tick()

    def __hash__(self):
        return super(BT_PAR_START, self).__hash__()

    def __repr__(self):
        return str(self)

    def __str__(self):
        return super().__str__()

class BT_EXEC_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "ES", parent)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return super().__str__()

    def get_STN_node(self):
        return super().get_STN_node()

    def get_parent(self):
        return super().get_parent()

    def add_child(self, child):
        super().add_child(child)

    def get_child(self, index):
        return super().get_child(index)

    def __hash__(self):
        return super(BT_EXEC_START, self).__hash__()

    def tick(self, par = False):
        assert len(self.children) == 0, "EXEC_START cannot have children"
        exec_action(self.STN_node)

class BT_EXEC_END(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "EE", parent)

    def get_STN_node(self):
        return super().get_STN_node()

    def get_parent(self):
        return super().get_parent()

    def add_child(self, child):
        super().add_child(child)

    def get_child(self, index):
        return super().get_child(index)

    def tick(self, par = False):
        assert len(self.children) == 0, "EXEC_START cannot have children"
        exec_action(self.STN_node)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return super().__str__()

    def __hash__(self):
        return super(BT_EXEC_END, self).__hash__()


class BT_WAIT_ACTION(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "WA", parent)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return super().__str__()

    def __hash__(self):
        return super(BT_WAIT_ACTION, self).__hash__()

    def get_STN_node(self):
        return super().get_STN_node()

    def get_parent(self):
        return super().get_parent()

    def add_child(self, child):
        super().add_child(child)

    def get_child(self, index):
        return super().get_child(index)

    def tick(self, par = False):
        pass
