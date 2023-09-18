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

class BT_INIT_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        if parent:
            raise Exception("INIT cannot have a parent")
        super().__init__(STN_node, level, "INIT", parent)

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

class BT_SEQ_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "SEQ", parent)

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

# class BT_SEQ_END(BT_NODE):
#     def __init__(self, STN_node, level, parent):
#         super().__init__(STN_node, level, "SE", parent)
#
#     def __repr__(self):
#         return str(self)
#
#     def __str__(self):
#         return super().__str__()
#
#     def get_STN_node(self):
#         return super().get_STN_node()
#
#     def get_parent(self):
#         return super().get_parent()
#
#     def add_child(self, child):
#         super().add_child(child)
#
#     def get_child(self, index):
#         return super().get_child(index)

class BT_PAR_START(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "PS", parent)

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

class BT_PAR_END(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "PE", parent)

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

class BT_EXEC_END(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "EE", parent)

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

class BT_WAIT_ACTION(BT_NODE):
    def __init__(self, STN_node, level, parent):
        super().__init__(STN_node, level, "WA", parent)

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