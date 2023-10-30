import re


actionRe = r"""
    action\(\n\s+(?P<name>[a-zA-Z_\(\),\s]+),\s*\n
        \s+\[(?P<preT>[,\sa-zA-Z_\(\)]+)\],\s*\n
        \s+\[(?P<preF>[,\sa-zA-Z_\(\)]*)\],\s*\n
        \s+\[(?P<finT>[,\sa-zA-Z_\(\)]*)\],\s*\n
        \s+\[(?P<kb>[,\sa-zA-Z_\(\)]*)\],\s*\n
        \s+\[(?P<eff>[\s\n,a-zA-Z_\(\)]+)\]\n\)\.""".replace(" ","").replace("\n","")

addEffRe = r'add\((?P<pred>[a-zA-Z\(\)_]+)\)'
delEffRe = r'del\((?P<pred>[a-zA-Z\(\)_]+)\)'

actions = []
class Action:
    def __init__(self, name_, preT_, preF_, finT_, kb_, eff_):
        self.name = name_
        self.preT = preT_
        self.preF = preF_
        self.finT = finT_
        self.kb = kb_
        self.eff = eff_

    def applyNegEff(self, predicates):
        for negPred in re.finditer(delEffRe, self.eff):
            if negPred:
                try:
                    predicates.remove(negPred["pred"])
                except Exception as E:
                    pass

    def applyPosEff(self, predicates):
        for posPred in re.finditer(addEffRe, self.eff):
            if posPred:
                predicates.append(posPred["pred"])   

    def __str__(self):
        return "action(\n\t{}\n\t[{}]\n\t[{}]\n\t[{}]\n\t[{}]\n\t[{}])".format(
                self.name, self.preT, self.preF, self.finT, self.kb, self.eff
            )

def main():
    global actions
    
    predicates = []

    with open("examples/medical/actions.pl", "r") as file:
        data = file.read()

        for actionText in re.finditer(actionRe, data):
            if actionText:
                actions.append(Action(
                    actionText.groupdict()["name"],
                    actionText.groupdict()["preT"],
                    actionText.groupdict()["preF"],
                    actionText.groupdict()["finT"],
                    actionText.groupdict()["kb"],
                    actionText.groupdict()["eff"]
                ))
    print(len(actions))

    for action in actions:
        action.applyPosEff(predicates)

    for pred in predicates:
        print(pred)

    for action in actions:
        action.applyNegEff(predicates)

    print("\nRemaining:")

    for pred in predicates:
        print(pred)

    



if __name__ == "__main__":
    main()
