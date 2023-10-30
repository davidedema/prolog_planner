from sys import exit

from Planner.Prolog import prolog as PrologLib
from Planner.STN import SimpTempNet
from Planner.BT import BehaviourTree

def main():
    Actions, Times = PrologLib.execTest("testScan")

    stn = SimpTempNet(Actions, Times)

    if stn.checkConsistency():
        print("No negative cycle, so the graph is fine")
    else:
        exit(-1)

    stn.draw()

    bt = BehaviourTree(stn)
    bt.draw()
    bt.tick()


if __name__ == "__main__":
    main()
