import rospy
from prolog_project.msg import Ack, Action

from pyswip import Prolog
from block import Block
import re

patternRot = "rotate\((\w+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(\d+)\)"
patternMov = "move\((\w+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+)\)"
patternLink = "link\((\w+),\s(\w+)\)"
patternGet = r"block\((\w+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(\d+),\s(\w+),\s(\w+),\s(\w+),\s\[([^\]]+)\],\s(\d+)\)"
prolog = Prolog()
prolog.consult("/home/davide/ros_ws/src/prolog_project/scripts/block_world.pl")
blocks = []

LOOP_RATE = 100
ACK_SUB_TOPIC = "/prolog/ack"
ACTION_PUB_TOPIC = "/prolog/action"

ready = True
actionPublisher = None

def ackCallback(data):
    print("ack received")
    global ready
    ready = True

# def publishAction(action):


def main():
    print("### STARTING PROLOG NODE ###")
    rospy.init_node('prolog_node', anonymous=True)
    actionPublisher = rospy.Publisher(ACTION_PUB_TOPIC, Action, queue_size=10)
    rospy.Subscriber(ACK_SUB_TOPIC, Ack, ackCallback)
    rate = rospy.Rate(LOOP_RATE)
    actionMsg = Action()

    list.clear(blocks)
    result = list(prolog.query("get_blocks(Blocks)"))
    for block in result[0]['Blocks']:
        match = re.match(patternGet, block)
        if match:
            blocks.append(Block(match.group(1), float(match.group(2)), float(match.group(3)), float(match.group(4)), float(match.group(5)), float(match.group(6)), float(match.group(7)), int(match.group(8)), match.group(9), match.group(10), match.group(11), match.group(12), int(match.group(13))))
    
    for block in blocks:
        print(block)
    
    global ready
    
    while not rospy.is_shutdown():
        print("Inserisci le specifiche del pilastro: ")
        x = input("x: ")
        y = input("y: ")
        z = input("z: ")
        h = input("Inserisci l'altezza del pilastro: ")
        w = input("Inserisci la larghezza del pilastro: ")
        d = input("Inserisci la profondità del pilastro: ")
        result = list(prolog.query(
                "once(pillar(" + x + "," + y + "," + z + "," + h + "," + w + "," + d + ", Actions))"))
        if result:
            print("Creazione pilastro . . .")
            for action in result[0]["Actions"]:
                while not ready:
                    rate.sleep()
                if "rotate" in action:
                    match = re.match(patternRot, action)
                    for block in blocks:
                        if(match.group(1) == block.ID):
                            b = block
                    actionMsg.action = "rotate"
                    actionMsg.x0 = float(match.group(2))
                    actionMsg.y0 = float(match.group(3))
                    actionMsg.z0 = float(match.group(4))
                    actionMsg.o0 = b.O
                    actionMsg.xf = 0
                    actionMsg.yf = 0
                    actionMsg.zf = 0
                    actionMsg.of = int(match.group(5))
                    actionPublisher.publish(actionMsg)
                    ready = False
                    
                if "move" in action:
                    match = re.match(patternMov, action)
                    actionMsg.action = "move"
                    actionMsg.x0 = float(match.group(2))
                    actionMsg.y0 = float(match.group(3))
                    actionMsg.z0 = float(match.group(4))
                    actionMsg.o0 = 0
                    actionMsg.xf = float(match.group(5))
                    actionMsg.yf = float(match.group(6))
                    actionMsg.zf = float(match.group(7))
                    actionMsg.of = 0
                    actionPublisher.publish(actionMsg)
                    ready = False
            print("Pilastro creato correttamente")
        else:
            print("Non è stato possibile creare il pilastro")

if __name__ == '__main__':
    main()
