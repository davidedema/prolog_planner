import rospy
from std_msgs.msg import Float64MultiArray
from ros_impedance_controller.srv import generic_float

import trajectory as traj
import numpy as np
import kinematics as kin
import kine as kine

from pyswip import Prolog
from block import Block
import re

patternRot = "rotate\((\w+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(\d+)\)"
patternMov = "move\((\w+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+)\)"
patternLink = "link\((\w+),\s(\w+)\)"
patternGet = r"block\((\w+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(-?\d+\.\d+),\s(\d+),\s(\w+),\s(\w+),\s(\w+),\s\[([^\]]+)\],\s(\d+)\)"
prolog = Prolog()
prolog.consult("../block_world.pl") # if not working, try to use the absolute path
blocks = []

LOOP_RATE = 100
JOINTS = 9
JOINT_TOPIC = '/ur5/joint_group_pos_controller/command'
GRIPPER_OPEN = 0.3
GRIPPER_CLOSE = -0.07

up_down = 0
side = 0


class Publisher:
    def __init__(self):
        self.pub = rospy.Publisher(JOINT_TOPIC, Float64MultiArray, queue_size=10)
        self.q0 = np.array([ -0.32,-0.78, -2.56,-1.63, -1.57, 3.49])
        self.rate = rospy.Rate(LOOP_RATE)
        self.gripper = [GRIPPER_OPEN, GRIPPER_OPEN]

    def publish_point(self, q):
        msg = Float64MultiArray()
        msg.data = q
        if up_down == 1:
            msg.data[5] = 0
        msg.data.extend(self.gripper)
        self.pub.publish(msg)
        self.rate.sleep()
    
    def move(self, pos, orientation):
        rotm = kin.eul2Rot(orientation)
        T = np.identity(4)
        T[:3,:3] = rotm
        T[:3,3] = pos
        possibleQ = kine.invKine(T)
        possibleQT = np.transpose(possibleQ)
        qf = self.findClosestQ(possibleQT)
        qi, qdi, qddi = traj.cubic_trajectory_planning(self.q0, qf, np.zeros(6), np.zeros(6))
        for i in range(len(qi[0,:])):
            self.publish_point(qi[:,i].flatten().tolist())
        self.q0 = qf
        print(qf)

    def eucledianDistance(self, q1, q2):
        q2f = np.zeros(6)
        for i in range(6):
            q2f[i] = q2[0,i]
        return np.linalg.norm(q1-q2)

    def closeGripper(self):
        self.gripper = [GRIPPER_CLOSE, GRIPPER_CLOSE]
        self.publish_point(self.q0.tolist())
    
    def openGripper(self):
        self.gripper = [GRIPPER_OPEN, GRIPPER_OPEN]
        self.publish_point(self.q0.tolist())
    
    def findClosestQ(self, q):
        minDistance = float('inf')
        closestQ = None
        qf_ = np.zeros(6)
        for q_ in q:
            distance = self.eucledianDistance(self.q0, q_)
            if distance < minDistance:
                minDistance = distance
                for i in range(6):
                    qf_[i] = q_[0,i]
                closestQ = qf_
        return closestQ
    
    def moveBlock(self, x1, y1, z1, x2, y2, z2):
        self.move([x1,y1,0.5], [0,0,0])
        for i in range(30):
            self.rate.sleep()
        self.move([x1,y1,z1], [0,0,0])
        for i in range(30):
            self.rate.sleep()
        self.closeGripper()
        for i in range(30):
            self.rate.sleep()
        self.move([x1,y1,0.5], [0,0,0])
        for i in range(30):
            self.rate.sleep()
        self.move([x2,y2,0.5], [0,0,0])
        for i in range(30):
            self.rate.sleep()
        self.move([x2,y2,z2], [0,0,0])
        for i in range(30):
            self.rate.sleep()
        self.openGripper()
        for i in range(30):
            self.rate.sleep()
        self.move([x2,y2,0.5], [0,0,0])
        for i in range(30):
            self.rate.sleep()
    
    def rotateBlock(self, id, x, y, z, no):
        for block in blocks:
            if(id == block.ID):
                b = block

        global up_down 
        
        if b.O == 2:
            # upside down
            up_down = 1
            print('upside down')
            self.move([x - 0.1, y + 0.05, 0.6], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.move([x - 0.1, y + 0.05, z + 0.123], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.closeGripper()
            for i in range(30):
                self.rate.sleep()
            desq = self.q0
            self.move([x - 0.1, y + 0.05, 0.6], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            # rotate the block
            up_down = 0
            self.move([x - 0.1, y + 0.05, 0.6], [90,0,0])
            for i in range(80):
                self.rate.sleep()
            self.move([x - 0.1, y + 0.05, z + 0.095], [90,0,0])
            for i in range(80):
                self.rate.sleep()
            self.openGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x - 0.1, y + 0.05, 0.5], [45,0,0])
            
        else:
            # side
            print('side')


def main():
    rospy.init_node('publisher', anonymous=True)
    pointPub = Publisher()
    pos = []
    rot = []
    q = []
    sel = None
    global up_down, side
    list.clear(blocks)
    result = list(prolog.query("get_blocks(Blocks)"))
    for block in result[0]['Blocks']:
        match = re.match(patternGet, block)
        if match:
            blocks.append(Block(match.group(1), float(match.group(2)), float(match.group(3)), float(match.group(4)), float(match.group(5)), float(match.group(6)), float(match.group(7)), int(match.group(8)), match.group(9), match.group(10), match.group(11), match.group(12), int(match.group(13))))
    
    for block in blocks:
        print(block)

    while not rospy.is_shutdown():
        sel = input('1: move, 2: send joints, 3: kin, 4: gripper, 5: quit : ')
        if sel == '1':
            x = input('x: ')
            y = input('y: ')
            z = input('z: ')
            pos = [x,y,z]
            r = input('roll: ')
            p = input('pitch: ')
            ya = input('yaw: ')
            rot = [r,p,ya]
            if r == '90':
                up_down = 1
            pointPub.move(pos, rot)
        elif sel == '2':
            q = input('q: ')
            q_float = [float(val) for val in q.split()]
            pointPub.publish_point(q_float)
        elif sel == '3':
            sel = input('1: direct, 2: inverse: ')
            if sel == '1':
                q = input('q: ')
                q_float = [float(val) for val in q.split()]
                T = kin.forward_kinematics(q_float)
                print(T)
            elif sel == '2':
                x = input('x: ')
                y = input('y: ')
                z = input('z: ')
                pos = [x,y,z]
                r = input('roll: ')
                p = input('pitch: ')
                ya = input('yaw: ')
                rot = [r,p,ya]
                rotm = kin.eul2Rot(rot)
                T = np.identity(4)
                T[:3,:3] = rotm
                T[:3,3] = pos
                q = kin.inverse_kinematics(T)
                print(q)
        elif sel == '4':
            sel = input('1: open, 2: close: ')
            if sel == '1':
                pointPub.openGripper()
            elif sel == '2':
                pointPub.closeGripper()
        elif sel == '5':
            break
        elif sel == 'pil':
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
                    if "rotate" in action:
                        match = re.match(patternRot, action)
                        if match:
                            pointPub.rotateBlock(match.group(1), float(match.group(2)), float(match.group(3)), float(match.group(4)), int(match.group(5)))

                    if "move" in action:
                        match = re.match(patternMov, action)
                        if match:
                            pointPub.moveBlock(float(match.group(2)), float(match.group(3)), float(match.group(4)), float(match.group(5)), float(match.group(6)), float(match.group(7)))
                    
                    if "link" in action:
                        match = re.match(patternLink, action)
                        if match:
                            pass
                print("Pilastro creato correttamente")
            else:
                print("Non è stato possibile creare il pilastro")

if __name__ == '__main__':
    main()