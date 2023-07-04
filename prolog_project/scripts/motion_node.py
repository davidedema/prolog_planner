import rospy
from std_msgs.msg import Float64MultiArray
from prolog_project.msg import Ack, Action

import trajectory as traj
import numpy as np
import kinematics as kin
import kine as kine

LOOP_RATE = 100

JOINT_TOPIC = '/ur5/joint_group_pos_controller/command'
ACK_PUB_TOPIC = "/prolog/ack"
ACTION_SUB_TOPIC = "/prolog/action"

GRIPPER_OPEN = 0.3
GRIPPER_CLOSE = -0.07

up_down = 0
side1 = 0
side2 = 0
side3 = 0
side4 = 0


class Publisher:
    def __init__(self):
        self.pubRobot = rospy.Publisher(JOINT_TOPIC, Float64MultiArray, queue_size=10)
        self.pubAck = rospy.Publisher(ACK_PUB_TOPIC, Ack, queue_size=10)
        rospy.Subscriber(ACTION_SUB_TOPIC, Action, self.actionCallback)
        self.q0 = np.array([ -0.32,-0.78, -2.56,-1.63, -1.57, 3.49])
        self.rate = rospy.Rate(LOOP_RATE)
        self.gripper = [GRIPPER_OPEN, GRIPPER_OPEN]
        self.firstB = True
    
    def actionCallback(self, msg):
        print("action received")
        if msg.action == "rotate":
            self.rotateBlock(msg.x0, msg.y0, msg.z0, msg.o0)
            # do the rotation then send ack
            self.sendAck()
        elif msg.action == "move":
            self.moveBlock(msg.x0, msg.y0, msg.z0, msg.xf, msg.yf, msg.zf)
            # do the movement then send ack
            self.sendAck()

    def publish_point(self, q):
        msg = Float64MultiArray()
        msg.data = q
        if up_down == 1:
            msg.data[5] = 0
        if side1 == 1:
            msg.data[5] = 1.57
        if side2 == 1:
            msg.data[5] = -1.57
        if side3 == 1:
            msg.data[5] = -2.45
        if side4 == 1:
            msg.data[5] = 0.758
        msg.data.extend(self.gripper)
        self.pubRobot.publish(msg)
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
        print("moving block")
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
        if self.firstB:
            self.move([x2,y2,z2], [0,0,0])
            self.firstB = False
        else:
            self.move([x2,y2,z2 + 0.02], [0,0,0])
        for i in range(30):
            self.rate.sleep()
        self.openGripper()
        for i in range(30):
            self.rate.sleep()
        self.move([x2,y2,0.5], [0,0,0])
        for i in range(30):
            self.rate.sleep()
    
    def rotateBlock(self, x, y, z, o):

        global up_down, side1, side2, side3, side4
        
        if o == 2:
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
            for i in range(30):
                self.rate.sleep()
            self.move([x - 0.1, y + 0.05, z + 0.095], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.openGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x - 0.1, y + 0.05, 0.5], [45,0,0])
            
        elif o == 3:
            # side 3
            print('side 3')
            side = 1
            self.move([x, y + 0.02, 0.6], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            self.move([x, y + 0.02, z], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            self.closeGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.6], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            side = 0
            up_down = 1
            # rotate the block
            self.move([x + 0.03, y + 0.02, 0.5], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.move([x + 0.03, y + 0.02, z + 0.1], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.openGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.5], [45,0,0])
            up_down = 0

        elif o == 4:
            # side 4
            print('side 4')
            side2 = 1
            self.move([x, y, 0.6], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, z], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            self.closeGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.6], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            side2 = 0
            
            # rotate the block
            self.move([x + 0.03, y + 0.02, 0.5], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            up_down = 1
            self.move([x + 0.03, y + 0.02, z + 0.1], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.openGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.5], [45,0,0])
            up_down = 0      

        elif o == 5:
            # side 5
            print('side 5')
            side3 = 1
            self.move([x, y, 0.6], [0,0,0])
            for i in range(50):
                self.rate.sleep()
            self.move([x, y, z], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            self.closeGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.6], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            
            side3 = 0
            
            up_down = 1
            # rotate the block
            self.move([x + 0.03, y + 0.02, 0.5], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.move([x + 0.03, y + 0.02, z + 0.1], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.openGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.5], [45,0,0])
            up_down = 0

        elif o == 6:
            # side 6
            print('side 6')
            side4 = 1
            self.move([x, y, 0.6], [0,0,0])
            for i in range(50):
                self.rate.sleep()
            self.move([x, y, z], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            self.closeGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.6], [0,0,0])
            for i in range(30):
                self.rate.sleep()
            
            side4 = 0
            
            up_down = 1
            # rotate the block
            self.move([x + 0.03, y + 0.02, 0.5], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.move([x + 0.03, y + 0.02, z + 0.1], [90,0,0])
            for i in range(30):
                self.rate.sleep()
            self.openGripper()
            for i in range(30):
                self.rate.sleep()
            self.move([x, y, 0.5], [45,0,0])
            up_down = 0

    def sendAck(self):
        msg = Ack()
        msg.ack = True
        self.pubAck.publish(msg)
        print('ack sent')   


def main():
    rospy.init_node('publisher', anonymous=True)
    pointPub = Publisher()
    rospy.spin()
    
if __name__ == '__main__':
    main()