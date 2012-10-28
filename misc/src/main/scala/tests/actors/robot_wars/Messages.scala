package tests.actors.robot_wars

import akka.actor.ActorRef

case class Shoot
case class Start
case class Damage(quality: Int)

case class RobotId(id: String, side: String)

case class RobotPosition(robotId: RobotId, robot: ActorRef, position: Position)
