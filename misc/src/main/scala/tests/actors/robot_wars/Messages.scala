package tests.actors.robot_wars

import akka.actor.ActorRef

case class Start

/**
 * Message for a dispatcher that robot is dead.
 */
case class Dead(robotId: RobotId, position: Position)

// make a damage to a robot
case class Damage(quality: Int)

// All custom messages must be inherited from this one to be processed.
case class AnyMessage

// Periodically self-sent message forcing robot to do something. 
case class Act

/**
 *
 */
case class RobotId(id: String, side: String)

/**
 * Notification message about robot position.
 */
case class RobotPosition(robotId: RobotId, robot: ActorRef, position: Position)

/**
 * Notification for the robot that his dispatcher has changed.
 */
case class NewDispatcher(dispatcher: ActorRef)

/**
 * Custom message of whatever kind, that can be used, for instance,
 * by robots to interchange with others with some commands.
 */
case class CustomMessage(message: AnyMessage)