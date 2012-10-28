package tests.actors.robot_wars

import akka.actor.ActorRef

case class Shoot
case class Start
case class Damage(quality: Int)

case class AnyMessage

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