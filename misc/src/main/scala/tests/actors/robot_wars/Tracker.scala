package tests.actors.robot_wars

import akka.actor.Actor

/**
 * Actor that gathers all the robot movements and attacks
 * to one place for further visualization and analysis.
 */
class Tracker extends Actor {

  override def receive = {
    case RobotPosition(robotId, _, position) => {

    }
    case Damage(d) => {

    }
    case Dead(robotId, position) => {

    }
    case _ => {
      // whatever
    }
  }
}