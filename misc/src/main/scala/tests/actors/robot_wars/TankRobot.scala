package tests.actors.robot_wars

import akka.util.Duration
import akka.util.duration._
import akka.actor.ActorRef

class TankRobot(var pos: Position) extends Robot(pos, 100) {

  val responseTime = 100 milliseconds;

  def act {
    // find some enemies in the nearest positions
    val neightborEnemies = getNeighbors

    // shoot them
    neightborEnemies.foreach(_ ! shell)
  }

  /**
   * Shoot shells in them
   */
  def shell = Damage(20)

  def getNeighbors = {
    // TODO Implement
    List[ActorRef]()
  }

}