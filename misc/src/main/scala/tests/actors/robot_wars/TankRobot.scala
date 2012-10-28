package tests.actors.robot_wars

import akka.util.Duration
import akka.util.duration._
import akka.actor.ActorRef

class TankRobot(
  override val id: String,
  override val side: String,
  val pos: Position,
  dispatcher: ActorRef)
  extends Robot(id, side, pos, 100, dispatcher) {

  val responseTime = 100 milliseconds;
  val sightDistance = 10;

  def act {
    // find some enemies among the neighbors
    val enemies = neighbors.filter(_._2.side != side)
    if (enemies.isEmpty) {
      // no enemies here, move somewhere to finds ones
      move
    }

    // attack with a shell
    enemies.head._3 ! shell

    // no need to move in case there's still enemies
    if (enemies.tail.isEmpty) {
      // there's no more enemies
      move
    }
  }

  def makeNextMove = {
    position
  }

  /**
   * Shoot shells in them
   */
  def shell = Damage(20)

}