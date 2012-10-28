package tests.actors.robot_wars

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.PoisonPill
import akka.actor.actorRef2Scala
import akka.util.Duration
import akka.util.duration.intToDurationInt

case class Position(x: Int, y: Int)

abstract class Robot(val id: String,
  // the side of the conflict
  val side: String,
  var position: Position,
  var life: Int)
  extends Actor {

  val responseTime: Duration
  val initialDelay = 10 milliseconds
  val sightDistance: Int;

  /**
   * Robots in sight of the given one.
   */
  private var neighbors = Set[(ActorRef, Position)]();

  private var schedule: Cancellable = null

  /**
   * Schedule periodic execution of "act" method.
   */
  override def preStart = {
    schedule = context.system.scheduler.schedule(initialDelay, responseTime)(Robot.this.act)
  }

  /**
   * It must do something every responseTime time interval.
   * Do some real actions (search, attack, move, etc.)
   */
  def act

  /**
   * Receive messages from other robots.
   */
  def receive = {
    case Start => {
      // start doing something
    }
    case Damage(q) => damage(q)
    case RobotPosition(RobotId(id, side), robot, p) => {
    }
  }

  def die {
    self ! PoisonPill
    schedule.cancel
  }

  /**
   * It cannot be overridden
   */
  private[robot_wars] final def damage(damage: Int) {
    life -= damage
    if (life < 0) {
      die
    }
  }

  /**
   *
   */
  final def newPosition(r: ActorRef, p: Position) {
    neighbors += ((r, p))
  }

  /**
   * Determine if the position p is visible by the robot.
   */
  def inSight(p: Position): Boolean =
    math.abs(position.x - p.x) < sightDistance &&
      math.abs(position.y - p.y) < sightDistance

}