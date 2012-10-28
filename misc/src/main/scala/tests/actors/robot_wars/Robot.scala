package tests.actors.robot_wars

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.PoisonPill
import akka.actor.actorRef2Scala
import akka.util.Duration
import akka.util.duration.intToDurationInt

/**
 * The basic position class.
 *
 * TODO Implement
 *
 */
case class Position(x: Int, y: Int)

/**
 * Base class for all robot actors. Defines basic functionality
 * for message interchanging and move/attack/die cycle. Some of the
 * methods like, damage, die or move cannot be overridden.
 *
 * The concept of time is implemented as periodical call for "act" method,
 * where robot is free to do anything. Any interaction with other robots
 * is done by sending them corresponding messages.
 *
 * Every robots move is followed by RobotPosition message sent to
 * local zone dispatcher.
 *
 */
abstract class Robot(val id: String,
  // the side of the conflict
  val side: String,
  var position: Position,
  var life: Int,
  var positionDispatcher: ActorRef)
  extends Actor {

  val responseTime: Duration
  val initialDelay = 10 milliseconds
  val sightDistance: Int;

  /**
   * Robots in sight of the given one.
   */
  protected var neighbors = Set[(Position, RobotId, ActorRef)]();

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
   * Move to new position according to some internal decision.
   */
  def makeNextMove: Position

  /**
   * Do nothing by default.
   */
  def customMessage(m: AnyMessage): Receive = {
    case _ =>
  }

  /**
   * Receive messages from other robots and dispatchers.
   */
  def receive = {
    case Damage(q) =>
      damage(q)
    case RobotPosition(rid @ RobotId(id, side), robot, p) =>
      neighbors += ((p, rid, robot))
    case NewDispatcher(pd) =>
      positionDispatcher = pd
    case CustomMessage(m) =>
      customMessage(m)
  }

  private[robot_wars] final def die {
    self ! PoisonPill
    schedule.cancel
  }

  private[robot_wars] final def move = {
    val newPosition = makeNextMove
    position = newPosition
    positionDispatcher ! RobotPosition(RobotId(id, side), self, newPosition)
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
   * Determine if the position p is visible by the robot.
   */
  private[robot_wars] final def inSight(p: Position): Boolean =
    math.abs(position.x - p.x) < sightDistance &&
      math.abs(position.y - p.y) < sightDistance

}