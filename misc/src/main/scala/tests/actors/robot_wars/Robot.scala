package tests.actors.robot_wars

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.PoisonPill
import akka.actor.actorRef2Scala
import akka.util.Duration
import akka.util.duration.intToDurationInt
import akka.event.Logging
import akka.actor.ActorLogging

/**
 * The basic position class.
 */
case class Position(x: Int, y: Int)

/**
 * Auxiliary structure to unify typical usage of robots in messages, etc.
 */
case class RobotInfo(
  val position: Position,
  val robotId: RobotId,
  val actor: ActorRef)

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
  var positionDispatcher: ActorRef,
  val tracker: ActorRef)
  extends Actor with ActorLogging {

  val responseTime: Duration
  val initialDelay = 10 milliseconds
  val sightDistance: Int;

  /**
   * Robots in sight of the given one.
   */
  protected var neighbors = Set[RobotInfo]();

  private var schedule: Cancellable = null

  /**
   * Schedule periodic execution of "act" method.
   * We don't call it directly from here to avoid
   * race conditions but instead send the message Act
   * to robot itself.
   */
  override def preStart = {
    schedule = context.system.scheduler.schedule(initialDelay, responseTime)({
      self ! Act
    })
    // send initial position to the dispatcher 
    positionDispatcher ! RobotPosition(RobotId(id, side), self, position)
    log.debug("Starting robot " + id);
  }

  /**
   * It must do something every responseTime time interval.
   * Do some real actions (search, attack, move, etc.)
   *
   * TODO Think of restricting maximal damage that can be done
   * in one "act" call. Otherwise it's easy to create a robot,
   * killing anyone in one action.
   */
  protected def act

  /**
   * Move to new position according to some internal decision.
   */
  protected def makeNextMove: Position

  /**
   * Do nothing by default.
   */
  protected def customMessage(m: AnyMessage): Receive = {
    case _ =>
  }

  def logged(message: Any)(f: Receive): Unit = {
    log.debug("id= " + id + " message = " + message)
    f(message)
  }

  /**
   * Receive messages from other robots and dispatchers.
   */
  def receive = {
    case m @ _ => logged(m) {
      case Act => act
      case Damage(f, t, q) => damage(q)
      case RobotPosition(rid @ RobotId(id, side), robot, p) => {
        if (inSight(p))
          neighbors += RobotInfo(p, rid, robot)
        else
          neighbors -= RobotInfo(p, rid, robot)
      }
      case Dead(robotId, position) =>
        neighbors = neighbors.filterNot(_.robotId == robotId)
      case NewDispatcher(pd) => positionDispatcher = pd
      case CustomMessage(m) => customMessage(m)
    }
  }

  private[robot_wars] final def die {
    self ! PoisonPill
    schedule.cancel
    bcast(Dead(RobotId(id, side), position))
    log.debug("Robot id=" + id + " is now dead")
  }

  private[robot_wars] final def move = {
    val newPosition = makeNextMove
    position = newPosition
    bcast(RobotPosition(RobotId(id, side), self, newPosition))
  }

  /**
   * It cannot be overridden
   */
  private[robot_wars] final def damage(damage: Int) {
    if (life < 0) {
      // it's already dead, so do nothing
    } else {
      life -= damage
      if (life < 0) {
        die
      }
    }
  }

  /**
   * Determine if the position p is visible by the robot.
   */
  private[robot_wars] final def inSight(p: Position): Boolean =
    math.abs(position.x - p.x) < sightDistance &&
      math.abs(position.y - p.y) < sightDistance

  /**
   * Send the message both to tracker and dispatcher.
   */
  private[this] def bcast(p: RobotMessage) = {
    positionDispatcher ! p
    tracker ! p
  }

  /**
   * Filter out only enemies from all neighbors.
   */
  protected final def enemies = neighbors.filter(_.robotId.side != side)

  protected final def myId = RobotId(id, side)

}