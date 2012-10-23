package tests.actors.robot_wars

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.PoisonPill
import akka.actor.actorRef2Scala
import akka.util.Duration
import akka.util.duration.intToDurationInt

case class Position(x: Int, y: Int)

/**
 * Base class for all robots.
 *
 */
abstract class Robot(
  val id: String,
  // the side of the conflict
  val side: String,
  var position: Position,
  var life: Int)
  extends Actor {

  val responseTime: Duration
  val initialDelay = 10 milliseconds

  private var schedule: Cancellable = null

  /**
   * Robots in sight of the given one.
   */
  private var neighbors = Set[ActorRef]();

  /**
   * Schedule periodic execution of "act" method.
   */
  override def preStart = {
    schedule = context.system.scheduler.schedule(initialDelay, responseTime)(this.act)
  }

  def damage(d : Int) {
    
  }
  
  /**
   * Die and stop waking up for acting
   */
  def die {
    self ! PoisonPill
    schedule.cancel
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
    case Damage(q) => {
      life -= q
      if (life <= 0)
        die
    }
    case NewPostion(p, r) => {

    }
  }

  /**
   * Determine if the position p is visible by the robot.
   */
  def inSight(p: Position): Boolean = {
    true
  }

}