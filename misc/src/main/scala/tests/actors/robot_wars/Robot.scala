package tests.actors.robot_wars

import akka.actor.Actor
import akka.actor.PoisonPill
import akka.util.duration._
import akka.util.Duration
import akka.actor.Cancellable

case class Position(x: Int, y: Int)

abstract class Robot(var position: Position, var life: Int)
  extends Actor {

  val responseTime: Duration
  val initialDelay = 10 milliseconds

  private var schedule: Cancellable = null

  /**
   * Schedule periodic execution of "act" method.
   */
  override def preStart = {
    schedule = context.system.scheduler.schedule(initialDelay, responseTime)(this.act)
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
  }

}