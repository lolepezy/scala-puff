package tests.actors.robot_wars

import akka.util.Duration
import akka.util.duration._
import akka.actor.ActorRef
import scala.util.Random

/**
 * Tank is slow and making big damage by shells.
 */
class TankRobot(
  override val id: String,
  override val side: String,
  val pos: Position,
  dispatcher: ActorRef)
  extends Robot(id, side, pos, 100, dispatcher) {

  // initial characteristics
  val responseTime = 1000 milliseconds
  val sightDistance = 10
  val shell = Damage(20)

  // the maximal distance at which it can move at a time
  val hop = 1

  private var enemiesToAttack = Set[RobotInfo]()
  private var track = Vector[Position]()

  private val random = new Random()

  def act {
    println("act, id = " + id)
    enemiesToAttack = enemies
    // attack with a shell

    log.debug("enemiesToAttack = {}", enemiesToAttack)
    
    if (enemiesToAttack.isEmpty) {
      // no enemies here, move somewhere to finds ones
      move
    } else {
      // attack one of them
      enemiesToAttack.head.actor ! shell
      // no need to move in case there's still enemies
      if (enemiesToAttack.tail.isEmpty) {
        // there's no more enemies
        move
      }
    }
  }

  def makeNextMove = {
    def getNextPost: Position = {
      val nextPosition = Position(
        position.x + (if (random.nextBoolean()) hop else -hop),
        position.y + (if (random.nextBoolean()) hop else -hop))
      if (!track.exists(_ == nextPosition)) {
        track = track :+ (nextPosition)
        nextPosition
      } else
        getNextPost
    }

    if (enemiesToAttack.isEmpty) {
      // make some random move except for positions 
      // that were already been
      getNextPost
    } else {
      // TODO Make it more intellectual
      // go where there're more enemies
      val (x0, x1, y0, y1) = enemiesToAttack.foldLeft((0, 0, 0, 0))(
        (counts, enemy) => (
          if (enemy.position.x < position.x) counts._1 + 1 else counts._1,
          if (enemy.position.x > position.x) counts._2 + 1 else counts._2,
          if (enemy.position.y < position.y) counts._3 + 1 else counts._3,
          if (enemy.position.y > position.y) counts._4 + 1 else counts._4))
      val xhop = if (x0 > x1) -hop else hop
      val yhop = if (y0 > y1) -hop else hop
      Position(position.x + xhop, position.y + yhop)
    }
  }

}