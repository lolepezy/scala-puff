package tests.actors.robot_wars

import akka.util.Duration
import akka.util.duration._
import akka.actor.ActorRef
import scala.util.Random

class TankRobot(
  override val id: String,
  override val side: String,
  val pos: Position,
  dispatcher: ActorRef)
  extends Robot(id, side, pos, 100, dispatcher) {

  // initial characteristics
  val responseTime = 100 milliseconds
  val sightDistance = 10
  val shell = Damage(20)

  // the maximal distance at which it can move at a time
  val hop = 1

  private var enemiesToAttack = Set[RobotInfo]()
  private var track = Vector[Position]()

  private val random = new Random()

  def act {
    enemiesToAttack = enemies
    // attack with a shell
    enemiesToAttack.head.actor ! shell

    if (enemiesToAttack.isEmpty) {
      // no enemies here, move somewhere to finds ones
      move
    }

    // no need to move in case there's still enemies
    if (enemiesToAttack.tail.isEmpty) {
      // there's no more enemies
      move
    }
  }

  def makeNextMove = {
    def getNextPost: Position = {
      val nextPosition = Position(
        position.x + (if (random.nextBoolean()) hop else -hop),
        position.y + (if (random.nextBoolean()) hop else -hop))
      if (track.exists(_ == nextPosition)) {
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
      // go where there're move enemies
      getNextPost
    }
  }

}