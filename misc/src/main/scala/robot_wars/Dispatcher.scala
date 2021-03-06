package robot_wars

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

/**
 * Dispatcher is used to pass position messages between robots.
 *
 * 1) Each robot sends messages "my new position is (x,y)" to the main dispatcher
 * 2) Main dispatcher resends that message to local dispatcher, i.e. dispatcher,
 * responsible for just local zone.
 * 2) Local dispatcher notifies all robots in the local zone.
 *
 * Here "all robots in the local zone" means every robot whose zone of vision intersects
 * with the local zone.
 *
 */
class Dispatcher(
  val zoneX0: Int,
  val zoneX1: Int,
  val zoneY0: Int,
  val zoneY1: Int) extends Actor {

  /**
   * Robots in the local zone.
   */
  var zoneRobots = Set[RobotInfo]()

  /**
   * Children dispatchers (if exist).
   */
  var children = List[ActorRef]()

  /**
   * Maximal distance at which robots can see each other.
   */
  private val maxPossibleSight = 20;

  /**
   * Maximal robots per zone.
   */
  private val maxZoneRobotsNumber = 100;

  def receive = {
    case np @ RobotPosition(rid @ RobotId(id, side), robot, p) => {
      if (children.isEmpty) {
        // there's no children dispatchers, so we'll 
        // notify robots in zone ourselves
        zoneRobots.foreach(zr =>
          if (closePositions(zr.position, p) && zr.robotId.id != id) zr.actor ! np)

        // if robot came to the zone, add him to the local 
        // list, if it's out of the zone --- remove it
        zoneRobots = if (zoneX0 <= p.x && p.x < zoneX1 &&
          zoneY0 <= p.y && p.y < zoneY1)
          zoneRobots + RobotInfo(p, rid, robot)
        else
          zoneRobots - RobotInfo(p, rid, robot)

        // if there's too much robots we must create descendant dispatchers 
        // and re-send the message to them
        val zoneRobotsCount = zoneRobots.size
        if (zoneRobotsCount > maxZoneRobotsNumber)
          createChildDispatchers

      } else
        children.foreach(_ forward np)
    }
    case dead @ Dead(robotId, position) => {
      // remove it from the local zone
      // TODO Implement it in more effective fashion
      zoneRobots = zoneRobots.filterNot(r => r.robotId == robotId)
      zoneRobots.foreach(_.actor forward dead)
    }
    case _ => {
      // log problem here
    }
  }

  /**
   * Used only for dividing zone into two zones.
   */
  private def setZoneRobots(robots: Set[RobotInfo]) = {
    zoneRobots = robots
    zoneRobots.foreach(_.actor ! NewDispatcher(self))
  }

  private def closePositions(p1: Position, p2: Position) =
    (math.abs(p1.x - p2.x) <= maxPossibleSight
      && math.abs(p1.y - p2.y) <= maxPossibleSight)

  /**
   *
   */
  private def createChildDispatchers {
    val xm = (zoneX0 + zoneX1) / 2
    val ym = (zoneY0 + zoneY1) / 2
    val (xc1, xc2, yc1, yc2) = zoneRobots.foldLeft((0, 0, 0, 0))(
      (counts, zr) => {
        val cx1 = if (zr.position.x < xm) 1 else 0
        val cx2 = if (zr.position.x >= xm) 1 else 0
        val cy1 = if (zr.position.y < ym) 1 else 0
        val cy2 = if (zr.position.y >= ym) 1 else 0
        (counts._1 + cx1, counts._2 + cx2, counts._3 + cy1, counts._4 + cy2)
      })

    val (d1, d2, (zr1, zr2)) = if (math.abs(xc1 - xc2) < math.abs(yc1 - yc2)) {
      // split zone by X, i.e. create two child dispatchers
      // in two adjacent zones
      (new Dispatcher(zoneX0, xm, zoneY0, zoneY1),
        new Dispatcher(xm, zoneX1, zoneY0, zoneY1),
        zoneRobots partition (_.position.x < xm))
    } else {
      // split zone by Y
      (new Dispatcher(zoneX0, zoneX1, zoneY0, ym),
        new Dispatcher(zoneX0, zoneX1, ym, zoneY1),
        zoneRobots partition (_.position.y < ym))
    }

    d1.setZoneRobots(zr1)
    d2.setZoneRobots(zr2)
    children = List(context.system.actorOf(Props(d1)), context.system.actorOf(Props(d2)))
    zoneRobots = Set[RobotInfo]()
  }

}