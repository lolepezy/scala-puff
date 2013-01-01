package robot_wars

import akka.actor.ActorSystem
import akka.actor.Props

object Battle extends App {

  def generateTanks(side: String, n: Int) = {
    (for (t <- (0 until n)) yield {
      val id = "tank_" + side + "_" + t
      // TODO Generate some random positions or spawn all 
      // robots from the same place (like base or something)
      val pos = Position(t, t)
      system.actorOf(Props(new TankRobot(id, side, pos, dispatcher, tracker)), name = "Tank" + side + t)
    }).toList
  }

  val system = ActorSystem("Battle")
  val dispatcher = system.actorOf(Props(new Dispatcher(-100, 100, -100, 100)), name = "MainDispatcher")
  val tracker = system.actorOf(Props(new Tracker("./battle_track.log")), name = "MainTracker")

  val tanks1 = generateTanks("side1", 20)
  val tanks2 = generateTanks("side2", 5)

  Thread.sleep(60000)
  system.shutdown

}