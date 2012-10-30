package tests.actors.robot_wars

import akka.actor.ActorSystem
import akka.actor.Props

object Battle extends App {

  def generateTanks(side: String, n: Int) = {
    (for (t1 <- (0 until n)) yield {
      val id = "tank_" + side + "_" + t1
      // TODO Generate some random positions or spawn all 
      // robots from the same place (like base or something)
      val pos = Position(t1, t1)
      val tank = new TankRobot(id, side, pos, dispatcher)
      system.actorOf(Props(tank), name = "MainDispatcher")
    }).toList
  }

  val system = ActorSystem("Battle")
  val dispatcher = system.actorOf(Props[Dispatcher], name = "MainDispatcher")

  val tanks1 = generateTanks("side1", 10)
  val tanks2 = generateTanks("side2", 10)

  Thread.sleep(10000)
  system.shutdown

}