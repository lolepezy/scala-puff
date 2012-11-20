package tests.actors.robot_wars

import akka.actor.Actor
import akka.actor.IO
import java.io.Writer
import java.io.FileWriter

/**
 * Actor that gathers all the robot movements and attacks
 * to one place for further visualization and analysis.
 */
class Tracker(val logPath: String) extends Actor {

  //  private java,File f = new File

  private var logWriter: Writer = null;

  override def preStart = {
    // initialize the log file stuff
    logWriter = new FileWriter(new java.io.File(logPath))
  }

  override def postStop = {
    logWriter.flush();
    logWriter.close();
  }

  override def receive = {
    case RobotPosition(robotId, _, position) => {
      write("move " + robotId + " " + position.x + " " + position.y)
    }
    case Damage(from, to, d) => {
      write("damage " + from.id + " " + to.id + " " + d)
    }
    case Dead(robotId, position) => {
      write("dead " + robotId.id + " " + position.x + " " + position.y)
    }
    case x @ _ => {
      // whatever
      write("unknown " + x)
    }
  }

  private def write(a: String) = {
    logWriter.write(a + "\n")
  }
}