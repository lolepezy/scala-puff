package nokia.rovers

import scala.io.Source

object RoverApp extends App {

  if (args.length == 0) {
    Console.println("Usage: scala fileName.scala <inputFile>");
    sys.exit(1);
  }

  val fileName = args(0)
  val lines = Source.fromFile(fileName).getLines.toList
  val nasaData = NASAData.readData(lines);
  val lastPositions = nasaData.getLastPositions
  lastPositions.foreach(lp => println(lp.x + " " + lp.y + " " + lp.cardinal))
}


