package nokia.rovers

import scala.io.Source

object RoverApp extends App {

  val fileName = args(0)
  if (fileName == null) {
    Console.println("Usage: scala fileName.scala <inputFile>");
    sys.exit(1);
  }

  val lines = Source.fromFile(fileName).getLines.toList
  val nasaData = NASAData.readData(lines);
  val lastPositions = nasaData.getLastPositions
  lastPositions.foreach(println)
}


