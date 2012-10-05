package nokia.rovers

import scala.io.Source

sealed abstract case class Cardinal(
  val left: Cardinal,
  val right: Cardinal)

case object N extends Cardinal(W, E)
case object S extends Cardinal(E, W)
case object E extends Cardinal(N, S)
case object W extends Cardinal(S, N)

sealed abstract case class Move
case object Left extends Move
case object Right extends Move
case object Forward extends Move

/**
 * The generic position definition.
 */
class Position(var x: Int, var y: Int, var cardinal: Cardinal) {
  def move(m: Move) = m match {
    case Left => new Position(x, y, cardinal.left)
    case Right => new Position(x, y, cardinal.right)
    case Forward => cardinal match {
      case N => new Position(x, y + 1, cardinal)
      case S => new Position(x, y - 1, cardinal)
      case E => new Position(x + 1, y, cardinal)
      case W => new Position(x - 1, y, cardinal)
    }
  }
}

/**
 * The rover is represented by initial position and list of movements.
 */
class Rover(
  val roverId: Int,
  val initialPosition: Position,
  val moves: List[Move],
  val boudaries: (Int, Int)) {

  def makeAllMoves(pos: Position, mov: List[Move], previousRovers: List[Position]): Position =
    mov match {
      case Nil => pos
      case m :: rest => {
        val nextPos = pos.move(m)
        if (nextPos.x < 0 || nextPos.x >= boudaries._1 ||
          nextPos.y < 0 || nextPos.y >= boudaries._2)
          throw new Exception("The position")

        // if we are at the same position as one of the previous rovers,
        // then we should throw an error
        val crashes = previousRovers.filter(p => nextPos.x == p.x && nextPos.y == p.y)
        if (crashes.isEmpty)
          makeAllMoves(nextPos, rest, previousRovers)
        else
          throw new Exception("There was the crash on the position: " + crashes.mkString)
      }
    }

  def lastPosition(previousRovers: List[Position]) =
    makeAllMoves(initialPosition, moves, previousRovers)
}

object Main extends App {

  def readData(lines : List[String]) = {
    val Array(maxX, maxY) = lines.head.split("\\s") map (_.toInt)

    // TODO here we should actually throw something more appropriate 
    assert(maxX > 0)
    assert(maxY > 0)

    val roverLines = lines.tail

    // even lines are initial positions
    var i = 0
    val initialPositions = roverLines
      .filter(z => { val k = i % 2; i += 1; k == 0 })
      .map(line => {
        val Array(x, y, d) = line split "\\s" map (_.trim)
        new Position(x.toInt, y.toInt, d match {
          case "N" => N
          case "S" => S
          case "W" => W
          case "E" => E
        })
      }).toList

    // odd lines are moves  
    i = 0
    val moves = roverLines
      .filter(x => { val k = i % 2; i += 1; k == 1 })
      .map(_.trim.map(_ match {
        case 'L' => Left
        case 'R' => Right
        case 'M' => Forward
      }).toList)

    assert(initialPositions.size == moves.size)

    (initialPositions, moves, maxX, maxY)
  }

  def getLastPostions() = {
    // create rovers from initial positions and move lists 
    var index = 0
    val rovers = initialPositions zip moves map (
      ipM => new Rover({ val i = index; index += 1; i }, ipM._1, ipM._2, (maxX, maxY)))

    // map them to last positions
    var previousRovers = List[Position]()
    (for (r <- rovers) yield {
      val lastP = r.lastPosition(previousRovers)
      previousRovers = lastP :: previousRovers
      lastP
    }).toList
  }

  val fileName = args(0)
  if (fileName == null) {
    Console.println("Usage: scala fileName.scala <inputFile>");
    sys.exit(1);
  }

  val lines = Source.fromFile(fileName).getLines.toList
  val (initialPositions, moves, maxX, maxY) = readData(lines)
  val lastPostions = getLastPostions()
}


