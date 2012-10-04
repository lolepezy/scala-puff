package nokia.rovers

import scala.io.Source

sealed abstract case class Cardinal(
  val left: Cardinal,
  val right: Cardinal)

case object N extends Cardinal(W, E)
case object S extends Cardinal(E, W)
case object E extends Cardinal(N, S)
case object W extends Cardinal(S, N)

sealed case class Move
case object Left extends Move
case object Right extends Move
case object Forward extends Move

class Position(var x: Int, var y: Int, var direction: Cardinal) {
  def next(m: Move) = m match {
    case Left => new Position(x, y, direction.left)
    case Right => new Position(x, y, direction.right)
    case Forward => direction match {
      case N => new Position(x, y + 1, direction)
      case S => new Position(x, y - 1, direction)
      case E => new Position(x + 1, y, direction)
      case W => new Position(x - 1, y, direction)
    }
  }
}

class Rover(val initialPosition: Position, moves: List[Move])

object Main extends App {
  val fileName = args(0)
  if (fileName == null) {
    Console.println("Usage: scala fileName.scala <inputFile>");
    sys.exit(1);
  }
  val lines = Source.fromFile(fileName).getLines.toList

  val Array(x, y) = lines.head.split("\\s") map (_.toInt)

  // TODO here we should actually throw something more appropriate 
  assert(x > 0)
  assert(y > 0)

  val roverLines = lines.tail

  // even lines are initial positions
  var i = 0
  val initialPositions = roverLines.filter(z => { val k = i % 2; i += 1; k == 0 }).
    map(line => {
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
  val moves = roverLines.filter(x => { val k = i % 2; i += 1; k == 1 }).map(
    _.trim match {
      case "L" => Left
      case "R" => Right
      case "M" => Forward
    }).toList

  assert(initialPositions.size == moves.size)

  
  
}

