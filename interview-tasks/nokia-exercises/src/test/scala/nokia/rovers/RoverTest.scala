package nokia.rovers

import org.junit.Assert._
import org.junit.Test

@Test
class RoverTest {

  @Test def testCardinal() {
    assertEquals(Position(1, 1, N), Position(1, 1, E).move(Left))
    assertEquals(Position(1, 1, N), Position(1, 1, W).move(Right))
    assertEquals(Position(0, 1, N), Position(0, 0, N).move(Forward))
    assertEquals(Position(1, 0, E), Position(0, 0, E).move(Forward))
    assertEquals(Position(0, 1, W), Position(1, 1, W).move(Forward))
  }

  @Test def testExample() {
    val lines = List("5 5", "1 2 N", "LMLMLMLMM", "3 3 E", "MMRMMRMRRM")
    val nasaData = NASAData.readData(lines);
    val lastPositions = nasaData.getLastPositions
    assertEquals(List(Position(1, 3, N), Position(5, 1, E)), lastPositions)
    lastPositions.foreach(println)
  }

  @Test def testReadData() {
    val lines = List("5 5", "1 2 N", "LMLMLMLMM", "3 3 E", "MMRMMRMRRM")
    val nasaData = NASAData.readData(lines);
    assertEquals(List(Position(1, 2, N), Position(3, 3, E)), nasaData.initialPositions)
    assertEquals(List(
      List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward),
      List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)),
      nasaData.moves)
    assertEquals(5, nasaData.maxX)
    assertEquals(5, nasaData.maxY)
  }
}