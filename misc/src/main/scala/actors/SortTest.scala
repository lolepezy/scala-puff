package actors

import org.junit.Assert._
import org.junit.Test
import scala.annotation.tailrec

class SortTest {

  @Test
  def testQSort1 {
    assertTrue(isSorted(QSort(Array(4, 3, 5, 1))))
    for (i <- (10 to 1000))
      assertTrue(isSorted(QSort(generateArray(i))))
  }

  def isSorted[T <% Ordered[T]](a: Array[T]) = {
    @tailrec
    def _sorted(offset: Int): Boolean = {
      if (offset == a.length - 1) true
      else if (a(offset) <= a(offset + 1))
        _sorted(offset + 1)
      else false
    }
    _sorted(0)
  }

  def generateArray(size: Int) = {
    val r = new scala.util.Random
    (0 to size) map (x => r.nextDouble) toArray
  }

}