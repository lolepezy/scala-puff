package net.ripe.ranges

import org.junit.Test
import scala.annotation.tailrec
import org.junit.Assert

case class Range(val start: Int, val end: Int)

object OverlapChecker {

  def compareRanges(r1: Range, r2: Range) =
    if (r1.start < r2.start) true
    else if (r1.start > r2.start) false
    else r1.end < r2.end

  def report(r1: Range, r2: Range) = "overlap" + r1 + " " + r2

  def reportOverlaps(ranges: List[Range]) = {
    @tailrec
    def searchOverlaps(rs: List[Range], overlaps: List[String] = List[String]()): List[String] = {
      rs match {
        case Nil => overlaps
        case r1 :: rest => {
          val o = rest.takeWhile(r2 => r2.start <= r1.end).map(r2 => report(r1, r2))
          searchOverlaps(rest, o ::: overlaps)
        }
      }
    }
    searchOverlaps(ranges.sortWith(compareRanges))
  }
}

class OverlapTest {
  
  @Test
  def test {
    val ranges = List(Range(11, 20), Range(21, 30), Range(11, 30), Range(31, 100))
    val overlaps = OverlapChecker.reportOverlaps(ranges)

    Assert.assertEquals(2, overlaps.size)
    Assert.assertTrue(overlaps.contains(OverlapChecker.report(Range(11, 20), Range(11, 30))))
    Assert.assertTrue(overlaps.contains(OverlapChecker.report(Range(11, 30), Range(21, 30))))
  }
}
