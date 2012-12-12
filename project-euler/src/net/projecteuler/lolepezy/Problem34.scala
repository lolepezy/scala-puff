package net.projecteuler.lolepezy

import org.junit.Test
import scala.math.BigInt._

class Problem34 {
  @Test
  def solve() {

    val f = (3 to 1000 * 1000).filter(n => n.toString.map(Math.digit).map(Math.digFact(_)).sum == n).sum

    println(f)
  }
}