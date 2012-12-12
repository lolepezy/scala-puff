package net.projecteuler.lolepezy

import org.junit.Test
import scala.math.BigInt._

class Problem52 {
  @Test
  def solve() {

    var n = 10
    var found = false
    while (!found) {
      n += 1
      val s1 = n.toString.toSet
      found = (2 to 6).map(k => (k * n).toString.toSet).foldLeft(true)((result, x) => result && x == s1)
    }

    println(n)
  }
}