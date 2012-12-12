package net.projecteuler.lolepezy

import org.junit.Test
import scala.math.BigInt._

class Problem29 {
  @Test
  def solve() {
    val s = (for (a <- 2 to 100; b <- 2 to 100) yield a.pow(b)).distinct.size
    println(s)
  }
}