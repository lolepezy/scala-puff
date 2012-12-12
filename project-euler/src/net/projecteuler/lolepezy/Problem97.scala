package net.projecteuler.lolepezy

import org.junit.Test
import scala.math.BigInt._

class Problem97 {
  @Test
  def solve() {

    var x: BigInt = 1
    val z = ((x << 7830457) * 28433 + 1) % Math.pow(10, 10)

    println(z)
  }
}