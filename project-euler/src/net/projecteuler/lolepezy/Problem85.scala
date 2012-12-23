package net.projecteuler.lolepezy

import org.junit.Test

class Problem85 {
  @Test
  def solve() {
    val min = (for (n <- (1 to 1500); m <- (1 to n)) yield 
        (math.abs(2000000 - n * m * (n + 1L) * (m + 1L) / 4), n * m)
    ).foldLeft((1000000000L, 0))((r, s) => if (s._1 < r._1) s else r)

    println(min._2)

  }

}