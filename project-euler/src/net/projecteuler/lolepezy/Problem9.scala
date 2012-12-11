package net.projecteuler.lolepezy

import org.junit.Test

class Problem9 {

  @Test
  def solve() {

    val x = for (
      a <- (1 to 998);
      b <- (a + 1 to 1000 - a);
      c = 1000 - a - b;
      if (a * a + b * b == c * c)
    ) yield a * b * c

    println(x.head)
  }

}