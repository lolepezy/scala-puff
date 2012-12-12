package net.projecteuler.lolepezy

import org.junit.Test

class Problem48 {

  @Test
  def solve() {

    val begin = System.currentTimeMillis()

    val sumStr = (1 to 1000).map(n => Math.pow(n, n)).sum.toString
    val s = sumStr.substring(sumStr.length - 10)

    val end = System.currentTimeMillis()

    println(s + ", duration " + (end - begin))

  }

}