package net.projecteuler.lolepezy

import org.junit.Test
import org.junit.Assert._

class Problem20 {

  @Test
  def solve() {

    assertEquals(1, (Math ! 0).toInt)
    assertEquals(1, (Math ! 1).toInt)
    assertEquals(2, (Math ! 2).toInt)
    assertEquals(2*3, (Math ! 3).toInt)
    assertEquals(2*3*4, (Math ! 4).toInt)
    
    val begin = System.currentTimeMillis()
    
    val x = (Math ! 100).toString
    val s = x map Math.digit sum

    println(x)
    
    val end = System.currentTimeMillis()
    
    println(s + ", duration " + (end - begin))

  }

}