package net.projecteuler.lolepezy

import org.junit.Test

class Problem16 {

  @Test
  def solve() {

    val begin = System.currentTimeMillis()
    val p = Math.pow(Math.pow(1024 * 1024, 10), 5)
    val end = System.currentTimeMillis()
    
    val n = p.toString.map({
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
    }).sum

    println(n, ", duration " + (end - begin))

  }

}