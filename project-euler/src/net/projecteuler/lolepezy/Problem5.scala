package net.projecteuler.lolepezy

import org.junit.Test

class Problem5 {

  def pow(v: Long, p: Int): Long = {
    if (p == 1) v
    else v * pow(v, p - 1)
  }

  @Test
  def solve() {
    val x = (1 to 20).map(Primes.divisors(_)).foldLeft(
      Map[Long, Int]())((totalMap, divisors) => {
        divisors.map(
          divisor => {
            (divisor._1 -> (totalMap.get(divisor._1) match {
              case None => divisor._2
              case Some(z) => math.max(z, divisor._2)
            }))
          }) ++ totalMap.filterNot(z => divisors.contains(z._1))

      }).foldLeft(1L)((product, allDivisors) =>
        product * pow(allDivisors._1, allDivisors._2))
    println(x)
  }

}