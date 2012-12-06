package net.projecteuler.lolepezy

import org.junit.Test
import org.junit.Assert._

class PrimesTest {

  //  @Test
  def testPrimes {
    //    println(Primes.primes(10))
    //    println(Primes.primes(20))
    //    println(Primes.primes(100))
    //    //        println(primes1(2000).size)
    //    //        println(primes1(20000).size)
    //    //        println(primes1(200000).size)
    //    println(Primes.primes(2 * 1000 * 1000).exists(!Primes.isPrime(_)))
    //
    //    println("answer = " + Primes.primes(2000000).foldLeft(0L)(_ + _))
  }

  @Test
  def testDivisors {
    assertEquals(Map(2 -> 1), Primes.divisors(2))
    assertEquals(Map(5 -> 1), Primes.divisors(5))
    assertEquals(Map(5 -> 1, 2 -> 1), Primes.divisors(10))
    assertEquals(Map(2 -> 2), Primes.divisors(4))
    assertEquals(Map(2 -> 4), Primes.divisors(16))
    assertEquals(Map(3 -> 3), Primes.divisors(27))
    assertEquals(Map(2 -> 2, 3 -> 2, 5 -> 1, 7 -> 1), Primes.divisors(2 * 3 * 5 * 2 * 3 * 7))
  }

}