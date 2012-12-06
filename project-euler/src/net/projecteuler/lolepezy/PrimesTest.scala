package net.projecteuler.lolepezy

import org.junit.Test

class PrimesTest {

    @Test
  def testPrimes {
            println(Primes.primes(10))
            println(Primes.primes(20))
            println(Primes.primes(100))
    //        println(primes1(2000).size)
    //        println(primes1(20000).size)
    //        println(primes1(200000).size)
    println(Primes.primes(2 * 1000 * 1000).exists(!Primes.isPrime(_)))

    println("answer = " + Primes.primes(2000000).foldLeft(0L)(_ + _))
  }
}