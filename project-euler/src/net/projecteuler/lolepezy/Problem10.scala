package net.projecteuler.lolepezy

import org.junit.Test

class Problem10Test {

  def primes1(n: Int) = {
    var i = 3
    var primes = List[Int](2, 3, 5, 7, 11)
    primes = primes.filter(_ < n)
    while (i < n) {
      if (i % 2 != 0 && i % 3 != 0 && i % 5 != 0 && i % 7 != 0 && i % 11 != 0)
        if (!primes.exists(i % _ == 0)) {
          primes = i :: primes
//          println("i=" + i)
        }
      i += 2
    }
    primes
  }

  @Test
  def testPrimes {
            println(primes1(10))
            println(primes1(20))
    //        println(primes1(2000).size)
    //        println(primes1(20000).size)
    //        println(primes1(200000).size)
//    println(primes1(2 * 1000 * 1000).size)

//    println("answer1 = " + primes1(2000000).foldLeft(0L)(_ + _))
    println("answer2 = " + Primes.primes(2000000).foldLeft(0L)(_ + _))
  }

}