package net.projecteuler.lolepezy

import org.junit.Test
import scala.annotation.tailrec

object Primes {

  def isPrime(n: Long): Boolean = {
    var divisor = 2;
    var hasDivisor = false
    while (!hasDivisor && divisor * divisor < n) {
      hasDivisor = n % divisor == 0
      divisor += 1
    }
    !hasDivisor
  }

  /**
   * Returns the list of all primes below n.
   */
  def primes(n: Long) = {
    val primes = new Array[Int](n.toInt / 2)
    var currentPos = 0;

    var initialPrimes = List[Int](2, 3, 5, 7, 11)
    var candidate = 13
    initialPrimes = initialPrimes.filter(_ < n)
    initialPrimes.copyToArray(primes)
    currentPos += initialPrimes.size

    while (candidate < n) {
      var x = primes(0);
      var j = 0;
      var hasDivisor = false
      while (x * x < candidate && !hasDivisor && j < currentPos) {
        x = primes(j)
        hasDivisor = candidate % x == 0
        if (!hasDivisor)
          j += 1
      }
      if (!hasDivisor) {
        primes(currentPos) = candidate
        currentPos += 1
      }
      candidate += 2
    }
    // filter out zeros
    primes.filter(_ > 0).toList
  }

  /**
   * Return the map(divider, power of the divider) for n.
   */
  def divisors(n: Long) = {

    def getDivisorPower(x: Long, d: Int) = {
      var value = x
      var divised = true;
      var power = 0
      do {
        val (div, mod) = (value / d, value % d)
        if (mod == 0 && div > 0) {
          power += 1
          value = div
        } else
          divised = false
      } while (divised)
      (value, power)
    }

    def getDivisorMap(v: Long, divs: List[Int]): List[(Long, Int)] = {
      if (v == 1)
        List()
      else {
        divs match {
          case Nil => List((v, 1))
          case x :: rest => {
            if (v % x == 0) {
              val (z, power) = getDivisorPower(v, x)
              (x.toLong, power) :: getDivisorMap(z, rest)
            } else
              getDivisorMap(v, rest)
          }
        }
      }
    }
    
    val value = math.abs(n)
    val primeDivisors = primes(value).filter(_ * 2 <= value)
    Map[Long, Int](getDivisorMap(value, primeDivisors): _*)
  }

}