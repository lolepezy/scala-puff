package net.projecteuler.lolepezy

import org.junit.Test

object Primes {

  def isPrime(n : Long) : Boolean = {
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
  def primes(n: Int) = {
    val primes = new Array[Int](n / 2)
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
    var m = Map[Int, Int]()
    
  }

}