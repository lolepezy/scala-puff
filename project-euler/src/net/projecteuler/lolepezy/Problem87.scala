package net.projecteuler.lolepezy

import org.junit.Test
import java.util.HashSet
import scala.collection.mutable.BitSet

class Problem87 {
  @Test
  def solve() {

    val max = 50 * 1000 * 1000
    //    val max = 5 * 10

    val maxPrime = math.sqrt(max - 24).toInt + 1

    val primes = Primes.primes(maxPrime)
    val primes2 = primes.map(i => i * i.toLong)
    val primes3 = primes.map(i => i * i * i.toLong)
    val primes4 = primes2.map(i => i * i)

    println(primes.size)

    val b = new BitSet(max)
    primes2.foreach(p2 => primes3.foreach(p3 => primes4.foreach(p4 => {
      val n = p2 + p3 + p4
      if (n < max)
        b.add(n.toInt)
    })))

    println(b.size)
    //    println(x.toSet.size)

  }

}