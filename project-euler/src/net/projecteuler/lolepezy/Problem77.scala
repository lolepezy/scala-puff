package net.projecteuler.lolepezy

import org.junit.Test

class Problem77 {
  @Test
  def solve() {
    var (n, d) = (20, 0)
    do {
      n += 1
      d = decompose(n).length
    } while (d < 5000)
    println("n = " + n + ", d = " + d)
  }

  def decompose(n: Long) = {
    val primes = Primes.primes(n);
    def decompose1(p: Long, k: Long): List[Option[List[Long]]] = {
      primes.withFilter(_ >= p).flatMap(i => {
        val z = k - i
        if (z == 0) List(Some(List(k)))
        else if (z > 1) {
          decompose1(i, z).collect({
            case Some(x) => Some[List[Long]](i :: x)
          })
        } else List(None)
      })
    }

    decompose1(primes.head, n).collect({ case Some(dec) => dec }).toList
  }
}