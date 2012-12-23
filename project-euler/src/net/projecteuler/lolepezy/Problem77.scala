package net.projecteuler.lolepezy

import org.junit.Test

class Problem77 {
  @Test
  def solve() {
    //    println(Primes.primes(10))
    //    println(getDecompositions(2, 10))
    println(decompose(10))
    println(decompose(20))

    for (n <- 10 to 100) {
      if (decompose(n).length > 5000)
        println("answer = " + n)
    }
  }

  def decompose(n: Long) = {

    val primes = Primes.primes(n);
    def decompose1(p: Long, k: Long): List[Option[List[Long]]] = {
      primes.withFilter(_ >= p).flatMap(i => {
        val z = k - i
        if (z == 0) List(Some(List(k)))
        else if (z > 1) {
          decompose1(i, z).collect({
            case Some(x) => Some({ val p: List[Long] = i :: x; p })
          })
        } else List(None)
      })
    }

    decompose1(primes.head, n).collect({
      case Some(dec) => dec
    }).toList

  }
}