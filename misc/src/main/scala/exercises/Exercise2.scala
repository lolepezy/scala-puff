package exercises

import org.junit.runner.RunWith
import org.junit._

import Assert._

class Exercise2 {
  def factorial(n: Long): Long = {
    if (n == 0) 1
    else n * factorial(n - 1);
  }
  def factorialTail(n: Long): Long = {
    def f(accum: Long, x: Long): Long = {
      if (x == 0) accum
      else f(accum * x, x - 1)
    }
    f(1, n)
  }

  @Test def run() {
    println(factorial(5));
    println(factorialTail(5));
  }
}

