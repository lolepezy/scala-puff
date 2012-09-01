package tests
import org.junit.runner.RunWith
import org.junit._

import Assert._

class Exercise1 {
  def sqrt(initial: Double): Double = {
    def sqrtIter(x: Double, iteration: Int): Double = {
      if (isGoodEnough(x, iteration)) x
      else sqrtIter(improve(x), iteration + 1);
    }

    def isGoodEnough(x: Double, iteration: Int): Boolean = {
      math.abs(x * x - initial) < 1e-8 ||
        iteration > 100
    }
    def improve(x: Double): Double = {
      (x + initial / x) / 2.
    }
    sqrtIter(initial, 0)
  }

  def whileCheck() {
    var n = 0;
    while (n < 10) {
      println("n = " + n)
      n = n + 1
    }
  }

}

class SampleTest {

  @Test def sample() {
    println(new Exercise1().sqrt(2))
    println(math.sqrt(2))
    assertEquals(42, 6 * 7)
    new Exercise1().whileCheck()
  }

  @Test def array() {
    val N = 33;
    val bits = Array.ofDim[Boolean](N, N, N, N);
    val x = bits(0)(0)(0)(0)
    x;
  }

  @Test def perfTest1 {
    var start = System.currentTimeMillis();
    for (x <- 0 until 10) {
      val m = new scala.collection.mutable.HashMap[Int, Int];
      var i = 0;
      while (i < 100000) { i = i + 1; m.put(i, i); };
    }
    var end = System.currentTimeMillis();
    println(end - start);
  }

  @Test def perfTest2 {
    var start = System.currentTimeMillis();
    for (x <- 0 until 10) {
      val m = new java.util.HashMap[Int, Int];
      var i = 0;
      while (i < 100000) { i = i + 1; m.put(i, i); };
    }
    var end = System.currentTimeMillis();
    println(end - start);
  }

  @Test def perfTest3 {
    var start = System.currentTimeMillis();
    for (x <- 0 until 10) {
      val m = new java.util.concurrent.ConcurrentHashMap[Int, Int];
      var i = 0;
      while (i < 100000) { i = i + 1; m.put(i, i); };
    }
    var end = System.currentTimeMillis();
    println(end - start);
  }

}

