package exercises._99
import org.junit.Test
import org.junit.Test

class Problem40 {

  def isPrime(n: Long) =
    List.range(2, n / 2 + 1).filter(i => n % i == 0).isEmpty

  def isPrimeFast(n: Long) = {
    var i = 1L;
    var prime = false
    while (i < n / 2 + 1 && !prime) {
      if (n % i == 0)
        prime = true
    }
    prime
  }

  def divisors(n: Long) =
    List.range(2, n / 2 + 1).filter(i => n % i == 0)

  def goldbach(m: Long) =
    List.range(1, m / 2 + 1).filter(i => isPrime(i) && isPrime(m - i)).map(i => (i, m - i))

  def fastGoldbach(m: Long) = {
    var i = 1L;
    var result = List[Tuple2[Long, Long]]()
    while (i < m / 2 + 1) {
      val r = m - i
      if (isPrimeFast(i) && isPrimeFast(m - i))
        result = (i, m - i) :: result
      i += 1
    }
    result
  }

  @Test def run() {
    val x = goldbach(1000);
    println(x)
  }

  @Test def fast() {
    val x = fastGoldbach(1000);
  }
}