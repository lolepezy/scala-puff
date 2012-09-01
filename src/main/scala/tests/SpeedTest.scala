package tests
import org.junit.Test

class SpeedTest {

  @Test def forSpeed {
    var cc = 0L
    while (cc < 10000000L) {
      var k = 0L
      for (i <- 1 to 1000) {
        k = k + i
      }
      cc = cc + 1
    }
  }

  @Test def whileSpeed {
    var cc = 0L
    while (cc < 10000000L) {
      var k = 0L
      var i = 1
      while (i < 1000) {
        k = k + i
        i = i + 1
      }
      cc = cc + 1
    }
  }
}