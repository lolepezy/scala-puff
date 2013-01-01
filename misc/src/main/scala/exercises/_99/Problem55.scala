package exercises._99

import org.junit.Test
import org.junit.Assert._
import TreeHelper._
import org.junit.Test

class Problem55 {

  @Test def check() {
    assertEquals(0, countAndBalance(End)._1)
    assertEquals(1, countAndBalance(Tree(1, End, End))._1)
    assertEquals(3, countAndBalance(Tree(1, Tree(22, End, End), Tree(3, End, End)))._1)

    assertTrue(countAndBalance(End)._2)
    assertTrue(countAndBalance(Tree(1, End, End))._2)
    assertTrue(countAndBalance(Tree(1, Tree(2, End, End), End))._2)
    assertTrue(countAndBalance(Tree(1, Tree(22, End, End), Tree(3, End, End)))._2)

    assertFalse(countAndBalance(Tree(1, Tree(2, Tree(3, End, End), End), End))._2)
  }

  def checkBalancedCreation(n: Int) = {
    val tree = createBalanced(n);
    val c = countAndBalance(tree)
    assertEquals(n, c._1)
    assertTrue(c._2)
  }

  @Test def checkBalances() {
    for (i <- 0 to 100)
      checkBalancedCreation(i)
  }

}