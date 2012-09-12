package tests.problems

import org.junit.Test
import org.junit.Assert._

import TreeHelper._

class Problem56 {

  @Test def checkBalances() {
    assertTrue(isSymmetric(End))
    assertTrue(isSymmetric(Tree[Int](1, End, End)))
    assertFalse(isSymmetric(Tree[Int](1, End, Tree[Int](2, End, End))))
    assertTrue(isSymmetric(
      Tree[Int](1,
        Tree[Int](2, End, End),
        Tree[Int](3, End, End))))

    assertTrue(isSymmetric(
      Tree[Int](1,
        Tree[Int](2,
          Tree[Int](4, End, End),
          End),
        Tree[Int](3,
          End,
          Tree[Int](5, End, End)))))

    assertFalse(isSymmetric(
      Tree[Int](1,
        Tree[Int](2,
          Tree[Int](4, End, End),
          End),
        Tree[Int](3,
          Tree[Int](6, End, End),
          Tree[Int](5, End, End)))))
  }

}