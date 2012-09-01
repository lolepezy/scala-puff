package ru.hflabs.testtask
import org.junit.Test
import org.junit.Assert._

class MinSubstringsTest {

  @Test def testStringEncode() {
    val lines: Defs.LinesType = List(List("A", "B"), List("A", "C", "D"))
    val (encoded, mapping) = SuffixTreeHelper.encodeLines(lines)
    assertEquals(0.toShort, encoded(0)(0))
    assertEquals(1.toShort, encoded(0)(1))
    assertEquals(0.toShort, encoded(1)(0))
    assertEquals(2.toShort, encoded(1)(1))
    assertEquals(3.toShort, encoded(1)(2))
  }

  type Tree = SuffixTreeNode[String, Int]

  @Test def testSuffixTreeSearch() {
    /*
		tree = (B->(C->(D,F),D->(E))
		BCD
		BCF
		BDE
     */
    val tree = new Tree("B",
      List[Tree](
        new Tree("C", List[Tree](
          new Tree("D", List[Tree](), 1),
          new Tree("F", List[Tree](), 2)), 0),
        new Tree("D", List[Tree](
          new Tree("E", List[Tree](), 3)), 0)),
      0)

    assertEquals(List(), tree.find(List("A", "C", "F")))
    assertEquals(List(3), tree.find(List("D", "E")))
    assertEquals(List(0).toSet, tree.find(List("B", "C")).toSet)
    assertEquals(List(0).toSet, tree.find(List("C")).toSet)
    assertEquals(List(0).toSet, tree.find(List("B")).toSet)
    assertEquals(List(3).toSet, tree.find(List("E")).toSet)
    assertEquals(List(0, 1).toSet, tree.find(List("D")).toSet)
    assertEquals(List(3).toSet, tree.find(List("B", "E")).toSet)

    assertFalse(tree.matches(List("A", "C", "F")))
    assertTrue(tree.matches(List("D", "E")))
    assertTrue(tree.matches(List("E")))
    assertTrue(tree.matches(List("B")))
    assertTrue(tree.matches(List("C")))
    assertTrue(tree.matches(List("D")))
    assertTrue(tree.matches(List("B", "C")))
    assertTrue(tree.matches(List("B", "E")))
  }

  @Test def testSuffixTreeAdd() {
    val tree = new Tree("A", 0)

    tree.add(List("A", "B"), 1)
    assertTrue(tree.matches(List("A")))
    assertTrue(tree.matches(List("B")))
    assertTrue(tree.matches(List("A", "B")))
    assertFalse(tree.matches(List("B", "A")))

    tree.add(List("A", "B", "C"), 1)
    assertTrue(tree.matches(List("A")))
    assertTrue(tree.matches(List("B")))
    assertTrue(tree.matches(List("A", "B")))
    assertTrue(tree.matches(List("A", "B", "C")))

    tree.add(List("A", "D"), 1)
    assertTrue(tree.matches(List("A", "B")))
    assertTrue(tree.matches(List("A", "D")))
    assertTrue(tree.matches(List("A", "B", "C")))
    assertFalse(tree.matches(List("A", "B", "D")))

    tree.add(List("F"), 1)
    assertFalse(tree.matches(List("F")))
  }

  @Test def testSuffixTreeHelperCreate() {
    val lines = List(List("KILL", "BILL"), List("KILL", "SANTA"))
    val (tree, mapping) = SuffixTreeHelper.create(lines)
    assertEquals(mapping("KILL"), 0)
    assertEquals(mapping("BILL"), 1)
    assertEquals(mapping("SANTA"), 2)
    assertTrue(tree.matches(List(0, 1)))
    assertTrue(tree.matches(List(0, 2)))
  }

}