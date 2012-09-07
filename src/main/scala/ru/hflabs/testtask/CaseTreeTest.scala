package ru.hflabs.testtask
import org.junit.Ignore
import org.junit.Test
import org.junit.Assert._

class CaseTreeTest {

  @Test def testSuffixTreeAdd() {
    val tree = SuffixXNode("A", List())
    TreeFinder.add(tree, List("A", "B"), 1)

    assertEquals(SuffixXNode("A", List(SuffixLeafNode("B", 1))), tree)

    TreeFinder.add(tree, List("A", "B", "C"), 3)
    assertEquals(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      tree)

    TreeFinder.add(tree, List("A", "D"), 4)
    assertEquals(
      SuffixXNode("A",
        List(
          SuffixLeafNode("D", 4),
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      tree)

    TreeFinder.add(tree, List("A", "B", "F"), 5)
    assertEquals(
      SuffixXNode("A",
        List(
          SuffixLeafNode("D", 4),
          SuffixXNode("B", List(SuffixLeafNode("F", 5), SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      tree)

  }

  @Test def testSuffixTreeFind() {

    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      List("A", "B", "C")).toSet)

    assertEquals(Set(1, 3), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      List("A", "B")).toSet)

    assertEquals(Set(1, 3, 4), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1), 
          SuffixLeafNode("D", 4))),
      List("A")).toSet)
      
    assertEquals(Set(1, 3), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      List("B")).toSet)

    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      List("C")).toSet)

    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      List("B", "C")).toSet)

    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
      SuffixXNode("A",
        List(
          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
          SuffixLeafNode("B", 1))),
      List("A", "C")).toSet)

  }

}