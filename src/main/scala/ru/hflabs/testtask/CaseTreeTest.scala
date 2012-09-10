package ru.hflabs.testtask
import org.junit.Ignore
import org.junit.Test
import org.junit.Assert._

class CaseTreeTest {

  //  @Test def testSuffixTreeAdd() {
  //    val tree = SuffixXNode("A", List())
  //    TreeFinder.add(tree, List("A", "B"), 1)
  //
  //    assertEquals(SuffixXNode("A", List(SuffixLeafNode("B", 1))), tree)
  //
  //    TreeFinder.add(tree, List("A", "B", "C"), 3)
  //    assertEquals(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      tree)
  //
  //    TreeFinder.add(tree, List("A", "D"), 4)
  //    assertEquals(
  //      SuffixXNode("A",
  //        List(
  //          SuffixLeafNode("D", 4),
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      tree)
  //
  //    TreeFinder.add(tree, List("A", "B", "F"), 5)
  //    assertEquals(
  //      SuffixXNode("A",
  //        List(
  //          SuffixLeafNode("D", 4),
  //          SuffixXNode("B", List(SuffixLeafNode("F", 5), SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      tree)
  //
  //  }
  //
  //  @Test def testSuffixTreeFind() {
  //
  //    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      List("A", "B", "C")).toSet)
  //
  //    assertEquals(Set(1, 3), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      List("A", "B")).toSet)
  //
  //    assertEquals(Set(1, 3, 4), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1),
  //          SuffixLeafNode("D", 4))),
  //      List("A")).toSet)
  //
  //    assertEquals(Set(1, 3), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      List("B")).toSet)
  //
  //    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      List("C")).toSet)
  //
  //    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      List("B", "C")).toSet)
  //
  //    assertEquals(Set(3), TreeFinder.getSubsetPayloads(
  //      SuffixXNode("A",
  //        List(
  //          SuffixXNode("B", List(SuffixLeafNode("C", 3))),
  //          SuffixLeafNode("B", 1))),
  //      List("A", "C")).toSet)
  //
  //  }

  @Test def testUniqueSubSet1() {
    val lines = List(List("A", "B"), List("A", "C"))
    val subsets = SuffixTreeHelper.searchUniqueSubset(lines)
    assertEquals(subsets(0), Set(Set("B")))
    assertEquals(subsets(1), Set(Set("C")))
  }

  @Test def testUniqueSubSet2() {
    val lines = List(List("A", "B", "C"), List("A", "C", "D"))
    val subsets = SuffixTreeHelper.searchUniqueSubset(lines)
    assertEquals(subsets(0), Set(Set("B")))
    assertEquals(subsets(1), Set(Set("D")))
  }

  @Test def testUniqueSubSet3() {
    val lines = List(
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1125", "города", "Москвы"),
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1126", "города", "Москвы"),
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1125", "города", "Санкт-Петербурга"),
      List("средняя", "муниципальная", "общеобразовательная", "гимназия", "№1126", "города", "Санкт-Петербурга"))

    val subsets = SuffixTreeHelper.searchUniqueSubset(lines)
    assertEquals(Set(Set("№1125", "Москвы")), subsets(0))
    assertEquals(Set(Set("школа", "№1126"), Set("№1126", "Москвы")), subsets(1))
    assertEquals(Set(Set("№1125", "Санкт-Петербурга"), Set("школа", "Санкт-Петербурга")), subsets(2))
    assertEquals(Set(Set("гимназия")), subsets(3))
  }

  @Test def testUniqueSubSetLongSubset() {
    val lines = List(
      List("A", "B", "X"),
      List("A", "B", "C", "Y"),
      List("A", "C", "X"),
      List("B", "E", "X"))

    val subsets = SuffixTreeHelper.searchUniqueSubset(lines)
    assertEquals(Set(Set("A", "B", "X")), subsets(0))
    assertEquals(Set(Set("Y")), subsets(1))
    assertEquals(Set(Set("C", "X")), subsets(2))
    assertEquals(Set(Set("E")), subsets(3))
  }

}