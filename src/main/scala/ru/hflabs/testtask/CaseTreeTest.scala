package ru.hflabs.testtask
import org.junit.Ignore
import org.junit.Test
import org.junit.Assert._

class CaseTreeTest {

  @Test def testUniqueSubSet1() {
    val lines = List(List("A", "B"), List("A", "C"))
    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(subsets(0), Set(Set("B")))
    assertEquals(subsets(1), Set(Set("C")))
  }

  @Test def testUniqueSubSet2() {
    val lines = List(List("A", "B", "C"), List("A", "C", "D"))
    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(subsets(0), Set(Set("B")))
    assertEquals(subsets(1), Set(Set("D")))
  }

  @Test def testUniqueSubSet3() {
    val lines = List(
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1125", "города", "Москвы"),
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1126", "города", "Москвы"),
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1125", "города", "Санкт-Петербурга"),
      List("средняя", "муниципальная", "общеобразовательная", "гимназия", "№1126", "города", "Санкт-Петербурга"))

    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
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

    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(Set(Set("A", "B", "X")), subsets(0))
    assertEquals(Set(Set("Y")), subsets(1))
    assertEquals(Set(Set("C", "X")), subsets(2))
    assertEquals(Set(Set("E")), subsets(3))
  }

}