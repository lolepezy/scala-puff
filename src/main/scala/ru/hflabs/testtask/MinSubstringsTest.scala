package ru.hflabs.testtask
import org.junit.Test
import org.junit.Assert._
import org.junit.Ignore

class MinSubstringsTest {

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
    assertEquals(subsets(0), Set(Set("№1125", "Москвы")))
    assertEquals(subsets(1), Set(Set("школа", "№1126"), Set("№1126", "Москвы")))
    assertEquals(subsets(2), Set(Set("№1125", "Санкт-Петербурга"), Set("школа", "Санкт-Петербурга")))
    assertEquals(subsets(3), Set(Set("гимназия")))
  }

}