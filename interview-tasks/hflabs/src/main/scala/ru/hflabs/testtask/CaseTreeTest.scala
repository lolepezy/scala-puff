package ru.hflabs.testtask
import org.junit.Ignore
import org.junit.Test
import org.junit.Assert._

class CaseTreeTest {

  @Test def testUniqueSubSet1() {
    val lines = List(List("A", "B"), List("A", "C"))
    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(subsets(0), Set(List("B")))
    assertEquals(subsets(1), Set(List("C")))
  }

  @Test def testUniqueSubSet2() {
    val lines = List(List("A", "B", "C"), List("A", "C", "D"))
    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(subsets(0), Set(List("B")))
    assertEquals(subsets(1), Set(List("D")))
  }

  @Test def testUniqueSubSet3() {
    val lines = List(
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1125", "города", "Москвы"),
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1126", "города", "Москвы"),
      List("средняя", "муниципальная", "общеобразовательная", "школа", "№1125", "города", "Санкт-Петербурга"),
      List("средняя", "муниципальная", "общеобразовательная", "гимназия", "№1126", "города", "Санкт-Петербурга"))

    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(Set(List("Москвы", "№1125")), subsets(0))
    assertEquals(Set(List("школа", "№1126"), List("Москвы", "№1126")), subsets(1))
    assertEquals(Set(List("Санкт-Петербурга", "№1125"), List("Санкт-Петербурга", "школа")), subsets(2))
    assertEquals(Set(List("гимназия")), subsets(3))
  }

  @Test def testUniqueSubSetLongerSubset() {
    val lines = List(
      List("A", "B", "X"),
      List("A", "B", "C", "Y"),
      List("A", "C", "X"),
      List("B", "E", "X"))

    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    assertEquals(Set(), subsets(0))
    assertEquals(Set(List("Y")), subsets(1))
    assertEquals(Set(List("C", "X")), subsets(2))
    assertEquals(Set(List("E")), subsets(3))
  }

  @Test def testUniqueBug() {
    val lines = List(
      List("str8", "str7", "str4", "str2"),
      List("str3", "str0"),
      List("str2", "str4"),
      List("str1", "str4"),
      List("str8", "str5"),
      List("str2", "str5", "str7", "str1"),
      List("str7", "str7"),
      List("str6", "str1", "str6", "str4"),
      List("str4", "str5", "str8", "str4"),
      List("str3", "str8"))

    val subsets = SuffixTreeHelper.searchUniqueSubsets(lines)
    println("subsets = " + subsets)
    val z = Map(
      0 -> Set(List("str7", "str8"), List("str2", "str8"), List("str4", "str7")),
      1 -> Set(List("str0")),
      2 -> Set(),
      3 -> Set(),
      4 -> Set(),
      5 -> Set(List("str1", "str5"), List("str5", "str7"), List("str2", "str5"), List("str1", "str7"), List("str1", "str2")),
      6 -> Set(List("str7", "str7")),
      7 -> Set(List("str6")),
      8 -> Set(List("str4", "str4"), List("str4", "str5")),
      9 -> Set(List("str3", "str8")))

    assertEquals(z, subsets)
  }

}
