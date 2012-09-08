package ru.hflabs.testtask

import org.junit.Ignore
import org.junit.Test
import org.junit.Assert._

class CodeMapTest {

  @Test def testCodeMapAdd() {
    val codeMap = new CodeMap[String, Int]
    codeMap ++ (List("A", "B", "C"), 1)
    assertEquals(Some(Set(1)), codeMap("A"))
    assertEquals(Some(Set(1)), codeMap("B"))
    assertEquals(Some(Set(1)), codeMap("C"))

    assertEquals(Some(Set(1)), codeMap("A", "B"))
    assertEquals(Some(Set(1)), codeMap("A", "C"))
    assertEquals(Some(Set(1)), codeMap("B", "C"))

    codeMap ++ (List("A", "C", "D"), 2)
    assertEquals(Some(Set(2)), codeMap("D"))
    assertEquals(Some(Set(1)), codeMap("A", "B"))
    assertEquals(Some(Set(1, 2)), codeMap("A", "C"))
    assertEquals(Some(Set(1)), codeMap("B", "C"))
    assertEquals(Some(Set(2)), codeMap("A", "D"))

  }

}