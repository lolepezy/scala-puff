package ru.hflabs.testtask

import org.junit.Test

class MinSubstringsLoadTest {

  @Test def testBigTree() {
    val strings = SubstringHelper.generateStrings
    val (tree, mapping) = SuffixTreeHelper.create(strings)
    
    
  }
}