package ru.hflabs.testtask

import org.junit.Test

class MinSubstringsLoadTest {

  @Test def testBigTree() {
    val strings = Util.timed("generateStrings")(SubstringHelper.generateStrings)
    val (tree, mapping) = SuffixTreeHelper.create(strings)
    
    
    
  }
}