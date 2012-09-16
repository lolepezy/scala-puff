package ru.hflabs.testtask

import org.junit.Test
import org.junit.Assert._

class MinSubstringsLoadTest {

  @Test def testBigTreeFinder() {
    val strings = Util.timed("generateStrings")(SubstringHelper.generateStrings)
    val allSubsets = Util.timed("subsets") {
      SuffixTreeHelper.searchUniqueSubsets(strings)
    }

    val linesAsSets = strings map (_.toSet)

    println("_____________________________________")

    var isBad = false
    var index = 0
    for (line <- linesAsSets) {
      val subsets = allSubsets(index) map (_.toSet)
      var index2 = 0
      for (ss <- linesAsSets) {
        if (index != index2) {
          subsets.foreach(s => {
            if (s.subsetOf(ss))
              isBad = true
            //assertFalse(s.toSet.subsetOf(ss))  
          })
          index2 += 1
        }
      }
      if (index % 100 == 0)
        println("index = " + index)
      index += 1
    }

    if (isBad) {
      println("strings = " + strings)
      println("allSubsets = " + allSubsets)
    }
  }
}