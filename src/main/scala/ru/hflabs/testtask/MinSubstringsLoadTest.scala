package ru.hflabs.testtask

import org.junit.Test

class MinSubstringsLoadTest {

  @Test def testBigTreeFinder() {
    val strings = Util.timed("generateStrings")(SubstringHelper.generateStrings)
    var (encodedLines, mapping) = SubstringHelper.encodeLines(strings);
    encodedLines = encodedLines.map(e => e.sortWith(_ <= _))

    val enc = encodedLines.tail.tail.head

    val iterationCount = 10000

    val subsets = Util.timed("subsets") {
      SuffixTreeHelper.searchUniqueSubset(strings)
    }

  }
}