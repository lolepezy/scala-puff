package ru.hflabs.testtask

import org.junit.Test

class MinSubstringsLoadTest {

  @Test def testBigTreeFinder() {
    val strings = Util.timed("generateStrings")(SubstringHelper.generateStrings)
    var (encodedLines, mapping) = SubstringHelper.encodeLines(strings);
    encodedLines = encodedLines.map(e => e.sortWith(_ <= _))
    val tree = SuffixTreeHelper.create(encodedLines)

    val enc = encodedLines.tail.tail.head

    val iterationCount = 10000

    Util.timed("finds") {
      var i = 0; while (i < iterationCount) {
        val x = tree.find(enc)
        i = i + 1
      }
    }
  }
}