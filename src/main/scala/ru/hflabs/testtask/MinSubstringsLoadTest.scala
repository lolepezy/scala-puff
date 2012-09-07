package ru.hflabs.testtask

import org.junit.Test

class MinSubstringsLoadTest {

  @Test def testBigTree() {
    val strings = Util.timed("generateStrings")(SubstringHelper.generateStrings)
    val (encodedLines, mapping) = SuffixTreeHelper.encodeLines(strings);
    val tree = SuffixTreeHelper.create(encodedLines)

    val enc = strings.head.map(x => mapping(x))

    val iterationCount = 10000
    Util.timed("matches") {
      var i = 0; while (i < iterationCount) {
        val x = tree.matches(enc)
        i = i + 1
      }
    }

    Util.timed("finds") {
      var i = 0; while (i < iterationCount) {
        val x = tree.find(enc)
        i = i + 1
      }
    }
  }
}