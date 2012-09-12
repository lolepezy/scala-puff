package ru.hflabs.testtask

/**
 * Represent line array as
 *
 */
class BitMinSubstring(encLines: Defs.EncLinesType) {

  private var LONG_BITS = 64;

  class BitArray() {
    val arraySize = Defs.dictionarySize / LONG_BITS
    val bvals = new Array[Long](arraySize)
    def set(index: Int, offset: Int) {
      val d = bvals(index) & (1 << offset)
      bvals.update(index, d)
    }
    def unset(index: Int, offset: Int) {
      val d = bvals(index) & (~(1 << offset) & 0xFFFFFFFFFFFFFFFFL)
      bvals.update(index, d)
    }
  }

  def bits = {
    encLines map (line => {
      val bits = new BitArray
      line.foreach(code => {
        val index = code / LONG_BITS
        val offset = code % LONG_BITS
        bits.set(index, offset)
      })
      bits
    })
  }

}