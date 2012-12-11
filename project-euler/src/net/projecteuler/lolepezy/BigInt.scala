package net.projecteuler.lolepezy

/**
 * Represents big integer
 *
 */
class BigInt(val i: Int) {

  private val digits = List[Byte]()

  def this() = this(0)

  def *(bi: BigInt) = {
    for (d1 <- digits; d2 <- bi.digits)
  }

  implicit def int2hugeInt(i: Int): BigInt = new BigInt(i)
}
