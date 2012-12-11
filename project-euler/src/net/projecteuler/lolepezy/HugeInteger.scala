package net.projecteuler.lolepezy

/**
 * Represents big integer
 *
 */
class HugeInteger(val i: Int) {

  private val digits = List[Byte]()

  def this() = this(0)

  def *(hi: HugeInteger) = {
    
  }

  implicit def int2hugeInt(i: Int): HugeInteger = new HugeInteger(i)
}
