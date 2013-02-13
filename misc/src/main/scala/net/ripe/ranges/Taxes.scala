package net.ripe.ranges

import org.junit.Test

class TNLaxes {

  @Test
  def tax {
    /**
     *
     * For the part of income up to € 18,945: 33.1%; tax on €18,945 is €6,271
     * For the part of income between €18,945 and €33,863: 41.95%; tax on €14,909 is € 6,258
     * For the part of income between €33,863 and €55,491: 42%; tax on €22,628 is €9,504
     * On all income over €55,694: 52%
     *
     */
    val monthGross = 3800 
    val amount = monthGross * 12 * 1.13

    println("amount = " + amount)
    val boxes = List(
      (0, 18945, 0.33),
      (18945, 33863, 0.4195),
      (33863, 55491, 0.42))

    val taxes = (for (b <- boxes) yield {
      if (amount > b._1) {
        val taxablePart = (if (amount > b._2) b._2 else amount) - b._1
        taxablePart * b._3
      } else 0
    })

    println(taxes)
    println(taxes.sum)
    println(amount - taxes.sum)
    
    val monthNet = (amount - taxes.sum)/1.13/12
    
    println(monthNet)
    println(1-monthNet/monthGross)
    

  }

}