package net.projecteuler.lolepezy

import scala.annotation.tailrec
import scala.math.BigInt
import scala.math.BigInt._

object Math {

  def pow(v: BigInt, p: Int): BigInt = {
    @tailrec
    def pow1(v: BigInt, p: Int, result: BigInt): BigInt = {
      if (p == 0) result
      else pow1(v, p - 1, v * result)
    }

    if (p < 10)
      pow1(v, p, 1)
    else {
      if (p % 2 == 0) {
        val x = pow(v, p / 2)
        x * x
      } else {
        val x = pow(v, (p - 1) / 2)
        x * x * v
      }
    }
    
//    v.pow(p)
  }

  def !(n: BigInt): BigInt = {
    def fact(n: BigInt, result: BigInt): BigInt = {
      if (n == 1) result
      else fact(n - 1, n * result)
    }
    if (n == 0) 1
    else fact(n, 1)
  }

  def digit: Char => Short = {
    case '0' => 0
    case '1' => 1
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
  }

}