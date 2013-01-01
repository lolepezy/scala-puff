package exercises._99

sealed abstract class Node[+A] {}

case class Tree[+A](value: A, left: Node[A], right: Node[A]) extends Node[A]

case object End extends Node[Nothing]

object TreeHelper {

  def countAndBalance[A](tree: Node[A]): (Int, Boolean) = tree match {
    case Tree(_, l, r) =>
      val countLeft = countAndBalance(l)
      val countRight = countAndBalance(r)
      (1 + countLeft._1 + countRight._1, countLeft._2 &&
        countRight._2 && math.abs(countLeft._1 - countRight._1) <= 1)
    case End => (0, true)
  }

  /**
   * Problem 55
   */
  def createBalanced(n: Int): Node[Int] = n match {
    case 0 => End
    case 1 => Tree[Int](n, End, End)
    case _ => {
      attach(createBalanced(n - 1), n)
    }
  }

  def attach[A](tree: Node[A], x: A): Node[A] = tree match {
    case Tree(z, left, right) => {
      val lc = countAndBalance(left)
      val rc = countAndBalance(right)
      if (lc._1 < rc._1)
        Tree(z, attach(left, x), right)
      else
        Tree(z, left, attach(right, x))
    }
    case End => Tree[A](x, End, End)
  }

  /**
   * Problem 56
   */
  def isSymmetric[A](tree: Node[A]) = {
    def isMirrorOf[A](t1: Node[A], t2: Node[A]): Boolean = {
      (t1, t2) match {
        case (End, End) => true
        case (Tree(_, left1, right1), Tree(_, left2, right2)) =>
          isMirrorOf(left1, right2) && isMirrorOf(left2, right1)
        case _ => false
      }
    }
    tree match {
      case End => true
      case Tree(_, left, right) => isMirrorOf(left, right)
    }
  }

  /**
   * Problem 60
   */
  def heightAndHBalanced[A](tree: Node[A]): (Int, Boolean) = tree match {
    case End => (0, true)
    case Tree(_, left, right) =>
      val lcb = heightAndHBalanced(left)
      val rcb = heightAndHBalanced(right)
      (1 + math.max(lcb._1, rcb._1), math.abs(lcb._1 - rcb._1) <= 1 && lcb._2 && rcb._2)
  }

  def createHeightBalanced[A](n: Int, x: A) {
    List()
    Map(1->2)
  }

} 
