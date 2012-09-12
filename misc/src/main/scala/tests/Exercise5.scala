package tests

import org.junit.Test

class Exercise5 {

  def flatten[A](xss: List[List[A]]): List[A] =
    (xss :\ (Nil: List[A]))((xs, ys) => xs ::: ys)

  def flatten1[A](xss: List[List[A]]): List[A] =
    for {
      x <- xss
      y <- x
    } yield y

  @Test def run() {
    println(flatten1(List(List(1, 2), List(3, 4))))
  }
}