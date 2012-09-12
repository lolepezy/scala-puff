package tests

import org.junit.Test

class AList[A] {
  def insert(x: A, list: List[A]): List[A] = {
    x :: list
  }
}

class Exercise3 {
  def flatten[A](xs: List[List[A]]): List[A] =
    (xs :\ (Nil: List[A]))((x, xs) => x ::: xs)

  @Test def run() {
    println(List(1, 2, 3, 4, 5).foldLeft(1)((x, y) => x * y));
    println((List(1, 2, 3, 4, 5) :\ 1)((x, y) => x * y));

    println(List(1, 2, 3, 4, 5).foldRight(1)((x, y) => x * y));

    println(List(1, 2, 3, 4, 5).reduceLeft((x, y) => x + y));
    println(List(1, 2, 3, 4, 5).foldRight(1)((x, y) => x * y));
  }
}

