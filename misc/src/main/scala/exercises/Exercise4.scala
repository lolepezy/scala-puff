package exercises

import org.junit.Test

class Exercise4 {
  def queens(n: Int): List[List[Int]] = {
    def placeQueens(k: Int): List[List[Int]] =
      if (k == 0) List(List())
      else {
        val x = (for {
          queens <- placeQueens(k - 1)
          row <- List.range(0, n)
          if isSafe(row, queens, k - 1)
        } yield row :: queens);
        //println("x=" + x);
        x
      }
    def isSafe(row: Int, queens: List[Int], column: Int): Boolean = {
      val e = 
      queens.isEmpty || List.range(0, queens.length).filter(
        i => queens(i) == row ||
          math.abs(queens(i) - column) == math.abs(i - row)).isEmpty;
      //println("row = " + row + " queens = " + queens + " column = " + column + ", e = " + e);
      e
    }
    placeQueens(n).reverse
  }

  @Test def run() {
    println("range=" + List.range(1, 8 + 1));
    println("result = " + queens(8));
    
  }
}