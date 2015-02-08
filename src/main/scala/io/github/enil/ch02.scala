package io.github.enil

/**
 * Exercise 2.1: calculate fibonacci numbers using tail-recursion.
 *
 * @author Emil Nilsson
 */
object Exercise21 {
  def main(args: Array[String]): Unit = {
    // print the n first number in the fibonacci sequence
    def loop(c: Int, n: Int): Unit =
      if (c < n) {
        println(s"fib($c) = ${fib(c)}")
        loop(c + 1, n)
      }

    loop(0, 20)
  }

  /**
   * Calculate the nth number in the fibonacci sequence.
   *
   * @param n an index in the fibonacci sequence
   * @return the nth fibonacci number.
   */
  def fib(n: Int): Int = {
    def go(c: Int, n: Int, left: Int, right: Int): (Int, Int) =
      if (c < n)
        // calculate the next number
        go(c + 1, n, right, left + right)
      else
        // at number n
        (left, right)

    go(0, n, 0, 1)._1
  }
}
