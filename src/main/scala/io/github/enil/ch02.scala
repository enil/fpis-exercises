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
      if (c < n) {
        // calculate the next number
        go(c + 1, n, right, left + right)
      } else {
        // at number n
        (left, right)
      }

    go(0, n, 0, 1)._1
  }
}

/**
 * Exercise 2.2: implement isSorted
 *
 * @author Emil Nilsson
 */
object Exercise22 {
  def main(args: Array[String]): Unit = {
    val ltInt = (a: Int, b: Int) => a < b
    assert(isSorted(Array(1, 2, 3), ltInt) == true)
    assert(isSorted(Array(1, 1, 3), ltInt) == false)
    assert(isSorted(Array(1, 2, 3, 2), ltInt) == false)

    val gtInt = (a: Int, b: Int) => a > b
    assert(isSorted(Array(3, 2, 1), gtInt) == true)
    assert(isSorted(Array(3, 2, 2), gtInt) == false)
    assert(isSorted(Array(3, 2, 1, 2), gtInt) == false)

    val ltString = (a: String, b: String) => a < b
    assert(isSorted(Array("bar", "baz", "foo"), ltString) == true)
    assert(isSorted(Array("bar", "baz", "baz"), ltString) == false)
    assert(isSorted(Array("bar", "baz", "foo", "baz"), ltString) == false)
  }

  /**
   * Checks whether an array is sorted according to a comparison function.
   *
   * @param as an array
   * @param ordered a comparison function
   * @tparam A the element type of the array
   * @return true if the array is sorted according to the function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int, o: Boolean): Boolean =
      if (!o) false
      else if (as.length - n > 1) go(n + 1, ordered(as(n), as(n + 1)))
      else true

    go(0, true)
  }
}

/**
 * Exercise 2.3: implement curry
 *
 * @author Emil Nilsson
 */
object Exercise23 {
  def main(args: Array[String]): Unit = {
    val f = curry((a: Int, b: Int) => a * b)(6)
    assert(f(7) == 42)
  }

  /**
   * Returns a function currying function for a function with 2 parameters.
   *
   * @param f the function to curry
   * @tparam A the type of the first parameter
   * @tparam B the type of the second parameter
   * @tparam C the type of the return value
   * @return a curried function
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }
}
