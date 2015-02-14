package io.github.enil

/**
 * Exercise 2.1: calculate fibonacci numbers using tail-recursion.
 *
 * @author Emil Nilsson
 */
object Exercise21 {
  def main(args: Array[String]): Unit = {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
    assert(fib(7) == 13)
    assert(fib(8) == 21)
    assert(fib(9) == 34)
    assert(fib(10) == 55)
  }

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
 * Exercise 2.2: implement isSorted.
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

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int, o: Boolean): Boolean =
      if (!o) false
      else if (as.length - n > 1) go(n + 1, ordered(as(n), as(n + 1)))
      else true

    go(0, true)
  }
}

/**
 * Exercise 2.3: implement curry.
 *
 * @author Emil Nilsson
 */
object Exercise23 {
  def main(args: Array[String]): Unit = {
    val f = curry((a: Int, b: Int) => a * b)(6)
    assert(f(7) == 42)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)
}

/**
 * Exercise 2.4: implement uncurry.
 *
 * @author Emil Nilsson
 */
object Exercise24 {
  def main(args: Array[String]) {
    val f = (a: Int) => (b: Int) => a * b
    assert(uncurry(f)(6, 7) == 42)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}
