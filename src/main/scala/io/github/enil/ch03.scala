package io.github.enil

/**
 * The trait List, its subtraits and its companion object List is copied verbatim from Functional Programming in Scala.
 *
 * Copyright (c) 2012, Manning Publications, Co.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
 * OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends Exercise32.ListExtension
with Exercise33.ListExtension
with Exercise34.ListExtension
with Exercise35.ListExtension
with Exercise36.ListExtension
with Exercise39.ListExtension
with Exercise310.ListExtension
with Exercise311.ListExtension {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
}

/**
 * Exercise 3.2: implement List.tail.
 *
 * @author Emil Nilsson
 */
object Exercise32 {
  def main(args: Array[String]): Unit = {
    assert(List.tail(List.apply(1, 2, 3)) == List.apply(2, 3))
    assert(List.tail(List.apply(1)) == Nil)
    assert(List.tail(Nil) == Nil)
  }

  trait ListExtension {
    def tail[A](as: List[A]): List[A] = as match {
      case Cons(x, xs) => xs
      case _ => Nil
    }
  }
}

/**
 * Exercise 3.3: implement List.setHead.
 *
 * @author Emil Nilsson
 */
object Exercise33 {
  def main(args: Array[String]) {
    assert(List.setHead(List.apply(1, 2, 3), 4) == List.apply(4, 2, 3))
    assert(List.setHead(List.apply(1), 2) == List.apply(2))
    assert(List.setHead(Nil, 1) == List.apply(1))
  }

  trait ListExtension {
    def setHead[A](as: List[A], a: A): List[A] = as match {
      case Cons(x, xs) => Cons(a, xs)
      case _ => Cons(a, Nil)
    }
  }
}

/**
 * Exercise 3.4: implement List.drop.
 *
 * @author Emil Nilsson
 */
object Exercise34 {
  def main(args: Array[String]): Unit = {
    assert(List.drop(List.apply(1, 2, 3), 2) == List.apply(3))
    assert(List.drop(List.apply(1, 2, 3), 4) == Nil)
    assert(List.drop(List.apply(1), 0) == List.apply(1))
  }

  trait ListExtension {
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }
  }
}

/**
 * Exercise 3.5: implement List.dropWhile.
 *
 * @author Emil Nilsson
 */
object Exercise35 {
  def main(args: Array[String]): Unit = {
    assert(List.dropWhile(List.apply(1, 2, 3), (x: Int) => x < 3) == List.apply(3))
    assert(List.dropWhile(List.apply(1, 2, 3), (x: Int) => x > 3) == List.apply(1, 2, 3))
    assert(List.dropWhile(Nil, (x: Int) => x < 3) == Nil)
  }

  trait ListExtension {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }
  }
}

/**
 * Exercise 3.6: implement List.init.
 *
 * @author Emil Nilsson
 */
object Exercise36 {
  def main(args: Array[String]): Unit = {
    assert(List.init(List.apply(1, 2, 3)) == List.apply(1, 2))
    assert(List.init(List.apply(1)) == Nil)
    assert(List.init(Nil) == Nil)
  }

  trait ListExtension {
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }
}

/**
 * Exercise 3.9: implement List.length with foldRight.
 *
 * @author Emil Nilsson
 */
object Exercise39 {
  def main(args: Array[String]): Unit = {
    assert(List.length(List.apply(1, 2, 3)) == 3)
    assert(List.length(List.apply(1)) == 1)
    assert(List.length(Nil) == 0)
  }

  trait ListExtension {
    def length[A](as: List[A]): Int =
      List.foldRight(as, 0)((_, x) => x + 1)
  }
}

/**
 * Exercise 3.10: implement List.foldLeft.
 *
 * @author Emil Nilsson
 */
object Exercise310 {
  def main(args: Array[String]) {
    assert(List.foldLeft(List.apply(1, 2, 3), 0.0)(_ + _) == 6)
    assert(List.foldLeft(List.apply(6, 7), 1.0)(_ * _) == 42)
    assert(List.foldLeft(Nil:List[Double], 0.0)(_ + _) == 0.0)
  }

  trait ListExtension {
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }
}

/**
 * Exercise 3.11: implement List.sum and List.product with foldLeft.
 *
 * List.sum and List.product are renamed as sum2 and product not to clash with the functions in List.
 *
 * @author Emil Nilsson
 */
object Exercise311 {
  def main(args: Array[String]): Unit = {
    assert(List.sum2(List.apply(1, 2, 3)) == 6)
    assert(List.product2(List.apply(6.0, 7.0)) == 42.0)
    assert(List.sum2(Nil) == 0)
  }

  trait ListExtension extends Exercise310.ListExtension {
    def sum2(ns: List[Int]): Int =
      foldLeft(ns, 0)(_ + _)

    def product2(ns: List[Double]): Double =
      foldLeft(ns, 1.0)(_ * _)
  }
}
