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
with Exercise35.ListExtension {
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
        case Cons(x, xs) => drop(xs, n-1)
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
