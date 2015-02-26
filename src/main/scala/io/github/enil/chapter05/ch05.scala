/*
 * Some of the traits, objects and classes are based on the source code from Functional Programming in Scala.
 * The original source code from Functional Programming in Scala can be found at https://github.com/fpinscala/fpinscala.
 * These code sections are copyrighted by Manning Publications, Co:
 *
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
 *
 *
 * All other code sections are copyrighted by Emil Nilsson:
 *
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Emil Nilsson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package io.github.enil.chapter05

/**
 * From Functional Programming in Scala.
 */
sealed trait Stream[+A] {
  /**
   * @author Emil Nilsson
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(x, xx) => x() :: xx().toList
  }

  /**
   * @author Emil Nilsson
   */
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xx) => if (n > 0) Cons(x, () => xx().take(n - 1)) else Empty
  }

  /**
   * @author Emil Nilsson
   */
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xx) => if (n > 0) xx().drop(n - 1) else this
  }
}

/**
 * From Functional Programming in Scala.
 */
case object Empty extends Stream[Nothing]

/**
 * From Functional Programming in Scala.
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  /**
   * A comparison method is needed to work in assert without having to convert to lists.
   *
   * @author Emil Nilsson
   */
  override def equals(obj: Any): Boolean = obj match  {
    case that: Cons[A] => that.h() == h() && that.t() == t()
    case _ => false
  }
}

/**
 * From Functional Programming in Scala.
 */
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

/**
 * Exercise 5.1: implement Stream.toList.
 *
 * @author Emil Nilsson
 */
object Exercise51 {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    assert(Stream().toList == List())
  }
}

/**
 * Exercise 5.2: implement Stream.take and Stream.drop.
 *
 * @author Emil Nilsson
 */
object Exercise52 {
  def main (args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).take(2) == Stream(1, 2))
    assert(Stream(1, 2, 3).take(4) == Stream(1, 2, 3))
    assert(Stream(1, 2, 3).take(0) == Empty)

    assert(Stream(1, 2, 3).drop(2) == Stream(3))
    assert(Stream(1, 2, 3).drop(4) == Empty)
    assert(Stream(1).drop(0) == Stream(1))
  }
}
