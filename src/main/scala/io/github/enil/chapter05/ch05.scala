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
   * From Functional Programming in Scala.
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

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

  /**
   * @author Emil Nilsson
   */
  def dropWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xx) => if (f(x())) xx().dropWhile(f) else this
  }

  /**
   * @author Emil Nilsson
   */
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(x, xx) => if (p(x())) xx().forAll(p) else false
  }

  /**
   * @author Emil Nilsson
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty[A])

  /**
   * @author Emil Nilsson
   */
  def headOption: Option[A] =
    this.foldRight[Option[A]](None)((h, _) => Some(h))

  /**
   * @author Emil Nilsson
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  /**
   * @author Emil Nilsson
   */
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  /**
   * @author Emil Nilsson
   */
  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)(Stream.cons(_, _))

  /**
   * @author Emil Nilsson
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h) append t)

  /**
   * @author Emil Nilsson
   */
  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  /**
   * @author Emil Nilsson
   */
  def take2(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), r) if r > 0 => Some(h(), (t(), r - 1))
      case _ => None
    }

  /**
   * @author Emil Nilsson
   */
  def takeWhile2(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  /**
   * @author Emil Nilsson
   */
  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold(this, bs) {
      case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
      case _ => None
    }

  /**
   * @author Emil Nilsson
   */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, s2) {
      case (Empty, Empty) => None
      case ((Cons(ah, at), Cons(bh, bt))) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case ((Cons(ah, at), Empty)) => Some((Some(ah()), None), (at(), Empty))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
    }

  /**
   * @author Emil Nilsson
   */
  def tails: Stream[Stream[A]] =
    Stream.unfold(Option(this)) {
      case None => None
      case Some(Empty) => Some(Empty, None)
      case Some(as@Cons(_, t)) => Some(as, Some(t()))
    }

  /**
   * @author Emil Nilsson
   */
  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s) map {
      case (Some(a), Some(b)) if a == b => true
      case (Some(_), None) => true
      case _ => false
    } forAll identity
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
  /**
   * From Functional Programming in Scala.
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * From Functional Programming in Scala.
   */
  def empty[A]: Stream[A] = Empty

  /**
   * From Functional Programming in Scala.
   */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   * @author Emil Nilsson
   */
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  /**
   * @author Emil Nilsson
   */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
   * @author Emil Nilsson
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (a, s) =>
      cons(a, unfold(s)(f))
    }.getOrElse(empty)
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

/**
 * Exercise 5.3: implement Stream.dropWhile.
 *
 * @author Emil Nilsson
 */
object Exercise53 {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).dropWhile((x: Int) => x < 3) == Stream(3))
    assert(Stream(1, 2, 3).dropWhile((x: Int) => x > 3) == Stream(1, 2, 3))
    assert(Empty.dropWhile((x: Int) => x < 3) == Empty)
  }
}

/**
 * Exercise 5.4: implement Stream.forAll.
 *
 * @author Emil Nilsson
 */
object Exercise54 {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 3, 5).forAll(_ % 2 == 1) == true)
    assert(Stream(1, 2, 3, 5).forAll(_ % 2 == 1) == false)
    assert(Stream(1, 3, 5, 6).forAll(_ % 2 == 1) == false)
    assert(Empty.forAll((x: Int) => x % 2 == 1) == true)
  }
}

/**
 * Exercise 5.5: implement Stream.takeWhile using Stream.foldRight.
 *
 * @author Emil Nilsson
 */
object Exercise55 {
  def main(args: Array[String]) {
    assert(Stream(1, 2, 3).takeWhile((x: Int) => x < 3) == Stream(1, 2))
    assert(Stream(1, 2, 3).takeWhile((x: Int) => x > 3) == Empty)
    assert(Empty.takeWhile((x: Int) => x < 3) == Empty)
  }
}

/**
 * Exercise 5.6: implement Stream.headOption using Stream.foldRight.
 *
 * @author Emil Nilsson
 */
object Exercise56 {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).headOption == Some(1))
    assert(Empty.headOption == None)
  }
}

/**
 * Exercise 5.7: implement Stream.map, Stream.filter, Stream.append and Stream.flatMap using Stream.foldRight.
 *
 * @author Emil Nilsson
 */
object Exercise57 {
  def main(args: Array[String]) {
    assert(Stream(1, 2, 3).map(_.toString) == Stream("1", "2", "3"))
    assert(Stream(1, 2, 3).map(_ + 1) == Stream(2, 3, 4))
    assert(Empty.map(_.toString) == Empty)

    assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0) == Stream(2, 4))
    assert(Stream(1, 3, 5, 7).filter(_ % 2 == 0) == Empty)
    assert(Empty.filter(_ => false) == Empty)

    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)) == Stream(1, 2, 3, 4, 5, 6))
    assert(Stream(1, 2, 3).append(Empty) == Stream(1, 2, 3))
    assert(Empty.append(Stream(4, 5, 6)) == Stream(4, 5, 6))

    assert(Stream(1, 2, 3).flatMap(i => Stream(i, i)) == Stream(1, 1, 2, 2, 3, 3))
    assert(Stream(1, 2, 3).flatMap(_ => Empty) == Empty)
    assert(Empty.flatMap(_ => Empty) == Empty)
  }
}

/**
 * Exercise 5.8: implement Stream.constant.
 *
 * @author Emil Nilsson
 */
object Exercise58 {
  def main(args: Array[String]): Unit = {
    assert(Stream.constant(1).take(3) == Stream(1, 1, 1))
    assert(Stream.constant("foo").take(2) == Stream("foo", "foo"))
  }
}

/**
 * Exercise 5.9: implement Stream.from.
 *
 * @author Emil Nilsson
 */
object Exercise59 {
  def main(args: Array[String]): Unit = {
    assert(Stream.from(0).take(3) == Stream(0, 1, 2))
    assert(Stream.from(-10).take(3) == Stream(-10, -9, -8))
  }
}

/**
 * Exercise 5.10: implement fib using Stream.
 *
 * @author Emil Nilsson
 */
object Exercise510 {
  def main(args: Array[String]): Unit = {
    assert(fib.take(8) == Stream(0, 1, 1, 2, 3, 5, 8, 13))
  }

  def fib: Stream[Int] = {
    def go(s: (Int, Int)): Stream[Int] =
      Stream.cons(s._1, go(s._2, s._1 + s._2))

    go(0, 1)
  }
}

/**
 * Exercise 5.12: implement fib, from, constant and ones using Stream.unfold.
 *
 * @author Emil Nilsson
 */
object Exercise512 {
  def main(args: Array[String]) {
    assert(fib.take(8) == Stream(0, 1, 1, 2, 3, 5, 8, 13))

    assert(from(0).take(3) == Stream(0, 1, 2))
    assert(from(-10).take(3) == Stream(-10, -9, -8))

    assert(constant(1).take(3) == Stream(1, 1, 1))
    assert(constant("foo").take(2) == Stream("foo", "foo"))

    assert(ones.take(3) == Stream(1, 1, 1))
  }

  def fib = Stream.unfold(0, 1) { case(left, right) => Some(left, (right, left + right)) }

  def from(n: Int) = Stream.unfold(n)(x => Some(x, x + 1))

  def constant[A](a: A) = Stream.unfold()(_ => Some(a, ()))

  def ones = Stream.unfold()(_ => Some(1, ()))
}

/**
 * Exercise 5.13: implement Stream.map, Stream.take, Stream.takeWhile, Stream.zipWith and Stream.zipAll using
 * Stream.unfold.
 *
 * @author Emil Nilsson
 */
object Exercise513 {
  def main(args: Array[String]) {
    assert(Stream(1, 2, 3).map2(_.toString) == Stream("1", "2", "3"))
    assert(Stream(1, 2, 3).map2(_ + 1) == Stream(2, 3, 4))
    assert(Empty.map2(_.toString) == Empty)

    assert(Stream(1, 2, 3).take2(2) == Stream(1, 2))
    assert(Stream(1, 2, 3).take2(4) == Stream(1, 2, 3))
    assert(Stream(1, 2, 3).take2(0) == Empty)

    assert(Stream(1, 2, 3).takeWhile2((x: Int) => x < 3) == Stream(1, 2))
    assert(Stream(1, 2, 3).takeWhile2((x: Int) => x > 3) == Empty)
    assert(Empty.takeWhile2((x: Int) => x < 3) == Empty)

    assert(Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _) == Stream(5, 7, 9))
    assert(Stream("foo", "bar").zipWith(Stream(1, 2, 3))(_ + _.toString) == Stream("foo1", "bar2"))
    assert(Stream("foo", "bar").zipWith(Empty)(_ + _.toString) == Empty)
    assert((Empty:Stream[String]).zipWith(Empty)(_ + _.toString) == Empty)

    assert(Stream(1, 2).zipAll(Stream(1, 4)) == Stream((Some(1), Some(1)), (Some(2), Some(4))))
    assert(Stream(1, 2).zipAll(Stream(1)) == Stream((Some(1), Some(1)), (Some(2), None)))
    assert(Stream(1).zipAll(Stream(1, 4)) == Stream((Some(1), Some(1)), (None, Some(4))))
  }
}

/**
 * Exercise 5.14: implement Stream.startsWith.
 *
 * @author Emil Nilsson
 */
object Exercise514 {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)) == true)
    assert(Stream(1, 2).startsWith(Stream(1, 2, 3)) == false)
    assert(Stream(1, 2, 3).startsWith(Empty) == true)
    assert(Empty.startsWith(Stream(1)) == false)
    assert(Empty.startsWith(Empty) == true)
  }
}

/**
 * Exercise 5.15: implement Stream.tails using Stream.unfold.
 *
 * @author Emil Nilsson
 */
object Exercise515 {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).tails == Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream()))
    assert(Empty.tails == Stream(Empty))
  }
}
