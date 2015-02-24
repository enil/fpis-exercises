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
package io.github.enil

import scala.{Option => _, Either => _, List => _}

/**
 * From Functional Programming in Scala.
 */
sealed trait Option[+A] {
  /**
   * @author Emil Nilsson
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /**
   * @author Emil Nilsson
   */
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  /**
   * @author Emil Nilsson
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /**
   * @author Emil Nilsson
   */
  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case None => default
    case Some(a) => this
  }

  /**
   * @author Emil Nilsson
   */
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }
}

/**
 * From Functional Programming in Scala.
 */
case class Some[+A](get: A) extends Option[A]

/**
 * From Functional Programming in Scala.
 */
case object None extends Option[Nothing]

/**
 * From Functional Programming in Scala.
 */
sealed trait Either[+E, +A] {
  /**
   * @author Emil Nilsson
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  /**
   * @author Emil Nilsson
   */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  /**
   * @author Emil Nilsson
   */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => this
  }

  /**
   * @author Emil Nilsson
   */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    // TODO: not sure whether it should be short-circuiting
    case (Left(x), _) => Left(x)
    case (_, Left(y)) => Left(y)
    case (Right(x), Right(y)) => Right(f(x, y))
  }
}

/**
 * From Functional Programming in Scala.
 */
case class Left[+E](value: E) extends Either[E, Nothing]

/**
 * From Functional Programming in Scala.
 */
case class Right[+A](value: A) extends Either[Nothing, A]

/**
 * Exercise 4.1: implement Option.map, Option.flatMap, Option.getOrElse, Option.orElse and Option.filter.
 *
 * @author Emil Nilsson
 */
object Exercise41 {
  def main(args: Array[String]): Unit = {
    val f = (s: String) => s.toUpperCase

    assert(Some("foo").map(f) == Some("FOO"))
    assert((None:Option[String]).map(f) == None)

    assert(Some("foo").flatMap(x => Some(f(x))) == Some("FOO"))
    assert(Some("foo").flatMap(_ => None) == None)
    assert((None:Option[String]).flatMap(x => Some(f(x))) == None)

    assert(Some("foo").getOrElse("bar") == "foo")
    assert((None:Option[String]).getOrElse("bar") == "bar")

    assert(Some("foo").orElse(Some("bar")) == Some("foo"))
    assert(Some("foo").orElse(None:Option[String]) == Some("foo"))
    assert((None:Option[String]).orElse(Some("bar")) == Some("bar"))
    assert((None:Option[String]).orElse(None:Option[String]) == None)

    val g = (s: String) => !s.isEmpty

    assert(Some("foo").filter(g) == Some("foo"))
    assert(Some("").filter(g) == None)
    assert((None:Option[String]).filter(g) == None)
  }
}

/**
 * Exercise 4.2: implement variance using Option.flatMap.
 *
 * @author Emil Nilsson
 */
object Exercise42 {
  def main(args: Array[String]): Unit = {
    assert(variance(Seq(1.0, 3.0)) == Some(2.0))
    assert(variance(Seq(1.0)) == Some(0.0))
    assert(variance(Seq()) == None)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => Some(xs.map(x => Math.pow(x - m, 2)).sum))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}

/**
 * Exercise 4.3: implement Option.map2.
 *
 * @author Emil Nilsson
 */
object Exercise43 {
  def main(args: Array[String]): Unit = {
    val f = (s: String, n: Int) => s + n.toString

    assert(Option.map2(Some("foo"), Some(1))(f) == Some("foo1"))
    assert(Option.map2(Some("foo"), None)(f) == None)
    assert(Option.map2(None, Some(1))(f) == None)
  }

  object Option {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }
}

/**
 * Exercise 4.4: implement Option.sequence.
 *
 * @author Emil Nilsson
 */
object Exercise44 {
  def main(args: Array[String]): Unit = {
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(Option.sequence(List(Some(1), None, Some(3))) == None)
    assert(Option.sequence(List(Some(1), Some(2), None)) == None)
    assert(Option.sequence(List()) == Some(List()))
    assert(Option.sequence(List(None)) == None)
  }

  object Option {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case Cons(None, _) => None
      case Cons(Some(x), xx) => sequence(xx).flatMap(y => Some(Cons(x, y)))
    }
  }
}

/**
 * Exercise 4.5: implement Option.traverse.
 *
 * @author Emil Nilsson
 */
object Exercise45 {
  def main(args: Array[String]): Unit = {
    def f = (x: Int) => if (x >= 0) Some(x.toString) else None

    assert(Option.traverse(List(1, 2, 3))(f) == Some(List("1", "2", "3")))
    assert(Option.traverse(List(1, -1, 3))(f) == None)
    assert(Option.traverse(List(1, 2, -1))(f) == None)
    assert(Option.traverse(List())(f) == Some(List()))
    assert(Option.traverse(List(-1))(f) == None)

    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(Option.sequence(List(Some(1), None, Some(3))) == None)
    assert(Option.sequence(List(Some(1), Some(2), None)) == None)
    assert(Option.sequence(List()) == Some(List()))
    assert(Option.sequence(List(None)) == None)
  }

  object Option {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case Cons(x, xx) => map2(f(x), traverse(xx)(f))((l, r) => Cons(l, r))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(x => x)

    /**
     * Copied from exercise43.Option.
     */
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }
}

/**
 * Exercise 4.6: implement Either.map, Either.flatMap, Either.orElse and Either.map2.
 *
 * @author Emil Nilsson
 */
object Exercise46 {
  def main(args: Array[String]) {
    val f = (x: Int) => x * x

    assert(Left("error").map(f) == Left("error"))
    assert(Right(2).map(f) == Right(4))

    val g = (x: Int) => if (x != 0) Right(x * x) else Left("x is 0")

    assert(Left("error").flatMap(g) == Left("error"))
    assert(Right(2).flatMap(g) == Right(4))
    assert(Right(0).flatMap(g) == Left("x is 0"))

    assert(Left("error").orElse(Right(-1)) == Right(-1))
    assert(Left("error").orElse(Left("unknown error")) == Left("unknown error"))
    assert(Right(2).orElse(Right(-1)) == Right(2))

    val h = (x: Int, y: Int) => x + y

    assert(Left("error1").map2(Left("error2"))(h) == Left("error1"))
    assert(Right(1).map2(Left("error2"))(h) == Left("error2"))
    assert(Right(1).map2(Right(2))(h) == Right(3))
  }
}

/**
 * Exercise 4.7: implement Either.sequence and Either.traverse.
 *
 * @author Emil Nilsson
 */
object Exercise47 {
  def main(args: Array[String]) {
    def f = (x: Int) => if (x >= 0) Right(x.toString) else Left(s"x is $x")

    assert(Either.traverse(List(1, 2, 3))(f) == Right(List("1", "2", "3")))
    assert(Either.traverse(List(1, -2, 3))(f) == Left("x is -2"))
    assert(Either.traverse(List(1, 2, -3))(f) == Left("x is -3"))
    assert(Either.traverse(List(1, -2, -3))(f) == Left("x is -2"))
    assert(Either.traverse(List())(f) == Right(List()))
    assert(Either.traverse(List(-1))(f) == Left("x is -1"))

    assert(Either.sequence(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
    assert(Either.sequence(List(Right(1), Left("no value"), Right(3))) == Left("no value"))
    assert(Either.sequence(List(Right(1), Right(2), Left("no value"))) == Left("no value"))
    assert(Either.sequence(List(Right(1), Left("invalid"), Left("no value"))) == Left("invalid"))
    assert(Either.sequence(List()) == Right(List()))
    assert(Either.sequence(List(Left("no value"))) == Left("no value"))
  }

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(x => x)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case Cons(a, aa) => f(a).map2(traverse(aa)(f))((x, y) => Cons(x, y))
    }
  }
}
