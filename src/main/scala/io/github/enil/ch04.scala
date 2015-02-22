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

import scala.{Option => _}

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
