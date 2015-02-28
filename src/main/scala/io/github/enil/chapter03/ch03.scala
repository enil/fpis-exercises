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

package io.github.enil.chapter03

import scala.{List => _, Nil => _}

/**
 * From Functional Programming in Scala.
 */
sealed trait List[+A]

/**
 * From Functional Programming in Scala.
 */
case object Nil extends List[Nothing]

/**
 * From Functional Programming in Scala.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * From Functional Programming in Scala.
 */
object List {
  /**
   * From Functional Programming in Scala.
   */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  /**
   * From Functional Programming in Scala.
   */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  /**
   * From Functional Programming in Scala.
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * From Functional Programming in Scala.
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
   * @author Emil Nilsson
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(x, xs) => xs
    case _ => Nil
  }

  /**
   * @author Emil Nilsson
   */
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Cons(x, xs) => Cons(a, xs)
    case _ => Cons(a, Nil)
  }

  /**
   * @author Emil Nilsson
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  /**
   * @author Emil Nilsson
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  /**
   * @author Emil Nilsson
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /**
   * @author Emil Nilsson
   */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, x) => x + 1)

  /**
   * @author Emil Nilsson
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * @author Emil Nilsson
   */
  def sum2(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  /**
   * @author Emil Nilsson
   */
  def product2(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  /**
   * @author Emil Nilsson
   */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((xs, x) => Cons(x, xs))

  /**
   * @author Emil Nilsson
   */
  def append[A](left: List[A], right: List[A]): List[A] =
    foldRight(left, right)(Cons(_, _))

  /**
   * @author Emil Nilsson
   */
  def increase(ns: List[Int]): List[Int] = ns match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, increase(xs))
  }

  /**
   * @author Emil Nilsson
   */
  def doublesToStrings(ns: List[Double]): List[String] = ns match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doublesToStrings(xs))
  }

  /**
   * @author Emil Nilsson
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  /**
   * @author Emil Nilsson
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  /**
   * @author Emil Nilsson
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  /**
   * @author Emil Nilsson
   */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  /**
   * @author Emil Nilsson
   */
  def addInts(ls: List[Int], rs: List[Int]): List[Int] = (ls, rs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addInts(xs, ys))
    case _ => Nil
  }

  /**
   * @author Emil Nilsson
   */
  def zipWith[A, B, C](ls: List[A], rs: List[B])(f: (A, B) => C): List[C] = (ls, rs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => Nil
  }

  /**
   * @author Emil Nilsson
   */
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(x, xs), Cons(y, ys)) => if (x == y) startsWith(xs, ys) else false
    case (_, Nil) => true
    case _ => false
  }

  /**
   * @author Emil Nilsson
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (Cons(x, xs), _) => if (startsWith(sup, sub)) true else hasSubsequence(xs, sub)
    case _ => false
  }
}

/**
 * From Functional Programming in Scala.
 */
sealed trait Tree[+A]

/**
 * From Functional Programming in Scala.
 */
case class Leaf[A](value: A) extends Tree[A]

/**
 * From Functional Programming in Scala.
 */
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
 * Companion object for the Tree trait.
 *
 * @author Emil Nilsson
 */
object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ max _ + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(x => Leaf(f(x)))(Branch(_, _))
}

/**
 * Exercise 3.2: implement List.tail.
 *
 * @author Emil Nilsson
 */
object Exercise32 {
  def main(args: Array[String]): Unit = {
    assert(List.tail(List(1, 2, 3)) == List(2, 3))
    assert(List.tail(List(1)) == Nil)
    assert(List.tail(Nil) == Nil)
  }
}

/**
 * Exercise 3.3: implement List.setHead.
 *
 * @author Emil Nilsson
 */
object Exercise33 {
  def main(args: Array[String]) {
    assert(List.setHead(List(1, 2, 3), 4) == List(4, 2, 3))
    assert(List.setHead(List(1), 2) == List(2))
    assert(List.setHead(Nil, 1) == List(1))
  }
}

/**
 * Exercise 3.4: implement List.drop.
 *
 * @author Emil Nilsson
 */
object Exercise34 {
  def main(args: Array[String]): Unit = {
    assert(List.drop(List(1, 2, 3), 2) == List(3))
    assert(List.drop(List(1, 2, 3), 4) == Nil)
    assert(List.drop(List(1), 0) == List(1))
  }
}

/**
 * Exercise 3.5: implement List.dropWhile.
 *
 * @author Emil Nilsson
 */
object Exercise35 {
  def main(args: Array[String]): Unit = {
    assert(List.dropWhile(List(1, 2, 3), (x: Int) => x < 3) == List(3))
    assert(List.dropWhile(List(1, 2, 3), (x: Int) => x > 3) == List(1, 2, 3))
    assert(List.dropWhile(Nil, (x: Int) => x < 3) == Nil)
  }
}

/**
 * Exercise 3.6: implement List.init.
 *
 * @author Emil Nilsson
 */
object Exercise36 {
  def main(args: Array[String]): Unit = {
    assert(List.init(List(1, 2, 3)) == List(1, 2))
    assert(List.init(List(1)) == Nil)
    assert(List.init(Nil) == Nil)
  }
}

/**
 * Exercise 3.9: implement List.length using foldRight.
 *
 * @author Emil Nilsson
 */
object Exercise39 {
  def main(args: Array[String]): Unit = {
    assert(List.length(List(1, 2, 3)) == 3)
    assert(List.length(List(1)) == 1)
    assert(List.length(Nil) == 0)
  }
}

/**
 * Exercise 3.10: implement List.foldLeft.
 *
 * @author Emil Nilsson
 */
object Exercise310 {
  def main(args: Array[String]) {
    assert(List.foldLeft(List(1, 2, 3), 0.0)(_ + _) == 6)
    assert(List.foldLeft(List(6, 7), 1.0)(_ * _) == 42)
    assert(List.foldLeft(Nil:List[Double], 0.0)(_ + _) == 0.0)
  }
}

/**
 * Exercise 3.11: implement List.sum and List.product using foldLeft.
 *
 * List.sum and List.product are renamed as sum2 and product not to clash with the functions in List.
 *
 * @author Emil Nilsson
 */
object Exercise311 {
  def main(args: Array[String]): Unit = {
    assert(List.sum2(List(1, 2, 3)) == 6)
    assert(List.product2(List(6.0, 7.0)) == 42.0)
    assert(List.sum2(Nil) == 0)
  }
}

/**
 * Exercise 3.12: implement List.reverse.
 *
 * @author Emil Nilsson
 */
object Exercise312 {
  def main(args: Array[String]): Unit = {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(List.reverse(Nil) == Nil)
  }
}

/**
 * Exercise 3.14: implement List.append using foldRight.
 *
 * @author Emil Nilsson
 */
object Exercise314 {
  def main(args: Array[String]) {
    assert(List.append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(List.append(List(1, 2, 3), Nil) == List(1, 2, 3))
    assert(List.append(Nil, List(4, 5, 6)) == List(4, 5, 6))
  }
}

/**
 * Exercise 3.16: implement List.increase.
 *
 * @author Emil Nilsson
 */
object Exercise316 {
  def main(args: Array[String]): Unit = {
    assert(List.increase(List(1, 2, 3)) == List(2, 3, 4))
    assert(List.increase(Nil) == Nil)
  }
}

/**
 * Exercise 3.17: implement List.doublesToStrings.
 *
 * @author Emil Nilsson
 */
object Exercise317 {
  def main(args: Array[String]): Unit = {
    assert(List.doublesToStrings(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
    assert(List.doublesToStrings(Nil) == Nil)
  }
}

/**
 * Exercise 3.18: implement List.map.
 *
 * @author Emil Nilsson
 */
object Exercise318 {
  def main(args: Array[String]): Unit = {
    assert(List.map(List(1, 2, 3))(_.toString) == List("1", "2", "3"))
    assert(List.map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
    assert(List.map(Nil)(_.toString) == Nil)
  }
}

/**
 * Exercise 3.19: implement List.filter.
 *
 * @author Emil Nilsson
 */
object Exercise319 {
  def main(args: Array[String]): Unit = {
    assert(List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))
    assert(List.filter(List(1, 3, 5, 7))(_ % 2 == 0) == Nil)
    assert(List.filter(Nil)(_ => false) == Nil)
  }
}

/**
 * Exercise 3.20: implement List.flatMap.
 *
 * @author Emil Nilsson
 */
object Exercise320 {
  def main(args: Array[String]): Unit = {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
    assert(List.flatMap(List(1, 2, 3))(_ => Nil) == Nil)
    assert(List.flatMap(Nil)(_ => Nil) == Nil)
  }
}

/**
 * Exercise 3.21: implement List.filter using flatMap.
 *
 * @author Emil Nilsson
 */
object Exercise321 {
  def main(args: Array[String]) {
    assert(List.filter2(List(1, 2, 3, 4, 5))(_ % 2 == 0) == List(2, 4))
    assert(List.filter2(List(1, 3, 5, 7))(_ % 2 == 0) == Nil)
    assert(List.filter2(Nil)(_ => false) == Nil)
  }
}

/**
 * Exercise 3.22: implement List.addInts.
 *
 * @author Emil Nilsson
 */
object Exercise322 {
  def main(args: Array[String]): Unit = {
    assert(List.addInts(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(List.addInts(List(1, 2, 3), List(4, 5)) == List(5, 7))
    assert(List.addInts(List(1, 2, 3), Nil) == Nil)
    assert(List.addInts(Nil, Nil) == Nil)
  }
}

/**
 * Exercise 3.23: implement List.zipWith.
 *
 * @author Emil Nilsson
 */
object Exercise323 {
  def main (args: Array[String]): Unit = {
    assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
    assert(List.zipWith(List("foo", "bar"), List(1, 2, 3))(_ + _.toString) == List("foo1", "bar2"))
    assert(List.zipWith(List("foo", "bar"), Nil)(_ + _.toString) == Nil)
    assert(List.zipWith(Nil:List[String], Nil:List[Any])(_ + _.toString) == Nil)
  }
}

/**
 * Exercise 3.24: implement List.hasSubsequence.
 *
 * @author Emil Nilsson
 */
object Exercise324 {
  def main(args: Array[String]): Unit = {
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) == true)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) == true)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(4)) == true)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(4, 3)) == false)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(1, 4)) == false)
    assert(List.hasSubsequence(List(1, 2, 3, 4), Nil) == true)
    assert(List.hasSubsequence(Nil, Nil) == true)
  }
}

/**
 * Exercise 3.25: implement Tree.size.
 *
 * @author Emil Nilsson
 */
object Exercise325 {
  def main(args: Array[String]): Unit = {
    assert(Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
    assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size(Leaf(1)) == 1)
  }
}

/**
 * Exercise 3.26: implement Tree.maximum.
 *
 * @author Emil Nilsson
 */
object Exercise326 {
  def main(args: Array[String]): Unit = {
    assert(Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    assert(Tree.maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) == 3)
    assert(Tree.maximum(Leaf(1)) == 1)
  }
}

/**
 * Exercise 3.27: implement Tree.depth.
 *
 * @author Emil Nilsson
 */
object Exercise327 {
  def main(args: Array[String]): Unit = {
    assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    assert(Tree.depth(Branch(Leaf(1), Leaf(2))) == 2)
    assert(Tree.depth(Leaf(1)) == 1)
  }
}

/**
 * Exercise 3.28: implement Tree.map.
 *
 * @author Emil Nilsson
 */
object Exercise328 {
  def main(args: Array[String]): Unit = {
    assert(Tree.map(Branch(Leaf(1), Leaf(2)))(_.toString) == Branch(Leaf("1"), Leaf("2")))
    assert(Tree.map(Leaf(2))(x => x * x) == Leaf(4))
  }
}

/**
 * Exercise 3.29: implement Tree.fold and size, maximum, depth and map using fold.
 *
 * @author Emil Nilsson
 */
object Exercise329 {
  def main(args: Array[String]) {
    assert(Tree.size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
    assert(Tree.size2(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size2(Leaf(1)) == 1)

    assert(Tree.maximum2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    assert(Tree.maximum2(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) == 3)
    assert(Tree.maximum2(Leaf(1)) == 1)

    assert(Tree.depth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    assert(Tree.depth2(Branch(Leaf(1), Leaf(2))) == 2)
    assert(Tree.depth2(Leaf(1)) == 1)

    assert(Tree.map2(Branch(Leaf(1), Leaf(2)))(_.toString) == Branch(Leaf("1"), Leaf("2")))
    assert(Tree.map2(Leaf(2))(x => x * x) == Leaf(4))
  }
}
