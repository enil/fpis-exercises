/*
 * Parts of the source code in this file is based on or copied the source code from the book Functional Programming in
 * Scala.
 * The original source code from Functional Programming in Scala can be found at https://github.com/fpinscala/fpinscala.
 * These code sections are copyrighted by Manning Publications, Co. with the following license:
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
 * All other code are copyrighted by Emil Nilsson with the following license:
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

package io.github.enil.chapter06

/**
 * From Functional Programming in Scala.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

/**
 * From Functional Programming in Scala.
 */
object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * @author Emil Nilsson
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  /**
   * @author Emil Nilsson
   */
  def sequence[A](s: List[Rand[A]]): Rand[List[A]] = {
    rng => s match {
      case r :: rs =>
        val (a, rng2) = r(rng)
        val (as, rng3) = sequence(rs)(rng2)
        (a :: as, rng3)
      case _ => (List(), rng)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  /**
   * @author Emil Nilsson
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val nn = if (n < 0) -n -1 else n
    (nn, rng2)
  }

  /**
   * @author Emil Nilsson
   */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val f = (n % (Int.MaxValue - 1)).toFloat / Int.MaxValue
    (f, rng2)
  }

  /**
   * @author Emil Nilsson
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng1) = rng.nextInt
    val (f, rng2) = double(rng1)
    ((n, f), rng2)
  }

  /**
   * @author Emil Nilsson
   */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (f, rng1) = double(rng)
    val (n, rng2) = rng1.nextInt
    ((f, n), rng2)
  }

  /**
   * @author Emil Nilsson
   */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (f1, rng1) = double(rng)
    val (f2, rng2) = double(rng1)
    val (f3, rng3) = double(rng2)
    ((f1, f2, f3), rng3)
  }

  /**
   * @author Emil Nilsson
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (List(), rng)
    else {
      val (n1, rng1) = rng.nextInt
      val (ns, rng2) = ints(count - 1)(rng1)
      (n1 :: ns, rng2)
    }
  }

  /**
   * @author Emil Nilsson
   */
  def double2: RNG.Rand[Double] =
    RNG.map(nonNegativeInt) { n =>
      (n % (Int.MaxValue - 1)).toFloat / Int.MaxValue
    }

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def randInts(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}

/**
 * From Functional Programming in Scala.
 */
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

/**
 * @author Emil Nilsson
 */
object Util {
  /**
   * Extension to compare two doubles to determine if they are approximately equal.
   */
  implicit class DoubleExtensions(val d: Double) {
    val epsilon = 1e-7

    def near(d2: Double): Boolean = Math.abs(d - d2) < epsilon
  }
}

/**
 * Exercise 6.1: implement nonNegativeInt using RNG.nextInt.
 *
 * @author Emil Nilsson
 */
object Exercise61 {
  def main (args: Array[String]): Unit = {
    import RNG._

    val rng = SimpleRNG(42)

    val (n1, rng1) = nonNegativeInt(rng)
    assert(n1 == 16159453)

    val (n2, _) = nonNegativeInt(rng1)
    assert(n2 == 1281479696) // -1281479697 - 1
  }
}

/**
 * Exercise 6.2: implement double.
 *
 * @author Emil Nilsson
 */
object Exercise62 {
  def main(args: Array[String]): Unit = {
    import RNG._
    import Util.DoubleExtensions

    val rng = SimpleRNG(42)

    val (f1, rng1) = double(rng)
    assert(f1 near 0.00752483169) // (16159453 % ((2^31)-2)) / ((2^31)-1)

    val (f2, _) = double(rng1)
    assert(f2 near 0.5967354852) // (1281479696 % ((2^31)-2)) / ((2^31)-1)
  }
}

/**
 * Exercise 6.3: implement intDouble, doubleInt and double3.
 *
 * @author Emil Nilsson
 */
object Exercise63 {
  import RNG._
  import Util.DoubleExtensions

  val rng = SimpleRNG(42)

  def main(args: Array[String]): Unit = {
    testIntDouble
    testDoubleInt
    testDouble3
  }

  def testIntDouble: Unit = {
    val ((n1, f1), rng1) = intDouble(rng)
    assert(n1 == 16159453)
    assert(f1 near 0.5967354852) // (1281479696 % ((2^31)-2)) / ((2^31)-1)

    val ((n2, f2), _) = intDouble(rng1)
    assert(n2 == -340305902)
    assert(f2 near 0.9386595431) // ((-(-2015756020)-1) % ((2^31)-2)) / ((2^31)-1)
  }

  def testDoubleInt: Unit = {
    val ((f1, n1), rng1) = doubleInt(rng)
    assert(f1 near 0.00752483169) // (16159453 % ((2^31)-2)) / ((2^31)-1)
    assert(n1 == -1281479697)

    val ((f2, n2), _) = doubleInt(rng1)
    assert(f2 near 0.158467284) // ((-(-340305902)-1) % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == -2015756020)
  }

  def testDouble3: Unit = {
    val ((f1, f2, f3), rng1) = double3(rng)
    assert(f1 near 0.0075248317) // (16159453 % ((2^31)-2)) / ((2^31)-1)
    assert(f2 near 0.5967354852) // ((-(-1281479697)-1) % ((2^31)-2)) / ((2^31)-1)
    assert(f3 near 0.158467284) // ((-(-340305902)-1) % ((2^31)-2)) / ((2^31)-1)

    val ((f4, f5, f6), _) = double3(rng1)
    assert(f4 near 0.9386595431) // ((-(-2015756020)-1) % ((2^31)-2)) / ((2^31)-1)
    assert(f5 near 0.8242210927) // (1770001318 % ((2^31)-2)) / ((2^31)-1)
    assert(f6 near 0.9008632316) // ((-(-1934589059)-1) % ((2^31)-2)) / ((2^31)-1)
  }
}

/**
 * Exercise 6.4: implement ints.
 *
 * @author Emil Nilsson
 */
object Exercise64 {
  def main(args: Array[String]): Unit = {
    import RNG._

    val rng = SimpleRNG(42)

    val (ns1, rng1) = ints(3)(rng)
    val (n1, _) = rng1.nextInt
    assert(ns1 == List(16159453, -1281479697, -340305902))
    assert(n1 == -2015756020)

    val (ns2, rng2) = ints(0)(rng)
    val (n2, _) = rng2.nextInt
    assert(ns2 == List())
    assert(n2 == 16159453)
  }
}

/**
 * Exercise 6.5: implement double using map.
 *
 * @author Emil Nilsson
 */
object Exercise65 {
  def main(args: Array[String]): Unit = {
    import RNG._
    import Util.DoubleExtensions

    val rng = SimpleRNG(42)

    val (f1, rng1) = double2(rng)
    assert(f1 near 0.00752483169) // (16159453 % ((2^31)-2)) / ((2^31)-1)

    val (f2, _) = double2(rng1)
    assert(f2 near 0.5967354852) // (1281479696 % ((2^31)-2)) / ((2^31)-1)
  }
}

/**
 * Exercise 6.6: implement map2.
 *
 * @author Emil Nilsson
 */
object Exercise66 {
  import RNG._
  import Util.DoubleExtensions

  val rng = SimpleRNG(42)

  def main(args: Array[String]): Unit = {
    testIntDouble
    testDoubleInt
  }

  def testIntDouble: Unit = {
    val ((n1, f1), rng1) = randIntDouble(rng)
    assert(n1 == 16159453)
    assert(f1 near 0.5967354852) // (1281479696 % ((2^31)-2)) / ((2^31)-1)

    val ((n2, f2), _) = randIntDouble(rng1)
    assert(n2 == -340305902)
    assert(f2 near 0.9386595431) // ((-(-2015756020)-1) % ((2^31)-2)) / ((2^31)-1)
  }

  def testDoubleInt: Unit = {
    val ((f1, n1), rng1) = randDoubleInt(rng)
    assert(f1 near 0.00752483169) // (16159453 % ((2^31)-2)) / ((2^31)-1)
    assert(n1 == -1281479697)

    val ((f2, n2), _) = randDoubleInt(rng1)
    assert(f2 near 0.158467284) // ((-(-340305902)-1) % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == -2015756020)
  }
}

/**
 * Exercise 6.7: implement ints using sequence.
 *
 * @author Emil Nilsson
 */
object Exercise67 {
  import RNG._

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)

    val (ns1, rng1) = randInts(3)(rng)
    val (n1, _) = rng1.nextInt
    assert(ns1 == List(16159453, -1281479697, -340305902))
    assert(n1 == -2015756020)

    val (ns2, rng2) = randInts(0)(rng)
    val (n2, _) = rng2.nextInt
    assert(ns2 == List())
    assert(n2 == 16159453)
  }
}
