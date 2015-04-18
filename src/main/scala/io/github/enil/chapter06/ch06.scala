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
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

/**
 * Exercise 6.1: implement nonNegativeInt using RNG.nextInt.
 *
 * @author Emil Nilsson
 */
object Exercise61 {
  def main (args: Array[String]): Unit = {
    val rng = SimpleRNG(42)

    val (n1, rng1) = nonNegativeInt(rng)
    assert(n1 == 16159453)

    val (n2, _) = nonNegativeInt(rng1)
    assert(n2 == 1281479696) //- 1281479697 - 1
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val nn = if (n < 0) -n -1 else n
    (nn, rng2)
  }
}

/**
 * Exercise 6.2: implement double.
 *
 * @author Emil Nilsson
 */
object Exercise62 {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)

    val (f1, rng1) = double(rng)
    assert(f1 near 0.00752483169) //(16159453 % ((2^31)-2)) / ((2^31)-1)

    val (f2, _) = double(rng1)
    assert(f2 near 0.5967354852) //(1281479696 % ((2^31)-2)) / ((2^31)-1)
  }

  /**
   * Copied from Exercise61
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val nn = if (n < 0) -n -1 else n
    (nn, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val f = (n % (Int.MaxValue - 1)).toFloat / Int.MaxValue
    (f, rng2)
  }

  /**
   * Extension to compare two doubles to determine if they are approximately equal.
   */
  implicit class DoubleExtensions(val d: Double) {
    val epsilon = 1e-8

    def near(d2: Double): Boolean = Math.abs(d - d2) < epsilon
  }
}
