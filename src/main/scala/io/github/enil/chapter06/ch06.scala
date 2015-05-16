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

  /**
   * @author Emil Nilsson
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
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

  /**
   * @author Emil Nilsson
   */
  def nonNegativeIntLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeIntLessThan(n)
    }
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
 * A mock random number generator yielding values from a predefined list of numbers.
 *
 * @author Emil Nilsson
 */
case class MockRNG(ints: Seq[Int]) extends RNG {
  def nextInt: (Int, RNG) = ints match {
    case h :: t => (h, MockRNG(t))
    case _ => throw new RuntimeException("Exhausted mock ints")
  }
}

/**
 * From Functional Programming in Scala.
 */
case class State[S, +A](run: S => (A, S)) {
  /**
   * @author Emil Nilsson
   */
  def apply(s: S): (A, S) = run(s)

  /**
   * @author Emil Nilsson
   */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  /**
   * @author Emil Nilsson
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a)(s2)
    }

  /**
   * @author Emil Nilsson
   */
  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.flatMap(b => State.unit(f(a, b))))
}

/**
 * @author Emil Nilsson
 */
object State {
  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    State(s => fs match {
      case r :: rs =>
        val (a, s2) = r(s)
        val (as, s3) = sequence(rs)(s2)
        (a :: as, s3)
      case _ => (List(), s)
    })

  /**
   * From Functional Programming in Scala.
   */
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  /**
   * From Functional Programming in Scala.
   */
  def get[S]: State[S, S] = State(s => (s, s))

  /**
   * From Functional Programming in Scala.
   */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
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

    val rng = MockRNG(List(-1, -2, 3, 4))

    val (n1, rng1) = nonNegativeInt(rng)
    val (n2, rng2) = nonNegativeInt(rng1)
    val (n3, rng3) = nonNegativeInt(rng2)
    // make sure the returned RNG is in the correct state
    val (n4, _) = rng3.nextInt

    assert(n1 == 0) // -(-1) - 1
    assert(n2 == 1) // -(-2) - 1
    assert(n3 == 3)
    assert(n4 == 4)
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

    val rng = MockRNG(List(10000000, -20000000, 30000000))

    val (f1, rng1) = double(rng)
    val (f2, rng2) = double(rng1)
    // make sure the returned RNG is in the correct state
    val (n, _) = rng2.nextInt

    assert(f1 near 0.0046566129) // (100000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f2 near 0.0093132253) // ((-(-20000000) - 1) % ((2^31)-2)) / ((2^31)-1)
    assert(n == 30000000)
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

  val rng = MockRNG(List(10000000, 20000000, 30000000, 40000000, 50000000, 60000000, 70000000))

  def main(args: Array[String]): Unit = {
    testIntDouble
    testDoubleInt
    testDouble3
  }

  def testIntDouble: Unit = {
    val ((n1, f1), rng1) = intDouble(rng)
    val ((n2, f2), rng2) = intDouble(rng1)
    // make sure the returned RNG is in the correct state
    val (n3, _) = rng2.nextInt

    assert(n1 == 10000000)
    assert(f1 near 0.0093132258) // (20000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == 30000000)
    assert(f2 near 0.0186264515) // (40000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n3 == 50000000)
  }

  def testDoubleInt: Unit = {
    val ((f1, n1), rng1) = doubleInt(rng)
    val ((f2, n2), rng2) = doubleInt(rng1)
    // make sure the returned RNG is in the correct state
    val (n3, _) = rng2.nextInt

    assert(f1 near 0.0046566129) // (10000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n1 == 20000000)

    assert(f2 near 0.0139698386) // (30000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == 40000000)
    assert(n3 == 50000000)
  }

  def testDouble3: Unit = {
    val ((f1, f2, f3), rng1) = double3(rng)
    val ((f4, f5, f6), rng2) = double3(rng1)
    // make sure the returned RNG is in the correct state
    val (n, _) = rng2.nextInt

    assert(f1 near 0.0046566129) // (10000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f2 near 0.0093132258) // (20000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f3 near 0.0139698386) // (30000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f4 near 0.0186264515) // (40000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f5 near 0.0232830644) // (50000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f6 near 0.0279396773) // (60000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n == 70000000)
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

    val rng = MockRNG(List(1, 2, 3, 4))

    val (ns1, rng1) = ints(3)(rng)
    // make sure the returned RNG is in the correct state
    val (n1, _) = rng1.nextInt

    assert(ns1 == List(1, 2, 3))
    assert(n1 == 4)

    val (ns2, rng2) = ints(0)(rng)
    // make sure the returned RNG is in the correct state
    val (n2, _) = rng2.nextInt

    assert(ns2 == List())
    assert(n2 == 1)
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

    val rng = MockRNG(List(10000000, -20000000, 30000000))

    val (f1, rng1) = double2(rng)
    val (f2, rng2) = double2(rng1)
    // make sure the returned RNG is in the correct state
    val (n, _) = rng2.nextInt

    assert(f1 near 0.0046566129) // (100000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f2 near 0.0093132253) // ((-(-20000000) - 1) % ((2^31)-2)) / ((2^31)-1)
    assert(n == 30000000)
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

  val rng = MockRNG(List(10000000, 20000000, 30000000, 40000000, 50000000))

  def main(args: Array[String]): Unit = {
    testIntDouble
    testDoubleInt
  }

  def testIntDouble: Unit = {
    val ((n1, f1), rng1) = randIntDouble(rng)
    val ((n2, f2), rng2) = randIntDouble(rng1)
    // make sure the returned RNG is in the correct state
    val (n3, _) = rng2.nextInt

    assert(n1 == 10000000)
    assert(f1 near 0.0093132258) // (20000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == 30000000)
    assert(f2 near 0.0186264515) // (40000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n3 == 50000000)
  }

  def testDoubleInt: Unit = {
    val ((f1, n1), rng1) = randDoubleInt(rng)
    val ((f2, n2), rng2) = randDoubleInt(rng1)
    // make sure the returned RNG is in the correct state
    val (n3, _) = rng2.nextInt

    assert(f1 near 0.0046566129) // (10000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n1 == 20000000)

    assert(f2 near 0.0139698386) // (30000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == 40000000)
    assert(n3 == 50000000)
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
    val rng = MockRNG(List(1, 2, 3, 4))

    val (ns1, rng1) = randInts(3)(rng)
    // make sure the returned RNG is in the correct state
    val (n1, _) = rng1.nextInt

    assert(ns1 == List(1, 2, 3))
    assert(n1 == 4)

    val (ns2, rng2) = randInts(0)(rng)
    // make sure the returned RNG is in the correct state
    val (n2, _) = rng2.nextInt

    assert(ns2 == List())
    assert(n2 == 1)
  }
}

/**
 * Exercise 6.8: implement flatMap and use it to implement nonNegativeIntLessThan.
 *
 * @author Emil Nilsson
 */
object Exercise68 {
  import RNG._

  def main(args: Array[String]): Unit = {
    val rng = MockRNG(List(Int.MaxValue, 1, 2, 3))

    // first value should be rejected
    val (n1, rng1) = nonNegativeIntLessThan(1000)(rng)
    val (n2, rng2) = nonNegativeIntLessThan(1000)(rng1)
    val (n3, _) = rng2.nextInt

    assert(n1 == 1)
    assert(n2 == 2)
    assert(n3 == 3)
  }
}

/**
 * Exercise 6.9: implement map and map2 using flatMap.
 *
 * @author Emil Nilsson
 */
object Exercise69 {
  import RNG.{Rand, flatMap, unit, int}

  def main(args: Array[String]): Unit = {
    val rng = MockRNG(List(1, 2, 3, 4))

    val (s1, rng1) = map(int)(-_)(rng)
    val (s2, rng2) = map2(int, int)(_ + _)(rng1)
    // make sure the returned RNG is in the correct state
    val (n, _) = rng2.nextInt

    assert(s1 == -1)
    assert(s2 == 5)
    assert(n == 4)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

/**
 * Exercise 6.10: implement unit, map, map2, flatMap and sequence for State.
 *
 * Reimplementations of functions generating random numbers are reimplemented for the State class to test the
 * implementations.
 *
 * @author Emil Nilsson
 */
object Exercise610 {
  import Util.DoubleExtensions

  def main(args: Array[String]): Unit = {
    testNonNegativeInt
    testDouble
    testNonNegativeIntLessThan
    testRandIntDouble
    testInts
  }

  def testNonNegativeInt: Unit = {
    val rng = MockRNG(List(-1, -2, 3, 4))

    val (n1, rng1) = nonNegativeInt(rng)
    val (n2, rng2) = nonNegativeInt(rng1)
    val (n3, rng3) = nonNegativeInt(rng2)
    // make sure the returned RNG is in the correct state
    val (n4, _) = rng3.nextInt

    assert(n1 == 0) // -(-1) - 1
    assert(n2 == 1) // -(-2) - 1
    assert(n3 == 3)
    assert(n4 == 4)
  }

  def testDouble: Unit = {
    val rng = MockRNG(List(10000000, -20000000, 30000000))

    val (f1, rng1) = double(rng)
    val (f2, rng2) = double(rng1)
    // make sure the returned RNG is in the correct state
    val (n, _) = rng2.nextInt

    assert(f1 near 0.0046566129) // (100000000 % ((2^31)-2)) / ((2^31)-1)
    assert(f2 near 0.0093132253) // ((-(-20000000) - 1) % ((2^31)-2)) / ((2^31)-1)
    assert(n == 30000000)
  }

  def testNonNegativeIntLessThan: Unit = {
       val rng = MockRNG(List(Int.MaxValue, 1, 2, 3))

    // first value should be rejected
    val (n1, rng1) = nonNegativeIntLessThan(1000)(rng)
    val (n2, rng2) = nonNegativeIntLessThan(1000)(rng1)
    val (n3, _) = rng2.nextInt

    assert(n1 == 1)
    assert(n2 == 2)
    assert(n3 == 3)
  }

  def testRandIntDouble: Unit = {
    val rng = MockRNG(List(10000000, 20000000, 30000000, 40000000, 50000000))

    val ((n1, f1), rng1) = randIntDouble(rng)
    val ((n2, f2), rng2) = randIntDouble(rng1)
    // make sure the returned RNG is in the correct state
    val (n3, _) = rng2.nextInt

    assert(n1 == 10000000)
    assert(f1 near 0.0093132258) // (20000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n2 == 30000000)
    assert(f2 near 0.0186264515) // (40000000 % ((2^31)-2)) / ((2^31)-1)
    assert(n3 == 50000000)
  }

  def testInts: Unit = {
    val rng = MockRNG(List(1, 2, 3, 4))

    val (ns1, rng1) = ints(3)(rng)
    // make sure the returned RNG is in the correct state
    val (n1, _) = rng1.nextInt

    assert(ns1 == List(1, 2, 3))
    assert(n1 == 4)

    val (ns2, rng2) = ints(0)(rng)
    // make sure the returned RNG is in the correct state
    val (n2, _) = rng2.nextInt

    assert(ns2 == List())
    assert(n2 == 1)
  }

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State { _.nextInt }

  def nonNegativeInt: Rand[Int] = int.map { i =>
    if (i < 0) -i - 1 else i
  }

  def double: Rand[Double] = nonNegativeInt.map { i =>
    (i % (Int.MaxValue - 1)).toFloat / Int.MaxValue
  }

  def nonNegativeIntLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) State.unit(mod) else nonNegativeIntLessThan(n)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = (ra map2 rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))
}

/**
 * Exercise 6.11: implement candy dispenser FSM using State.
 *
 * @author Emil Nilsson
 */
object Exercise611 {
  val machine = Machine(locked = true, candies = 5, coins = 0)

  def main(args: Array[String]): Unit = {
    testBuySome
    testBuyAll
  }

  def testBuySome: Unit = {
    // successfully buys 3 out of 5 candies
    val input: List[Input] = List(Coin, Turn, Turn, Turn, Coin, Coin, Turn, Coin, Turn, Turn)

    val machine = Machine(locked = true, candies = 5, coins = 0)
    val ((candies: Int, coins: Int), _) = simulateMachine(input)(machine)

    assert(candies == 2)
    assert(coins == 3)
  }

  def testBuyAll: Unit = {
    // successfully buys all 5 candies
    val input: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    val machine = Machine(locked = true, candies = 5, coins = 0)
    val ((candies: Int, coins: Int), _) = simulateMachine(input)(machine)

    assert(candies == 0)
    assert(coins == 5)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(Machine.process))
      s <- State.get
    } yield (s.candies, s.coins)

  /**
   * From Functional Programming in Scala.
   */
  sealed trait Input

  /**
   * From Functional Programming in Scala.
   */
  case object Coin extends Input

  /**
   * From Functional Programming in Scala.
   */
  case object Turn extends Input

  /**
   * From Functional Programming in Scala.
   */
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  /**
   * From Functional Programming in Scala.
   */
  object Machine {
    /**
     * @author Emil Nilsson
     */
    def process(input: Input): State[Machine, Unit] = input match {
      case Coin => insertCoin
      case Turn => turnKnob
    }

    /**
     * @author Emil Nilsson
     */
    def insertCoin: State[Machine, Unit] = State.modify(s => (s.locked, s.candies) match {
      case (true, c) if c > 0 => Machine(locked = false, s.candies, coins = s.coins + 1)
      case _ => s
    })

    /**
     * @author Emil Nilsson
     */
    def turnKnob: State[Machine, Unit] = State.modify(s => (s.locked, s.candies) match {
      case (false, c) if c > 0 => Machine(locked = true, candies = s.candies - 1, s.coins)
      case _ => s
    })
  }
}
