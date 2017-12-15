package Hascalator

import scala.language.implicitConversions
import Data.List._
import Hascalator.Data.Ratio.Ratio

/**
  * The [[Hascalator.Prelude]]: a standard module. The Prelude is imported
  * by default into all Haskell modules unless either there is
  * an explicit import statement for it, or the NoImplicitPrelude
  * extension is enabled.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Prelude {


  def error(message: String = ""): ⊥ = throw new Exception(message)


  def badArg: ⊥ = throw new Exception("bad argument")

  //Basic data types

  //Function


  def id[A](it: A): A = it


  def const[A, B](a: A)(b: B): A = a


  def flip[A, B, C](f: A => B => C): B => A => C =
    b => a => f(a)(b)

  //data Bool

  type Bool = Data.Bool.Bool

  val True: Bool = Data.Bool.True

  val False: Bool = Data.Bool.False

  /**
    * Boolean "and"
    */

  def &&(b1: Bool, b2: Bool): Bool = {
    requireNonNull(b1) && b2
  }

  /**
    * Boolean "or"
    */

  def ||(b1: Bool, b2: Bool): Bool = {
    requireNonNull(b1) || b2
  }

  /**
    * Boolean "not"
    */

  def not(b: Bool): Bool = {
    !requireNonNull(b)
  }


  val otherwise: Bool = True

  //data Maybe

  /**
    * The maybe function takes a default value, a function, and a Maybe value.
    * If the Maybe value is Nothing, the function returns the default value.
    * Otherwise, it applies the function to the value inside the Just and returns
    * the result.
    *
    * Examples:
    *
    * {{{
    *     scala> maybe(False)(odd)(Just(3))
    *     res1: Hascalator.Data.Bool.Bool = False
    *
    *     scala> maybe(False)(odd)(Nothing)
    *     res2: Hascalator.Data.Bool.Bool = False
    * }}}
    *
    */

  def maybe[A, B](b: B)(f: A => B)(m: Maybe[A]): B = {
    requireNonNull(f)
    requireNonNull(m) match {
      case Just(value) => f(value)
      case Nothing => b
    }
  }

  //data Either


  def either[A, B, C](fl: A => C)
                     (fr: B => C)
                     (e: Either[A, B]): C = {
    requireNonNull(fl)
    requireNonNull(fr)

    requireNonNull(e) match {
      case Left(value) => fl(value)
      case Right(value) => fr(value)
    }
  }

  //data Ordering


  val LT: Ordering = new Ordering("LT", -1)


  val EQ: Ordering = new Ordering("EQ", 0)


  val GT: Ordering = new Ordering("GT", 1)

  //data Char


  implicit def charToCh(c: scala.Char): Char = new Char(c)



  implicit def chToChar(ch: Char): scala.Char =
    requireNonNull(ch).self


  implicit def chToCharacter(ch: Char): Character =
    requireNonNull(ch).self

  //type String

  //TODO

  //Tuples

  /**
    * Extract the first component of a pair.
    */

  def fst[A, B](t: (A, B)): A =
    requireNonNull(t)._1

  /**
    * Extract the second component of a pair.
    */

  def snd[A, B](t: (A, B)): B =
    requireNonNull(t)._2

  /**
    * curry converts an uncurried function to a curried function.
    */

  def curry[A, B, C](f: ((A, B)) => C): A => B => C = {
    requireNonNull(f)
    a => b => f((a, b))
  }

  /**
    * uncurry converts a curried function to a function on pairs.
    */

  def uncurry[A, B, C](f: A => B => C): ((A, B)) => C = {
    requireNonNull(f)

    pair => f(pair._1)(pair._2)
  }

  //Basic type classes


  //type class Ord


  implicit def toOrdImpl[T: Ord](t: T): Ord.Impl[T] =
    new Ord.Impl[T](t)


  def compare[T: Ord](t1: T)(t2: T): Ordering =
    implicitly[Ord[T]].compare(t1)(t2)


  final def <[T: Ord](t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => True
      case EQ => False
      case GT => False
    }


  final def <=[T: Ord](t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => True
      case EQ => True
      case GT => False
    }


  final def >[T: Ord](t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => False
      case EQ => False
      case GT => True
    }


  final def >=[T: Ord](t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => False
      case EQ => True
      case GT => True
    }


  def max[T: Ord](t1: T)(t2: T): T =
    compare(t1)(t2) match {
      case LT => t2
      case _ => t1
    }


  def min[T: Ord](t1: T)(t2: T): T =
    compare(t1)(t2) match {
      case LT => t1
      case _ => t2
    }

  //type class Enum


  implicit def toEnumImpl[E: Enum](e: E): Enum.Impl[E] =
    new Enum.Impl[E](e)


  def succ[E: Enum](e: E): E =
    implicitly[Enum[E]].succ(e)


  def pred[E: Enum](e: E): E =
    implicitly[Enum[E]].pred(e)


  def toEnum[E: Enum](i: Int): E =
    implicitly[Enum[E]].toEnum(i)


  def fromEnum[E: Enum](e: E): Int =
    implicitly[Enum[E]].fromEnum(e)


  def enumFrom[E: Enum](e: E): List[E] =
    implicitly[Enum[E]].enumFrom(e)


  def enumFromThen[E: Enum](e1: E)(e2: E): List[E] =
    implicitly[Enum[E]].enumFromThen(e1)(e2)


  def enumFromTo[E: Enum](e1: E)(e2: E): List[E] =
    implicitly[Enum[E]].enumFromTo(e1)(e2)


  def enumFromThenTo[E: Enum](e1: E)(e2: E)(e3: E): List[E] =
    implicitly[Enum[E]].enumFromThenTo(e1)(e2)(e3)

  //type class Bounded


  def minBound[B: Bounded]: B =
    implicitly[Bounded[B]].minBound


  def maxBound[B: Bounded]: B =
    implicitly[Bounded[B]].maxBound

  //Numbers

  //Numeric types

  type Int = Data.Int.Int

  type Integer = scala.BigInt

  /**
    * Single-precision floating point numbers. It is desirable that this type
    * be at least equal in range and precision to the IEEE single-precision type.
    */
  type Float = scala.Float

  /**
    * Double-precision floating point numbers. It is desirable that this type
    * be at least equal in range and precision to the IEEE double-precision type.
    */
  type Double = scala.Double

  /**
    * Arbitrary-precision rational numbers, represented as a ratio of two
    * Integer values. A rational number may be constructed using the % operator.
    */
  type Rational = Ratio[Integer]


  //Numeric type classes

  //type class Num

  /**
    * Unary negation.
    */
  def negate[A: Num](a: A): A =
    implicitly[Num[A]].negate(a)

  /**
    * Absolute value.
    */
  def abs[A: Num](a: A): A =
    implicitly[Num[A]].abs(a)

  /**
    * Sign of a number.
    */
  def signum[A: Num](a: A): A =
    implicitly[Num[A]].signum(a)

  /**
    * Conversion from an Integer. An integer literal represents the application
    * of the function fromInteger to the appropriate value of type Integer,
    * so such literals have type A : Num.
    */
  def fromInteger[A: Num](i: Integer): A =
    implicitly[Num[A]].fromInteger(i)

  //List

  def cycle[A](l: List[A]): List[A] =
    l ++ cycle(l)


  //Functions

  implicit def functionWapper[T1, R](f: T1 => R): Prelude.Function[T1, R] = {
    new Prelude.Function(requireNonNull(f))
  }
}
