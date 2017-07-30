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

    def error(message: String = "bad argument"): ⊥ = throw new Exception(message)

    //Basic data types

    //Function

    @inline
    def id[A](it: A): A = it

    @inline
    def const[A, B](a: A)(b: B): A = a

    @inline
    def flip[A, B, C](@NotNull f: A => B => C): B => A => C =
        b => a => f(a)(b)

    //data Bool

    val True: Bool = new Bool("True", 1)
    val False: Bool = new Bool("False", 0)

    @inline
    implicit def functionWapper[T1, R](@NotNull f: T1 => R): Prelude.Function[T1, R] = {
        new Prelude.Function(requireNonNull(f))
    }

    /**
      * Boolean "and"
      */
    @inline
    def &&(@NotNull b1: Bool, @NotNull b2: Bool): Bool = {
        requireNonNull(b1) && b2
    }

    /**
      * Boolean "or"
      */
    @inline
    def ||(@NotNull b1: Bool, @NotNull b2: Bool): Bool = {
        requireNonNull(b1) || b2
    }

    /**
      * Boolean "not"
      */
    @inline
    def not(@NotNull b: Bool): Bool = {
        !requireNonNull(b)
    }

    @NotNull
    val otherwise: Bool = True

    //data Maybe

    @inline
    implicit def eqMaybe[L, R](implicit eq: Eq[L, R]): Eq[Maybe[L], Maybe[R]] = Eq

    @inline
    implicit val eqNothing: Eq[Nothing.type, Nothing.type] = Eq

    @inline
    implicit def eqNothingJust[T](implicit eq: Eq[T, T]): Eq[Nothing.type, Just[T]] = Eq

    @inline
    implicit def eqJustNothing[T](implicit eq: Eq[T, T]): Eq[Just[T], Nothing.type] = Eq

    @inline
    implicit def eqJustMaybe[L, R](implicit eq: Eq[L, R]): Eq[Just[L], Maybe[R]] = Eq

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
      *     res1: Hascalator.Prelude.Bool = False
      *
      *     scala> maybe(False)(odd)(Nothing)
      *     res2: Hascalator.Prelude.Bool = False
      * }}}
      *
      */
    @inline
    def maybe[A, B](b: B)(@NotNull f: A => B)(@NotNull m: Maybe[A]): B = {
        requireNonNull(f)
        requireNonNull(m) match {
            case Just(value) => f(value)
            case Nothing => b
        }
    }

    //data Either

    @inline
    implicit def eqEither[LL, LR, RL, RR]: Eq[Either[LL, LR], Either[RL, RR]] = Eq

    @inline
    implicit def eqLeft[T](implicit eq: Eq[T, T]): Eq[Left[T], Left[T]] = Eq

    @inline
    implicit def eqRight[T](implicit eq: Eq[T, T]): Eq[Right[T], Right[T]] = Eq

    @inline
    implicit def eqLeftRight[L, R](implicit eql: Eq[L, L],
                                   eqr: Eq[R, R]): Eq[Left[L], Right[R]] = Eq

    @inline
    implicit def eqRightLeft[L, R](implicit eql: Eq[L, L],
                                   eqr: Eq[R, R]): Eq[Right[R], Left[L]] = Eq

    @inline
    def either[A, B, C](@NotNull fl: A => C)
                       (@NotNull fr: B => C)
                       (@NotNull e: Either[A, B]): C = {
        requireNonNull(fl)
        requireNonNull(fr)

        requireNonNull(e) match {
            case Left(value) => fl(value)
            case Right(value) => fr(value)
        }
    }

    //data Ordering

    @NotNull
    val LT: Ordering = new Ordering("LT", -1)

    @NotNull
    val EQ: Ordering = new Ordering("EQ", 0)

    @NotNull
    val GT: Ordering = new Ordering("GT", 1)

    //data Char

    @inline
    @NotNull
    implicit def charToCh(c: scala.Char): Char = new Char(c)


    @inline
    implicit def chToChar(@NotNull ch: Char): scala.Char =
        requireNonNull(ch).self

    @inline
    @NotNull
    implicit def chToCharacter(@NotNull ch: Char): Character =
        requireNonNull(ch).self

    //type String

    //TODO

    //Tuples

    /**
      * Extract the first component of a pair.
      */
    @inline
    def fst[A, B](@NotNull t: (A, B)): A =
    requireNonNull(t)._1

    /**
      * Extract the second component of a pair.
      */
    @inline
    def snd[A, B](@NotNull t: (A, B)): B =
    requireNonNull(t)._2

    /**
      * curry converts an uncurried function to a curried function.
      */
    @inline
    @NotNull
    def curry[A, B, C](f: ((A, B)) => C): A => B => C = {
        requireNonNull(f)
        a => b => f((a, b))
    }

    /**
      * uncurry converts a curried function to a function on pairs.
      */
    @inline
    @NotNull
    def uncurry[A, B, C](@NotNull f: A => B => C): ((A, B)) => C = {
        requireNonNull(f)

        pair => f(pair._1)(pair._2)
    }

    //Basic type classes

    //type class Ord

    @inline
    @NotNull
    implicit def toOrdImpl[T: Ord](t: T): Ord.Impl[T] =
        new Ord.Impl[T](t)

    @inline
    @NotNull
    def compare[T: Ord](t1: T)(t2: T): Ordering =
        implicitly[Ord[T]].compare(t1)(t2)

    @inline
    @NotNull
    final def <[T: Ord](@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => True
            case EQ => False
            case GT => False
        }

    @inline
    @NotNull
    final def <=[T: Ord](@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => True
            case EQ => True
            case GT => False
        }

    @inline
    @NotNull
    final def >[T: Ord](@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => False
            case EQ => False
            case GT => True
        }

    @inline
    @NotNull
    final def >=[T: Ord](@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => False
            case EQ => True
            case GT => True
        }

    @inline
    @NotNull
    def max[T: Ord](@NotNull t1: T)(@NotNull t2: T): T =
        compare(t1)(t2) match {
            case LT => t2
            case _ => t1
        }

    @inline
    @NotNull
    def min[T: Ord](@NotNull t1: T)(@NotNull t2: T): T =
        compare(t1)(t2) match {
            case LT => t1
            case _ => t2
        }

    //type class Enum

    @inline
    implicit def toEnumImpl[E: Enum](e: E): Enum.Impl[E] =
        new Enum.Impl[E](e)

    @inline
    def succ[E: Enum](e: E): E =
        implicitly[Enum[E]].succ(e)

    @inline
    def pred[E: Enum](e: E): E =
        implicitly[Enum[E]].pred(e)

    @inline
    def toEnum[E: Enum](i: Int): E =
        implicitly[Enum[E]].toEnum(i)

    @inline
    def fromEnum[E: Enum](e: E): Int =
        implicitly[Enum[E]].fromEnum(e)

    @inline
    def enumFrom[E: Enum](e: E): List[E] =
        implicitly[Enum[E]].enumFrom(e)

    @inline
    def enumFromThen[E: Enum](e1: E)(e2: E): List[E] =
        implicitly[Enum[E]].enumFromThen(e1)(e2)

    @inline
    def enumFromTo[E: Enum](e1: E)(e2: E): List[E] =
        implicitly[Enum[E]].enumFromTo(e1)(e2)

    @inline
    def enumFromThenTo[E: Enum](e1: E)(e2: E)(e3: E): List[E] =
        implicitly[Enum[E]].enumFromThenTo(e1)(e2)(e3)

    //type class Bounded

    @inline
    def minBound[B: Bounded]: B =
        implicitly[Bounded[B]].minBound

    @inline
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


    //type class Eq
    @inline
    implicit def toEqImpl[A](v: A): Eq.Impl[A] = new Eq.Impl(v)
}
