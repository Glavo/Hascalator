package Hascalator

import scala.language.implicitConversions

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

    //data Bool

    @NotNull
    val True: Bool = new Bool("True", 1)
    @NotNull
    val False: Bool = new Bool("False", 0)

    @inline
    @NotNull
    implicit def functionWapper[T1, R](@NotNull f: T1 => R): Prelude.Function[T1, R] = {
        new Prelude.Function(requireNonNull(f))
    }

    /**
      * Boolean "and"
      */
    @inline
    @NotNull
    def &&(@NotNull b1: Bool, @NotNull b2: Bool): Bool = {
        requireNonNull(b1) && b2
    }

    /**
      * Boolean "or"
      */
    @inline
    @NotNull
    def ||(@NotNull b1: Bool, @NotNull b2: Bool): Bool = {
        requireNonNull(b1) || b2
    }

    /**
      * Boolean "not"
      */
    @inline
    @NotNull
    def not(@NotNull b: Bool): Bool = {
        !requireNonNull(b)
    }

    @NotNull
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
      *     res1: Hascalator.Prelude.Bool = False
      *
      *     scala> maybe(False)(odd)(Nothing)
      *     res2: Hascalator.Prelude.Bool = False
      * }}}
      *
      */
    @inline
    def maybe[A, B](b: B)(@NotNull f: A => B)(@NotNull m: Maybe[A]): B = {
        Nil.fold _
        requireNonNull(f)
        requireNonNull(m) match {
            case Just(value) => f(value)
            case Nothing => b
        }
    }

    //data Either

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

    /*
    @inline
    implicit def chToChar(@NotNull ch: Char): scala.Char =
        requireNonNull(ch).self
*/
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


    //type class Ord

    @inline
    @NotNull
    implicit def toOrdImpl[T : Ord](t: T): Ord.Impl[T] =
        new Ord.Impl[T](t)
}
