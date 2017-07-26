package Hascalator.Prelude

import Hascalator._
/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
abstract class Ord[T] {
    def compare(@NotNull t1: T)(@NotNull t2: T): Ordering

    @inline
    @NotNull
    final def <(@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => True
            case EQ => False
            case GT => False
        }

    @inline
    @NotNull
    final def <=(@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => True
            case EQ => True
            case GT => False
        }

    @inline
    @NotNull
    final def >(@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => False
            case EQ => False
            case GT => True
        }

    @inline
    @NotNull
    final def >=(@NotNull t1: T)(@NotNull t2: T): Bool =
        compare(t1)(t2) match {
            case LT => False
            case EQ => True
            case GT => True
        }

    @inline
    @NotNull
    def max(@NotNull t1: T)(@NotNull t2: T): T =
        compare(t1)(t2) match {
            case LT => t2
            case _ => t1
        }

    @inline
    @NotNull
    def min(@NotNull t1: T)(@NotNull t2: T): T =
        compare(t1)(t2) match {
            case LT => t1
            case _ => t2
        }
}

object Ord {
    class Impl[T : Ord](@inline val self: T) {

        @inline
        @NotNull
        def <(other: T): Bool =
            implicitly[Ord[T]].<(self)(other)

        @inline
        @NotNull
        def <=(other: T): Bool =
            implicitly[Ord[T]].<=(self)(other)

        @inline
        @NotNull
        def >(other: T): Bool =
            implicitly[Ord[T]].>(self)(other)

        @inline
        @NotNull
        def >=(other: T): Bool =
            implicitly[Ord[T]].>=(self)(other)

        @inline
        @NotNull
        def max(other: T): T =
            implicitly[Ord[T]].max(self)(other)

        @inline
        @NotNull
        def min(other: T): T =
            implicitly[Ord[T]].min(self)(other)
    }
}