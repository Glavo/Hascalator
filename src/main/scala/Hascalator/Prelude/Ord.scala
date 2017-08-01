package Hascalator.Prelude

import Hascalator._

import scala.annotation.implicitNotFound

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("No instance for Ord[${T}]")
abstract class Ord[T] {
    def compare(t1: T)(t2: T): Ordering

    @inline
    final def <(t1: T)(t2: T): Bool =
        compare(t1)(t2) match {
            case LT => True
            case EQ => False
            case GT => False
        }

    @inline
    final def <=(t1: T)(t2: T): Bool =
        compare(t1)(t2) match {
            case LT => True
            case EQ => True
            case GT => False
        }

    @inline
    final def >(t1: T)(t2: T): Bool =
        compare(t1)(t2) match {
            case LT => False
            case EQ => False
            case GT => True
        }

    @inline
    final def >=(t1: T)(t2: T): Bool =
        compare(t1)(t2) match {
            case LT => False
            case EQ => True
            case GT => True
        }

    @inline
    def max(t1: T)(t2: T): T =
        compare(t1)(t2) match {
            case LT => t2
            case _ => t1
        }

    @inline
    def min(t1: T)(t2: T): T =
        compare(t1)(t2) match {
            case LT => t1
            case _ => t2
        }
}

object Ord {

    final class Impl[T: Ord](@inline val self: T) {

        @inline
        def <(other: T): Bool =
            implicitly[Ord[T]].<(self)(other)

        @inline
        def <=(other: T): Bool =
            implicitly[Ord[T]].<=(self)(other)

        @inline
        def >(other: T): Bool =
            implicitly[Ord[T]].>(self)(other)

        @inline
        def >=(other: T): Bool =
            implicitly[Ord[T]].>=(self)(other)

        @inline
        def max(other: T): T =
            implicitly[Ord[T]].max(self)(other)

        @inline
        def min(other: T): T =
            implicitly[Ord[T]].min(self)(other)
    }

}