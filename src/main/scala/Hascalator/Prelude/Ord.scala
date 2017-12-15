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


  final def <(t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => True
      case EQ => False
      case GT => False
    }


  final def <=(t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => True
      case EQ => True
      case GT => False
    }


  final def >(t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => False
      case EQ => False
      case GT => True
    }


  final def >=(t1: T)(t2: T): Bool =
    compare(t1)(t2) match {
      case LT => False
      case EQ => True
      case GT => True
    }


  def max(t1: T)(t2: T): T =
    compare(t1)(t2) match {
      case LT => t2
      case _ => t1
    }


  def min(t1: T)(t2: T): T =
    compare(t1)(t2) match {
      case LT => t1
      case _ => t2
    }
}

object Ord {

  trait Impl {

    implicit class RichOrd[T: Ord](val self: T) {


      def <(other: T): Bool =
        implicitly[Ord[T]].<(self)(other)


      def <=(other: T): Bool =
        implicitly[Ord[T]].<=(self)(other)


      def >(other: T): Bool =
        implicitly[Ord[T]].>(self)(other)


      def >=(other: T): Bool =
        implicitly[Ord[T]].>=(self)(other)


      def max(other: T): T =
        implicitly[Ord[T]].max(self)(other)


      def min(other: T): T =
        implicitly[Ord[T]].min(self)(other)
    }


    def compare[T: Ord](t1: T)(t2: T): Ordering =
      implicitly[Ord[T]].compare(t1)(t2)

    def <[T: Ord](t1: T)(t2: T): Bool =
      compare(t1)(t2) match {
        case LT => True
        case EQ => False
        case GT => False
      }

    def <=[T: Ord](t1: T)(t2: T): Bool =
      compare(t1)(t2) match {
        case LT => True
        case EQ => True
        case GT => False
      }

    def >[T: Ord](t1: T)(t2: T): Bool =
      compare(t1)(t2) match {
        case LT => False
        case EQ => False
        case GT => True
      }

    def >=[T: Ord](t1: T)(t2: T): Bool =
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

  }

}