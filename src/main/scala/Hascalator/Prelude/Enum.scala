package Hascalator.Prelude

import Hascalator._
import Data.List._

import scala.annotation.implicitNotFound

/**
  * Class Enum defines operations on sequentially ordered types.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("No instance for Enum[${A}]")
abstract class Enum[A] {
  /**
    * the successor of a value. For numeric types, succ adds 1.
    */
  def succ(t: A): A

  /**
    * the predecessor of a value. For numeric types, pred subtracts 1.
    */
  def pred(t: A): A

  /**
    * Convert from an Int.
    */
  def toEnum(i: Int): A

  /**
    * Convert to an Int. It is implementation-dependent what fromEnum
    * returns when applied to a value that is too large to fit in an Int.
    */
  def fromEnum(e: A): Int

  def enumFrom(e: A): List[A]

  def enumFromThen(e1: A)(e2: A): List[A]

  def enumFromTo(e1: A)(e2: A): List[A]

  def enumFromThenTo(e1: A)(e2: A)(e3: A): List[A]
}

object Enum {

  trait Impl {
    implicit class RichEnum[T: Enum](val self: T) {
      def succ: T = implicitly[Enum[T]].succ(self)

      def pred: T = implicitly[Enum[T]].pred(self)

      def fromEnum: Int = implicitly[Enum[T]].fromEnum(self)
    }

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
  }
}
