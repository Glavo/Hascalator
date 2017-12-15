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

  class Impl[T: Enum](@inline val self: T) {
    def succ = implicitly[Enum[T]].succ(self)

    def pred = implicitly[Enum[T]].pred(self)

    def fromEnum = implicitly[Enum[T]].fromEnum(self)
  }

}
