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
@implicitNotFound("No instance for Enum[${E}]")
abstract class Enum[E] {
    /**
      * the successor of a value. For numeric types, succ adds 1.
      */
    def succ(t: E): E

    /**
      * the predecessor of a value. For numeric types, pred subtracts 1.
      */
    def pred(t: E): E

    /**
      * Convert from an Int.
      */
    def toEnum(i: Int): E

    /**
      * Convert to an Int. It is implementation-dependent what fromEnum
      * returns when applied to a value that is too large to fit in an Int.
      */
    def fromEnum(e: E): Int

    def enumFrom(e: E): List[E]

    def enumFromThen(e1: E)(e2: E): List[E]

    def enumFromTo(e1: E)(e2: E): List[E]

    def enumFromThenTo(e1: E)(e2: E)(e3: E): List[E]
}

object Enum {

    class Impl[T: Enum](@inline val self: T) {
        def succ = implicitly[Enum[T]].succ(self)

        def pred = implicitly[Enum[T]].pred(self)

        def fromEnum = implicitly[Enum[T]].fromEnum(self)
    }

}
