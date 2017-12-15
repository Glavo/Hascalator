package Hascalator.Prelude

import scala.annotation.implicitNotFound

/**
  * The Bounded class is used to name the upper and lower
  * limits of a type. Ord is not a superclass of Bounded since
  * types that are not totally ordered may also have upper and lower bounds.
  *
  * The Bounded class may be derived for any enumeration type; minBound
  * is the first constructor listed in the data declaration and maxBound
  * is the last. Bounded may also be derived for single-constructor datatypes
  * whose constituent types are in Bounded.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("No instance for Bounded[${B}]")
abstract class Bounded[B] {
  def minBound: B

  def maxBound: B
}

object Bounded {
  trait Impl {
    def minBound[B: Bounded]: B =
      implicitly[Bounded[B]].minBound


    def maxBound[B: Bounded]: B =
      implicitly[Bounded[B]].maxBound
  }
}