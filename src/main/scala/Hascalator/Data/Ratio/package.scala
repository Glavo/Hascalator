package Hascalator.Data

import Hascalator._
import Prelude._

/**
  * Created by Glavo on 17-7-27.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Ratio {

  /**
    * Arbitrary-precision rational numbers, represented as a ratio of two
    * Integer values. A rational number may be constructed using the % operator.
    */
  type Rational = Ratio[Integer]

  //TODO

  def %[A](a1: A)(a2: A): Ratio[A] =
    new Ratio[A](a1, a2)

  /**
    * Extract the numerator of the ratio in reduced form: the numerator
    * and denominator have no common factor and the denominator is positive.
    */
  def numerator[A](ratio: Ratio[A]): A = ratio.numerator

  /**
    * Extract the denominator of the ratio in reduced form: the numerator
    * and denominator have no common factor and the denominator is positive.
    */
  def denominator[A](ratio: Ratio[A]): A = ratio.denominator
}
