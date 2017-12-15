package Hascalator.Prelude

import scala.annotation.implicitNotFound

/**
  * Created by Glavo on 17-7-31.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("No instance for Real[${A}]")
abstract class Real[A](implicit num: Num[A], ord: Ord[A]) {
  /**
    * the rational equivalent of its real argument with full precision
    */
  def toRational(a: A): Rational
}

object Real {

  class Impl[A: Real](val self: A) {
    def toRational: Rational = implicitly[Real[A]].toRational(self)
  }

}