package Hascalator.Prelude

import scala.annotation.implicitNotFound

/**
  * Created by Glavo on 17-7-30.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
trait Eq[-L <: Any, -R <: Any]

object Eq extends Eq[Any, Any] {

    final implicit class Impl[L](val value: L) extends AnyVal {

        @inline
        def ===[R](other: R)(implicit eq: Eq[R, L]): Bool = {
            if (value == other) True else False
        }

        @inline
        def /==[R](other: R)(implicit eq: Eq[R, L]): Bool = {
            if (value == other) False else True
        }

    }

    implicit val eqChar: Eq[scala.Char, scala.Char] = Eq

    implicit val eqByte: Eq[Byte, Byte] = Eq

    implicit val eqShort: Eq[Short, Short] = Eq

    implicit val eqInt: Eq[Int, Int] = Eq

    implicit val eqLong: Eq[Long, Long] = Eq

    implicit val eqFloat: Eq[Float, Float] = Eq

    implicit val eqDouble: Eq[Double, Double] = Eq

    implicit val eqString: Eq[String, String] = Eq

    implicit val eqBoolean: Eq[Boolean, Boolean] = Eq
}
