package Hascalator.Prelude

import Hascalator._

import scala.annotation.implicitNotFound
import Data.Int._ 

/**
  * Created by Glavo on 17-7-30.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("Values of types ${L} and ${R} cannot be compared with === or /==")
sealed trait Eq[-L <: Any, -R <: Any]

object Eq extends Eq[Any, Any] {

    final implicit class Impl[L](val value: L) extends AnyVal {

        @inline
        def ===[R](other: R)(implicit eq: Eq[L, R]): Bool = {
            if (value == other) True else False
        }

        @inline
        def /==[R](other: R)(implicit eq: Eq[L, R]): Bool = {
            if (value == other) False else True
        }

    }

    implicit val eqChar: Eq[scala.Char, scala.Char] = Eq

    implicit val eqInt8: Eq[Int8, Int8] = Eq

    implicit val eqInt16: Eq[Int16, Int16] = Eq

    implicit val eqInt: Eq[Int, Int] = Eq

    implicit val eqInt64: Eq[Int64, Int64] = Eq

    implicit val eqFloat: Eq[Float, Float] = Eq

    implicit val eqDouble: Eq[Double, Double] = Eq

    implicit val eqString: Eq[String, String] = Eq

    implicit val eqBoolean: Eq[Boolean, Boolean] = Eq

    implicit val eqInt8Int16: Eq[Int8, Int16] = Eq

    implicit val eqInt16Int8: Eq[Int16, Int8] = Eq
    
    implicit val eqInt8Int: Eq[Int8, Int] = Eq

    implicit val eqIntInt8: Eq[Int, Int8] = Eq

    implicit val eqInt8Int64: Eq[Int8, Int64] = Eq

    implicit val eqInt64Int8: Eq[Int64, Int8] = Eq

    implicit val eqInt16Int: Eq[Int16, Int] = Eq

    implicit val eqIntInt16: Eq[Int, Int16] = Eq

    implicit val eqInt16Int64: Eq[Int16, Int64] = Eq

    implicit val eqInt64Int16: Eq[Int64, Int16] = Eq

    implicit val eqIntInt64: Eq[Int, Int64] = Eq

    implicit val eqInt64Int: Eq[Int64, Int] = Eq

    implicit val eqFloatDouble: Eq[Float, Double] = Eq

    implicit val eqDoubleFloat: Eq[Double, Float] = Eq

}
