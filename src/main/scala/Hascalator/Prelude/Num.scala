package Hascalator.Prelude

import scala.annotation.implicitNotFound

/**
  * Created by Glavo on 17-7-27.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("No instance for Num[${A}]")
abstract class Num[A] {

  /**
    * Unary negation.
    */
  def negate(a: A): A

  /**
    * Absolute value.
    */
  def abs(a: A): A

  /**
    * Sign of a number.
    */
  def signum(a: A): A

  /**
    * Conversion from an Integer. An integer literal represents the application
    * of the function fromInteger to the appropriate value of type Integer,
    * so such literals have type A : Num.
    */
  def fromInteger(i: Integer): A
}

object Num {

  trait Impl {

    implicit class RichNum[A: Num](val self: A) {


      def negate: A = implicitly[Num[A]].negate(self)


      def abs: A = implicitly[Num[A]].abs(self)


      def signum: A = implicitly[Num[A]].signum(self)
    }

    /**
      * Unary negation.
      */
    def negate[A: Num](a: A): A =
      implicitly[Num[A]].negate(a)

    /**
      * Absolute value.
      */
    def abs[A: Num](a: A): A =
      implicitly[Num[A]].abs(a)

    /**
      * Sign of a number.
      */
    def signum[A: Num](a: A): A =
      implicitly[Num[A]].signum(a)

    /**
      * Conversion from an Integer. An integer literal represents the application
      * of the function fromInteger to the appropriate value of type Integer,
      * so such literals have type A : Num.
      */
    def fromInteger[A: Num](i: Integer): A =
      implicitly[Num[A]].fromInteger(i)

  }

}
