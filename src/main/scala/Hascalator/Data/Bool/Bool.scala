package Hascalator.Data.Bool

import Hascalator.Data.List._
import Hascalator.Prelude._
import Hascalator._
import org.jetbrains.annotations.NotNull

import scala.annotation.switch

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Bool private[Prelude](override val toString: String,
                                  override val hashCode: Int) extends Base {

    /**
      * Boolean "and"
      */
    @inline
    def &&(other: Bool): Bool = {
        requireNonNull(other)

        if (this == True && other == True) True
        else False
    }

    /**
      * Boolean "or"
      */
    @inline
    def ||(other: Bool): Bool = {
        requireNonNull(other)
        if (this == True || other == True) True
        else False
    }

    /**
      * Boolean "not"
      */
    @inline
    def unary_! : Bool = {
        if (this == True) False
        else True
    }
}

object Bool {
    implicit object InstanceEnum extends Enum[Bool] {

        override def succ(t: Bool): Bool = t match {
            case False => True
            case True => basArg
        }

        override def pred(t: Bool): Bool = t match {
            case False => basArg
            case True => False
        }

        override def fromEnum(e: Bool): Int = e match {
            case False => 0
            case True => 1
        }

        override def enumFrom(e: Bool): List[Bool] = e match {
            case False => False :: True :: Nil
            case True => True :: Nil
        }

        override def enumFromThen(e1: Bool)(e2: Bool): List[Bool] = (e1, e1) match {
            case (False, False) => cycle(False :: Nil)
            case (False, True) => False !:: True :: Nil
            case (True, False) => True !:: False :: Nil
            case (True, True) => cycle(True :: Nil)
        }

        override def toEnum(i: Int): Bool = (i: @switch) match {
            case 0 => False
            case 1 => True
        }

        override def enumFromTo(e1: Bool)(e2: Bool): List[Bool] = (e1, e2) match {
            case (False, False) => False :: Nil
            case (False, True) => False !:: True :: Nil
            case (True, False) => Nil
            case (True, True) => True :: Nil
        }

        override def enumFromThenTo(e1: Bool)(e2: Bool)(e3: Bool): List[Bool] = (e1, e2, e3) match {
            case (False, False, False) => cycle(False :: Nil)
            case (False, False, True) => cycle(False :: Nil)
            case (False, True, False) => False :: Nil
            case (False, True, True) => False !:: True :: Nil
            case (True, False, False) => True !:: False :: Nil
            case (True, False, True) => True :: Nil
            case (True, True, False) => Nil
            case (True, True, True) => cycle(True :: Nil)

        }
    }

    implicit val eqBool: Eq[Bool, Bool] = Eq
}

