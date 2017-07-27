package Hascalator.Prelude

import Hascalator._
import Data.List._
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
    @NotNull
    def &&(@NotNull other: Bool): Bool = {
        requireNonNull(other)

        if (this == True && other == True) True
        else False
    }

    /**
      * Boolean "or"
      */
    @inline
    @NotNull
    def ||(other: Bool): Bool = {
        requireNonNull(other)
        if (this == True || other == True) True
        else False
    }

    /**
      * Boolean "not"
      */
    @inline
    @NotNull
    def unary_! : Bool = {
        if (this == True) False
        else True
    }
}

object Bool {
    implicit val InstanceEnum: Enum[Bool] = new Enum[Bool] {

        override def succ(t: Bool): Bool =  t match {
            case False => True
            case True => error()
        }

        override def pred(t: Bool): Bool = t match {
            case False => error()
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
            case (False, False) => ???
        }

        override def toEnum(i: Int): Bool = (i : @switch) match {
            case 0 => False
            case 1 => True
        }

        override def enumFromTo(e1: Bool)(e2: Bool): List[Bool] = ???

        override def enumFromThenTo(e1: Bool)(e2: Bool)(e3: Bool): List[Bool] = ???
    }
}

