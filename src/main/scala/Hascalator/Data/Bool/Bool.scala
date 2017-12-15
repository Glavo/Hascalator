package Hascalator.Data.Bool

import Hascalator.Data.List._
import Hascalator.Prelude._
import Hascalator._

import scala.annotation.switch

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
sealed abstract class Bool {

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

  case object True extends Bool

  case object False extends Bool

  implicit object InstanceEnum extends Enum[Bool] {
    val listTrue: List[Bool] = True :: Nil
    val listFalse: List[Bool] = False :: Nil
    val listTrueFalse: List[Bool] = True !:: False :: Nil
    val listFalseTrue: List[Bool] = False !:: True :: Nil

    @inline
    override def succ(t: Bool): Bool = t match {
      case False => True
      case True => badArg
    }

    @inline
    override def pred(t: Bool): Bool = t match {
      case False => badArg
      case True => False
    }

    @inline
    override def fromEnum(e: Bool): Int = e match {
      case False => 0
      case True => 1
    }

    @inline
    override def enumFrom(e: Bool): List[Bool] = e match {
      case False => listFalseTrue
      case True => listTrue
    }

    @inline
    override def toEnum(i: Int): Bool = (i: @switch) match {
      case 0 => False
      case 1 => True
      case _ => badArg
    }

    @inline
    override def enumFromThen(e1: Bool)(e2: Bool): List[Bool] = (e1, e2) match {
      case (False, False) => cycle(False :: Nil)
      case (False, True) => listFalseTrue
      case (True, False) => listTrueFalse
      case (True, True) => cycle(True :: Nil)
    }

    @inline
    override def enumFromTo(e1: Bool)(e2: Bool): List[Bool] = (e1, e2) match {
      case (False, False) => listFalse
      case (False, True) => listFalseTrue
      case (True, False) => Nil
      case (True, True) => listTrue
    }

    @inline
    override def enumFromThenTo(e1: Bool)(e2: Bool)(e3: Bool): List[Bool] = (e1, e2, e3) match {
      case (False, False, False) => cycle(False :: Nil)
      case (False, False, True) => cycle(False :: Nil)
      case (False, True, False) => listFalse
      case (False, True, True) => listFalseTrue
      case (True, False, False) => listTrueFalse
      case (True, False, True) => listTrue
      case (True, True, False) => Nil
      case (True, True, True) => cycle(True :: Nil)
    }
  }

}

