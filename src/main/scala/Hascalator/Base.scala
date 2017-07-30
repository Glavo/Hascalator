package Hascalator

import Hascalator.Prelude._

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
trait Base  {

    @inline
    final def ===[A](other: A)(implicit eq: Eq[this.type, A]): Bool = {
        if(this == other) True else False
    }

    @inline
    final def /==[A](other: A)(implicit eq: Eq[this.type, A]) : Bool = {
        if(this == other) False else True
    }
}
