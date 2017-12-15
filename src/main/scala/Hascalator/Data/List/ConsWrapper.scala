package Hascalator.Data.List

import Hascalator.Lazy

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
class ConsWrapper[A](tl: => List[A]) {
  def ::[B >: A](hd: B): List[B] = new Cons(hd, tl)
}
