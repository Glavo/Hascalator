package Hascalator.Prelude

import Hascalator.⊥

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
sealed abstract class Either[+L, +R] {

}

object Either {

}

final case class Left[+T](value: T) extends Either[T, ⊥]

object Left {
}

final case class Right[+T](value: T) extends Either[⊥, T]

object Right {
}
