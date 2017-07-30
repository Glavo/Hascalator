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
    def eqEither[LL, LR, RL, RR]: Eq[Either[LL, LR], Either[RL, RR]] = Eq
}

final case class Left[T](value: T) extends Either[T, ⊥]

final case class Right[T](value: T) extends Either[⊥, T]
