package Hascalator.Prelude

import Hascalator._

/**
  * CThe [[Hascalator.Prelude.Maybe]] type encapsulates an optional value. A value of type
  * Maybe a either contains a value of type a (represented as Just a),
  * or it is empty (represented as Nothing). Using Maybe is a good way
  * to deal with errors or exceptional cases without resorting to drastic
  * measures such as error.
  *
  * The Maybe type is also a monad. It is a simple kind of error monad,
  * where all errors are represented by Nothing. A richer error monad can
  * be built using the Either type.
  *
  * @author Glavo
  * @since 0.1.0
  */
abstract sealed class Maybe[+T] protected {

  @inline
  def isEmpty: Bool =
    if (this == Nothing) True
    else False

}

object Maybe {
}

case class Just[+T](value: T) extends Maybe[T]

object Just {
}

case object Nothing extends Maybe[⊥] {
}
