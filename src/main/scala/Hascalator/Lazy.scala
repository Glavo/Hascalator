package Hascalator

import scala.annotation.unchecked.uncheckedStable

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Lazy[+T](f: () => T,
                     var hasInit: Boolean = false) extends (() => T) {

    lazy val value: T = f()

    def unary_! : T = {
        if (hasInit) value
        else {
            hasInit = true
            value
        }
    }

    override def apply(): T = !this
}

object Lazy {
    def apply[T](f: => T, b: Boolean = false): Lazy[T] = new Lazy(() => f, b)
}