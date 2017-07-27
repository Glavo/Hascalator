package Hascalator

/**
  * Created by Glavo on 17-7-27.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Any[A](val value: A) extends AnyVal {

    @inline
    def run[R](f: A => R) = f(value)


}
