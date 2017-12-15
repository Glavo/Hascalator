package Hascalator.Prelude

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Function[-T1, +R](val self: T1 => R) extends AnyVal {


  def $(t: T1): R = self(t)


  def `.`[T](other: T => T1): T => R = it => self(other(it))
}
