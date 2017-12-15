package Hascalator.Data

import scala.language.implicitConversions

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Function {


  implicit def wrapper[T, R](f: T => R): Function[T, R] = new Function(f)


}
