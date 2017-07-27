package Hascalator.Data.Function

import scala.collection.mutable

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Function[T, R](val self: T => R) extends AnyVal {
    def buffered: T => R =
        new ((T) => R) {
            val map: mutable.Map[T, R] = mutable.Map.empty

            override def apply(v1: T): R = map.getOrElseUpdate(v1, self(v1))
        }
}
