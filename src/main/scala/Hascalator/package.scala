import Hascalator.Prelude
import Hascalator.Prelude.{Bool, False, True}

import scala.language.implicitConversions

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Hascalator {

    type NotNull = org.jetbrains.annotations.NotNull

    type ⊥ = scala.Nothing

    /**
      * Checks that the specified object reference is not `null`. This
      * method is designed primarily for doing parameter validation in methods
      * and constructors, as demonstrated below:
      *
      * @param obj the object reference to check for nullity
      * @tparam T the type of the reference
      * @return `obj` if not `null`
      * @throws NullPointerException if `obj` is `null`
      */
    @inline
    @NotNull
    def requireNonNull[T](obj: T): T = {
        if (obj == null)
            throw new NullPointerException
        else obj
    }

    @inline
    @NotNull
    implicit def booleanToBool(b: Boolean): Bool = {
        if (b) True
        else False
    }


    @inline
    implicit def boolToBoolean(@NotNull b: Bool): Boolean = {
        requireNonNull(b)
        if (b == True) true
        else false
    }
}
