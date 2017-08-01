import Hascalator.Prelude.{Bool, False, True}

import scala.language.implicitConversions

/**
  * Root package of Hascalator.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Hascalator {

    type NotNull = org.jetbrains.annotations.NotNull

    type ⊥ = scala.Nothing

    type `_|_` = scala.Nothing

    type Int = Data.Int.Int

    /**
      * Checks that the specified object reference is not `null`. This
      * method is designed primarily for doing parameter validation in methods
      * and constructors, as demonstrated below:
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

    @inline
    implicit def byLazy[A](f: => A): Lazy[A] = new Lazy[A](() => f)
}
