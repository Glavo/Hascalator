package Hascalator.Prelude

import Hascalator.requireNonNull
import org.jetbrains.annotations.NotNull

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Bool private[Prelude](override val toString: String,
                                     override val hashCode: Int) {

    /**
      * Boolean "and"
      */
    @inline
    @NotNull
    def &&(@NotNull other: Bool): Bool = {
        requireNonNull(other)

        if (this == True && other == True) True
        else False
    }

    /**
      * Boolean "or"
      */
    @inline
    @NotNull
    def ||(other: Bool): Bool = {
        requireNonNull(other)
        if (this == True || other == True) True
        else False
    }

    /**
      * Boolean "not"
      */
    @inline
    @NotNull
    def unary_! : Bool = {
        if (this == True) False
        else True
    }
}

object Bool {

}

