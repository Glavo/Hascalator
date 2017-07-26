package Hascalator.Prelude

import org.jetbrains.annotations.NotNull

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Function[-T1, +R](@NotNull val self: T1 => R) extends AnyVal {

    @inline
    def $(t: T1): R = self(t)

    @inline
    @NotNull
    def `.`[T](@NotNull other: T => T1): T => R = it => self(other(it))
}