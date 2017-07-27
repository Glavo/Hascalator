package Hascalator.Prelude

import Hascalator.{Data, _}

/**
  * The character type Char is an enumeration whose values
  * represent Unicode (or equivalently ISO/IEC 10646) characters
  * (see [[http://www.unicode.org/]] for details). This set extends
  * the ISO 8859-1 (Latin-1) character set (the first 256 characters),
  * which is itself an extension of the ASCII character set
  * (the first 128 characters). A character literal in Haskell has type Char.
  *
  * To convert a Char to or from the corresponding Int value
  * defined by Unicode, use toEnum and fromEnum from
  * the Enum class respectively (or equivalently ord and chr).
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Char(@inline
                 @NotNull
                 val self: Character) extends Base {

    {
        requireNonNull(self)
    }

    @inline
    @NotNull
    override def toString = self.toString

    @inline
    override def hashCode(): scala.Int = self.hashCode()

    @inline
    override def equals(obj: scala.Any): Boolean = {
        if (requireNonNull(self).getClass != getClass) false
        else {
            val ch = obj.asInstanceOf[Char]
            ch.self == self
        }
    }


}

object Char {
    @inline
    @NotNull
    def apply(char: scala.Char): Char = new Char(char)

    @inline
    @NotNull
    def apply(@NotNull character: Character): Char =
        new Char(requireNonNull(character))

    @inline
    @NotNull
    def unapply(arg: Char): Option[Character] =
        if (arg != null) Some(arg.self)
        else None

    implicit val InstanceOrd: Ord[Char] = new Ord[Char] {
        override def compare(@NotNull t1: Char)(@NotNull t2: Char): Ordering = {
            requireNonNull(t1)
            requireNonNull(t2)

            val i = t1.self.compareTo(t2.self)

            if (i < 0) LT
            else if (i == 0) EQ
            else GT
        }
    }
}
