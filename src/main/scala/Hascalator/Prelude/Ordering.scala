package Hascalator.Prelude

import Hascalator.Base

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
final class Ordering private[Prelude](override val toString: String,
                                      override val hashCode: Int)  extends Base

object Ordering {
    implicit val eqOrdering: Eq[Ordering, Ordering] = Eq
}