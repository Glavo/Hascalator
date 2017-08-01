package Hascalator

import scala.annotation.implicitNotFound

/**
  * Created by Glavo on 17-8-1.
  *
  * @author Glavo
  * @since 0.1.0
  */
@implicitNotFound("${A} can't be null")
trait Nullable[A]
