package Hascalator

/**
  * Created by Glavo on 17-7-27.
  *
  * @author Glavo
  * @since 0.1.0
  */
class Exception(message: String) extends RuntimeException(message)

object Exception {
    @inline
    def error(message: String = ""): ⊥ =
        throw new Exception(message)

    @inline
    def badArg : ⊥ = throw new Exception("bad argument")
}
