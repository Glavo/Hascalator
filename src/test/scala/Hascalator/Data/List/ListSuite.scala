package Hascalator.Data.List

import org.scalatest._

/**
  * Created by Glavo on 17-7-27.
  *
  * @author Glavo
  * @since 0.1.0
  */
class ListSuite extends FunSuite {
    test("equals") {
        assert(
            (1 :: 2 :: 3 :: Nil) ==
                (1 :: 2 :: 3 :: Nil)
        )

        assert(
            ("Scala" :: "Haskell" :: "Java" :: Nil) ==
                ("Scala" :: "Haskell" :: "Java" :: Nil))

        assert(Nil == Nil)

        assert((1 :: Nil) != Nil)

        assert(("Lisp" :: Nil) != ("Scala" :: Nil))
    }

    test("++") {
        val l1 = "Java" :: "C++" :: Nil

        val l2 = "Haskell" :: "Erlang" :: Nil

        assert(
            (l1 ++ l2) == ("Java" :: "C++" :: "Haskell" :: "Erlang" :: Nil)
        )

        assert(
            Nil ++ Nil == Nil
        )
    }

    test("apply") {
        assert(("Long" :: "Int" :: "Byte" :: Nil) ==
            List("Long", "Int", "Byte"))

        assert(Nil == List())
    }
}
