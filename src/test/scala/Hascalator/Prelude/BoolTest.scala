package Hascalator.Prelude

import Hascalator._
import Data.List._

import org.scalatest._

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
class BoolTest extends FlatSpec {
    "True && True" should "be True" in {
        assert((True && True) == True)
    }

    "True && False" should "be False" in {
        assert((True && False) == False)
    }

    "False && True" should "be False" in {
        assert((False && True) == False)
    }

    "False && False" should "be False" in {
        assert((False && False) == False)
    }

    "True || True" should "be True" in {
        assert((True || True) == True)
    }

    "True || False" should "be True" in {
        assert((True || False) == True)
    }

    "False || True" should "be True" in {
        assert((False || True) == True)
    }

    "False || False" should "be False" in {
        assert((False || False) == False)
    }

    "[False ..]" should "be [False, True]" in {
        assert(enumFrom(False) == List(False, True))
    }

    "[True ..]" should "be [True]" in {
        assert(enumFrom(True) == List(True))
    }

    "[False, False ..]" should "be [False, ...]" in {
        var list = enumFromThen(False)(False)

        for (_ <- 1 to 100) {
            assert(list.head == False)
            list = list.tail
        }
    }

    "[False, True ..]" should "be [False, True]" in {
        assert(enumFromThen(False)(True) == List(False, True))
    }

    "[True, False ..]" should "be [True, False]" in {
        assert(enumFromThen(True)(False) == List(True, False))
    }

    "[True, True ..]" should "be [True, ...]" in {
        var list = enumFromThen(True)(True)

        for (_ <- 1 to 100) {
            assert(list.head == True)
            list = list.tail
        }
    }

    "[False .. False]" should "be [False]" in {
        assert(enumFromTo(False)(False) == List(False))
    }

    "[False .. True]" should "be [False, True]"in {
        assert(enumFromTo(False)(True) == List(False, True))
    }

    "[True .. False]" should "be []"in {
        assert(enumFromTo(True)(False) == List())
    }

    "[True .. True]" should "be [True]"in {
        assert(enumFromTo(True)(True) == List(True))
    }

    "[False, False .. False]" should "be [False, ...]" in {
        var list = enumFromThenTo(False)(False)(False)

        for (_ <- 1 to 100) {
            assert(list.head == False)
            list = list.tail
        }
    }

    "[False, False .. True]" should "be [False, ...]" in {
        var list = enumFromThenTo(False)(False)(False)

        for (_ <- 1 to 100) {
            assert(list.head == False)
            list = list.tail
        }
    }

    "[False, True .. False]" should "be [False]" in {
        assert(enumFromThenTo(False)(True)(False) == List(False))
    }

    "[False, True .. True]" should "be [False, True]" in {
        assert(enumFromThenTo(False)(True)(True) == List(False, True))
    }

    "[True, False .. False]" should "be [True, False]" in {
        assert(enumFromThenTo(True)(False)(False) == List(True, False))
    }

    "[True, False .. True]" should "be [True]" in {
        assert(enumFromThenTo(True)(False)(True) == List(True))
    }

    "[True, True .. False]" should "be []" in {
        assert(enumFromThenTo(True)(True)(False) == List())
    }

    "[True, True .. True]" should "be [True, ...]" in {
        var list = enumFromThenTo(True)(True)(True)

        for (_ <- 1 to 100) {
            assert(list.head == True)
            list = list.tail
        }
    }
}
