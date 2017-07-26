package Hascalator

import Hascalator.Prelude.{False, True}
import org.scalatest._

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
class BoolSpec extends FlatSpec {
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
}
