package Hascalator.Data

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Int {
  /**
    * A fixed-precision integer type with at least the range [-2^29 .. 2^29-1].
    * The exact range for a given implementation can be determined by using
    * minBound and maxBound from the Bounded class.
    */
  type Int = scala.Int

  type Int8 = scala.Byte

  type Int16 = scala.Short

  type Int32 = scala.Int

  type Int64 = scala.Long
}


