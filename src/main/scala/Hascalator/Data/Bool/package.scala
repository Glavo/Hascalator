package Hascalator.Data

/**
  * Created by Glavo on 17-8-1.
  *
  * @author Glavo
  * @since 0.1.0
  */
package object Bool {
  val True: Bool = Bool.True
  val False: Bool = Bool.False

  /**
    * Boolean "and"
    */
  @inline
  def &&(b1: Bool, b2: Bool): Bool = {
    b1 && b2
  }

  /**
    * Boolean "or"
    */
  @inline
  def ||(b1: Bool, b2: Bool): Bool = {
    b1 || b2
  }

  /**
    * Boolean "not"
    */
  @inline
  def not(b: Bool): Bool = {
    !b
  }

  val otherwise: Bool = True
}
