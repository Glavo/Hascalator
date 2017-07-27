package Hascalator.Data.List

import Hascalator._
import Data.Function._

import scala.annotation.unchecked.uncheckedStable
import scala.annotation.varargs
import scala.collection.immutable.Stream
import scala.language.implicitConversions

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
abstract sealed class List[+A] extends Base {
    /** Indicates whether or not the `Stream` is empty.
      *
      * @return `true` if the `Stream` is empty and `false` otherwise.
      */
    def isEmpty: Boolean

    /** Gives constant time access to the first element of this `Stream`.  Using
      * the `fibs` example from earlier:
      *
      * {{{
      * println(fibs head)
      * // prints
      * // 0
      * }}}
      *
      * @return The first element of the `Stream`.
      * @throws java.util.NoSuchElementException if the stream is empty.
      */
    def head: A

    /** A stream consisting of the remaining elements of this stream after the
      * first one.
      *
      * Note that this method does not force evaluation of the `Stream` but merely
      * returns the lazy result.
      *
      * @return The tail of the `Stream`.
      * @throws UnsupportedOperationException if the stream is empty.
      */
    def tail: List[A]

    def baseString: String

    protected def tailDefined: Boolean

    @inline
    final def ++[B >: A](other: List[B]): List[B] = this match {
        case head :: tail => head :: (tail ++ other)
        case Nil => other
    }

    @inline
    final def !::[B >: A](b: B): List[B] =
        new Cons(b, this)

    @inline
    final def unary_! : List[A] = {
        if (this eq Nil) {}
        else !tail

        this
    }

    override def equals(obj: scala.Any): Boolean = obj match {
        case Nil => this eq Nil
        case head :: tail => this != Nil && head == this.head && tail == this.tail
        case _ => false
    }

    override def toString: String = s"[$baseString]"
}

object List {

    def apply[A](@varargs args: A*): List[A] = {
        var list: List[A] = Nil
        args.reverseIterator

        list
    }


    @inline
    implicit def listBuilder[T](f: => List[T]): ConsWrapper[T] =
        new ConsWrapper(f)
}

final class Cons[+A](override val head: A,
                     tl: => List[A]) extends List[A] {

    override def isEmpty = false

    @volatile private[this] var tlVal: List[A] = _
    @volatile private[this] var tlGen = tl _

    def tailDefined: Boolean = tlGen eq null

    override def tail: List[A] = {
        if (!tailDefined)
            synchronized {
                if (!tailDefined) {
                    tlVal = tlGen()
                    tlGen = null
                }
            }
        tlVal
    }

    override def baseString: String =
        head + {
            if (tailDefined)
                if (tail == Nil) ""
                else ", " + tail.baseString
            else ", ..."
        }


}

object Nil extends List[⊥] {
    @inline
    def apply[A]: List[A] = this

    override def isEmpty = true

    override def head = throw new NoSuchElementException("head of empty stream")

    override def tail = throw new UnsupportedOperationException("tail of empty stream")

    def tailDefined = false

    @inline
    def ::[A](a: A): List[A] = new Cons[A](a, this)

    override def equals(obj: scala.Any): Boolean =
        obj.asInstanceOf[AnyRef] eq this

    override def baseString: String = ""
}

object :: {
    def unapply[A](arg: Cons[A]): Option[(A, List[A])] =
        Some((arg.head, arg.tail))
}