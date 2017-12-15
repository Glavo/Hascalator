package Hascalator.Data.List

import Hascalator._
import scala.language.implicitConversions

/**
  * Created by Glavo on 17-7-26.
  *
  * @author Glavo
  * @since 0.1.0
  */
abstract sealed class List[+A] {

    /**
      * Indicates whether or not the `List` is empty.
      */
    def isEmpty: Boolean

    /**
      * Gives constant time access to the first element of this `List`.
      */
    def head: A

    /**
      * A list consisting of the remaining elements of this list after the
      * first one.
      */
    def tail: List[A]

    def baseString: String

    protected def tailDefined: Boolean

    @inline
    final def ++[B >: A](other: => List[B]): List[B] = this match {
        case head :: tail => head :: (tail ++ other)
        case Nil => other
    }

    @inline
    final def !::[B >: A](b: B): List[B] =
        new Cons(b, this, true)

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

    @inline
    def apply[A](args: A*): List[A] = {
        var list: List[A] = Nil
        val it = args.reverseIterator

        while (it.hasNext) {
            list = it.next() !:: list
        }

        list
    }


    @inline
    implicit def listBuilder[T](f: => List[T]): ConsWrapper[T] =
        new ConsWrapper(f)
}

final class Cons[+A](override val head: A,
                     tl: => List[A],
                     defined: Boolean = false) extends List[A] {

    override def isEmpty = false

    @volatile private[this] var tlVal: List[A] = _
    @volatile private[this] var tlGen = tl _

    {
        if (defined) {
            tlVal = tlGen()
            tlGen = null
        }
    }

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
    def ::[A](a: A): List[A] = new Cons[A](a, this, true)

    override def equals(obj: scala.Any): Boolean =
        obj.asInstanceOf[AnyRef] eq this

    override def baseString: String = ""
}

object :: {
    def unapply[A](arg: Cons[A]): Option[(A, List[A])] =
        Some((arg.head, arg.tail))
}