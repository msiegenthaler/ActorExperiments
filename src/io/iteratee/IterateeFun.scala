package io
package iteratee

import Iteratee._

object IterateeFun {
  def mapping[A, B](f: A => B): Iteratee[A, B] = {
    def handle(in: Input[A]): Iteratee[A, B] = in match {
      case Data(d) => cont(handle, f(d))
      case Empty => cont(handle)
      case EOF => done
    }
    cont(handle)
  }

  def compose[I, A, O](a: Iteratee[I, A], b: Iteratee[A, O]): Iteratee[I, O] = {
    def handle(in: Input[I]): Iteratee[I, O] = {
      val ait = a(in)
      ait match {
        case Result(d) =>
          val bit = b(Data(d))
          if (ait.isDone) compose(done, bit)
          else compose(ait, bit)
        case _: Done => compose(done, b(EOF))
        case _ =>
          //optimize away pointless 'Empty' messages
          val b2 = cont(b.apply)
          compose(ait, b2)
      }
    }
    if (b.isDone) b match {
      case Result(r) => done(r)
      case _ => done
    }
    else b match {
      case Result(r) => cont(handle, r)
      case _ => cont(handle)
    }
  }
}