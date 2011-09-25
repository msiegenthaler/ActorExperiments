package io
package iteratee

import Iteratee._

object IterateeFun {
  def mapping[A, B](f: A ⇒ B): Iteratee[A, B] = {
    def handle(in: Input[A]): Iteratee[A, B] = in match {
      case Data(d) ⇒ cont(handle, f(d))
      case Empty   ⇒ cont(handle)
      case EOF     ⇒ done
    }
    cont(handle)
  }

  def compose[I, A, O](a: Iteratee[I, A], b: Iteratee[A, O]): Iteratee[I, O] = {
    def handle(in: Input[I]): Iteratee[I, O] = {
      val ait = a(in)
      ait match {
        case DoneWithResult(d)    ⇒ compose(done, b(Data(d)))
        case ContWithResult(_, d) ⇒ compose(ait, b(Data(d)))
        case DoneWithoutResult    ⇒ compose(done, b(EOF))
        case ContWithoutResult(_) ⇒ compose(ait, cont(b.apply)) //optimize away pointless 'Empty' messages
      }
    }
    b match {
      case DoneWithResult(r)    ⇒ done(r)
      case DoneWithoutResult    ⇒ done
      case ContWithResult(_, r) ⇒ cont(handle, r)
      case ContWithoutResult(_) ⇒ cont(handle)
    }
  }
}