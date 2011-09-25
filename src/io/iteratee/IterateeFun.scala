package io
package iteratee

import Iteratee._

object IterateeFun {
  def unit[A]: Iteratee[A, A] = mapping(a ⇒ a)

  def mapping[A, B](f: A ⇒ B): Iteratee[A, B] = {
    def handle(in: Input[A]): Iteratee[A, B] = in match {
      case Data(d) ⇒ cont(handle, f(d))
      case Empty   ⇒ cont(handle)
      case EOF     ⇒ done
    }
    cont(handle)
  }

  def take[A](n: Long): Iteratee[A, A] = {
    def handle(n: Long)(in: Input[A]): Iteratee[A, A] = in match {
      case Data(d) if n > 1  ⇒ cont(handle(n - 1), d)
      case Data(d) if n == 1 ⇒ done(d)
      case Data(d)           ⇒ done
      case Empty             ⇒ cont(handle(n))
      case EOF               ⇒ done
    }
    cont(handle(n))
  }

  def drop[A](n: Long): Iteratee[A, A] = {
    def handle(n: Long)(in: Input[A]): Iteratee[A, A] = in match {
      case Data(d) if n > 0 ⇒ cont(handle(n - 1))
      case Data(d)          ⇒ cont(handle(0), d)
      case Empty            ⇒ cont(handle(n))
      case EOF              ⇒ done
    }
    cont(handle(n))
  }

  def count[A]: Iteratee[A, Long] = {
    def handle(n: Int)(in: Input[A]): Iteratee[A, Long] = in match {
      case Data(_) ⇒ cont(handle(n + 1), n)
      case Empty   ⇒ cont(handle(n))
      case EOF     ⇒ done
    }
    cont(handle(0))
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