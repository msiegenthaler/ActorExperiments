package io
package iteratee

import annotation._
import Iteratee._

object IterateeFun {
  /** Does nothing. Usually only useful at the beginning of an iteratee chain. */
  def unit[A]: Iteratee[A, A] = mapping(a ⇒ a)

  /** Applies f to each element received and then pass it on */
  def mapping[A, B](f: A ⇒ B): Iteratee[A, B] = {
    def handle(in: Input[A]): Iteratee[A, B] = in match {
      case Data(d) ⇒ cont(handle, f(d))
      case Empty   ⇒ cont(handle)
      case EOF     ⇒ done
    }
    cont(handle)
  }

  /** Outputs each elements of the inputed Traversable as an own element. */
  def traverse[A]: Iteratee[Traversable[A], A] = {
    def trav(next: Iteratee[Traversable[A], A], t: Traversable[A]): Iteratee[Traversable[A], A] = {
      if (t.isEmpty) next
      else callAgain(_ ⇒ trav(next, t.tail), t.head)
    }
    def handle(in: Input[Traversable[A]]): Iteratee[Traversable[A], A] = in match {
      case Data(t) ⇒ trav(cont(handle), t)
      case Empty   ⇒ cont(handle)
      case EOF     ⇒ done
    }
    cont(handle)
  }

  /** Only forward to first n elements. */
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

  /** Don't forward the first n elements. */
  def drop[A](n: Long): Iteratee[A, A] = {
    def handle(n: Long)(in: Input[A]): Iteratee[A, A] = in match {
      case Data(d) if n > 0 ⇒ cont(handle(n - 1))
      case Data(d)          ⇒ cont(handle(0), d)
      case Empty            ⇒ cont(handle(n))
      case EOF              ⇒ done
    }
    cont(handle(n))
  }

  /** Forwards only the last element. */
  def last[A] = tail[A](1)
  /** Forwards only the last n elements. Warning: Up to n elements are kept in memory */
  def tail[A](n: Int): Iteratee[A, A] = {
    import collection.immutable.Queue
    def handle(q: Queue[A], ql: Int)(in: Input[A]): Iteratee[A, Traversable[A]] = in match {
      case Data(d) if ql < n ⇒ cont(handle(q enqueue d, ql + 1))
      case Data(d)           ⇒ cont(handle(q.dequeue._2 enqueue d, n))
      case Empty             ⇒ cont(handle(q, ql))
      case EOF               ⇒ done(q)
    }
    cont(handle(Queue(), 0)) |> traverse
  }

  /** Forwards the number of elements received so far on each element received. Often used as count |> last */
  def count[A]: Iteratee[A, Long] = {
    def handle(n: Int)(in: Input[A]): Iteratee[A, Long] = in match {
      case Data(_) ⇒ cont(handle(n + 1), n)
      case Empty   ⇒ cont(handle(n))
      case EOF     ⇒ done
    }
    cont(handle(0))
  }

  /** Inputs the result of Iteratee a into Iteratee b thereby combining their processing */
  def compose[I, O, A](a: Iteratee[I, A], b: Iteratee[A, O], preserveOut: Boolean = true): Iteratee[I, O] = {
    def handleCont(a: Cont[I, A], b: Cont[A, O])(in: Input[I]): Iteratee[I, O] = a(in) match {
      case na @ Result(data) ⇒ compose(na, b(Data(data)), true)
      case na                ⇒ compose(na, b, false)
    }
    def handleCallAgainA(a: CallAgain[I, A], b: Cont[A, O])(in: NoDataInput): Iteratee[I, O] = a(in) match {
      case na @ Result(data) ⇒ compose(na, b(Data(data)), true)
      case na                ⇒ compose(na, b, false)
    }
    def handleCallAgainB[I, O, A](a: Iteratee[I, A], b: CallAgain[A, O]): Iteratee[I, O] = {
      def handle(b: CallAgain[A, O])(in: NoDataInput): Iteratee[I, O] = b(in) match {
        case CallAgain(nb) ⇒ callAgainOption(handle(nb), nb.outOption)
        case Cont(nb)      ⇒ compose(a, nb, true)
        case Done(nb)      ⇒ compose(a, nb, true)
      }
      callAgainOption(handle(b), b.outOption)
    }
    def handleDoneA(b: Cont[A, O])(in: NoDataInput): Iteratee[I, O] = {
      assert(in.isEOF, "invalid input, expected EOF but received " + in)
      compose(done, b(EOF))
    }
    def emptyA(in: NoDataInput): Iteratee[I, A] = in match {
      case EOF   ⇒ callAgain(emptyA)
      case Empty ⇒ callAgain(emptyA)
    }
    val out = if (preserveOut) b.outOption else None
    b match {
      case Cont(b) ⇒ a match {
        case Cont(a)      ⇒ contOption(handleCont(a, b) _, out)
        case CallAgain(a) ⇒ callAgainOption(handleCallAgainA(a, b) _, out)
        case Done(a)      ⇒ callAgainOption(handleDoneA(b) _, out)
      }
      case CallAgain(b) ⇒
        //special mode a that just passes through empty/eof until b is ready again
        handleCallAgainB(a, b)
      case Done(b) ⇒
        doneOption[O](out)
    }
  }
}