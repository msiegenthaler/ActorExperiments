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

  def last[A] = tail(1)
  def tail[A](n: Int): Iteratee[A, A] = {
    import collection.immutable.Queue
    def handle(q: Queue[A], ql: Int)(in: Input[A]): Iteratee[A, Traversable[A]] = in match {
      case Data(d) if ql < n ⇒ cont(handle(q enqueue d, ql + 1))
      case Data(d)           ⇒ cont(handle(q.dequeue._2 enqueue d, n))
      case Empty             ⇒ cont(handle(q, ql))
      case EOF               ⇒ done(q)
    }
    cont(handle(Queue(), 0)).traverse
  }

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
    def handleCallAgainA(a: CallAgain[I, A], b: Cont[A, O])(in: NoDataInput): Iteratee[I, O] = a(Empty) match {
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
    def emptyA(in: NoDataInput): Iteratee[I, A] = in match {
      case EOF   ⇒ callAgain(emptyA)
      case Empty ⇒ callAgain(emptyA)
    }

    val out = if (preserveOut) b.outOption else None
    b match {
      case Cont(b) ⇒ a match {
        case Cont(a)      ⇒ contOption(handleCont(a, b) _, out)
        case CallAgain(a) ⇒ callAgainOption(handleCallAgainA(a, b) _, out)
        case Done(a)      ⇒ doneOption[O](out)
      }
      case CallAgain(b) ⇒
        //special mode a that just passes through empty/eof until b is ready again
        handleCallAgainB(a, b)
      case Done(b) ⇒
        doneOption[O](out)
    }
  }

  /** Outputs each elements of the inputed Traversable as an own element. */
  def traverse[I, O](it: Iteratee[I, Traversable[O]]): Iteratee[I, O] = {
    def trav(next: Iteratee[I, Traversable[O]], t: Traversable[O]): Iteratee[I, O] = {
      if (t.isEmpty) traverse(next)
      else callAgain(_ ⇒ trav(next, t.tail), t.head)
    }
    def handleCont(it: Cont[I, Traversable[O]])(in: Input[I]): Iteratee[I, O] = it(in) match {
      case nit @ Result(r) ⇒ trav(nit, r)
      case nit             ⇒ traverse(nit)
    }
    def handleCallAgain(it: CallAgain[I, Traversable[O]])(in: NoDataInput): Iteratee[I, O] = it(in) match {
      case nit @ Result(r) ⇒ trav(nit, r)
      case nit             ⇒ traverse(nit)
    }
    it match {
      case Cont(it)      ⇒ cont(handleCont(it))
      case CallAgain(it) ⇒ callAgain(handleCallAgain(it))
      case Done(d) ⇒ d.outOption match {
        case Some(out) ⇒ trav(done, out)
        case None      ⇒ done
      }
    }
  }
  trait TraversableIteratee[I, O] {
    def traverse(): Iteratee[I, O]
  }
  implicit def iterateeToTraversable[I, O](it: Iteratee[I, Traversable[O]]): TraversableIteratee[I, O] = new TraversableIteratee[I, O] {
    override def traverse() = IterateeFun.traverse(it)
  }
}