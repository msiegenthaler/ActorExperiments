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

  private def process[I, O](it: Iteratee[I, O], in: Input[I]): Iteratee[I, O] = it match {
    case Cont(it) ⇒ it(in)
    case CallAgain(it) ⇒
      in match {
        case d: NoDataInput ⇒ it(d)
        case _              ⇒ throw new AssertionError("invalid input to CallAgain iteratee (" + in + " -> " + it)
      }
    case Done(_) ⇒ done
  }
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

  //TODO handle a.callAgain
  //    b match {
  //      case ContWithResult(_, r)      ⇒ cont(handleCont, r)
  //      case ContWithoutResult(_)      ⇒ cont(handleCont)
  //      case CallAgainWithResult(_, r) ⇒ callAgain(handleCallAgain, r)
  //      case CallAgainWithoutResult(_) ⇒ callAgain(handleCallAgain)
  //      case DoneWithResult(r)         ⇒ done(r)
  //      case DoneWithoutResult         ⇒ done
  //    }

  null
  //    def doCompose(a: Iteratee[I, A], b: Iteratee[A, O], again: Boolean): Iteratee[I, O] = {
  //      def handle(in: Input[I]): Iteratee[I, O] = {
  //        val ait = a(in)
  //        ait match {
  //          case ContWithResult(_, d) ⇒ doCompose(ait, b(Data(d)), false)
  //          case ContWithoutResult(_) ⇒
  //            if (b.isCallAgain) doCompose(ait, b(Empty), false)
  //            else doCompose(ait, cont(b.apply), false) //optimize away pointless 'Empty' messages
  //          case CallAgainWithResult(_, d) ⇒ doCompose(ait, b(Data(d)), true)
  //          case CallAgainWithoutResult(_) ⇒
  //            if (b.isCallAgain) doCompose(ait, b(Empty), false)
  //            else doCompose(ait, cont(b.apply), true) //optimize away pointless 'Empty' messages
  //          case DoneWithResult(d) ⇒ doCompose(done, b(Data(d)), false)
  //          case DoneWithoutResult ⇒ doCompose(done, b(EOF), false)
  //        }
  //      }
  //      b match {
  //        case ContWithResult(_, r)      ⇒ if (again) callAgain(handle, r) else cont(handle, r)
  //        case ContWithoutResult(_)      ⇒ if (again) callAgain(handle) else cont(handle)
  //        case CallAgainWithResult(_, r) ⇒ callAgain(handle, r)
  //        case CallAgainWithoutResult(_) ⇒ callAgain(handle)
  //        case DoneWithResult(r)         ⇒ done(r)
  //        case DoneWithoutResult         ⇒ done
  //      }
  //    }
  //    doCompose(a, b, false)
  //  }

  //  def traverse[I, O](it: Iteratee[I, Traversable[O]]): Iteratee[I, O] = {
  //    def handle(in: Input[I]): Iteratee[I, O] = {
  //      it(in) match {
  //        case nit @ ContWithResult(_, l) ⇒
  //          process(nit)(l)
  //      }
  //      null
  //    }
  //    def process(next: Iteratee[I, Traversable[O]])(l: Traversable[O]): Iteratee[I, O] = {
  //    	if (l.isEmpty) {
  //    	  traverse(next)
  //    	} else {
  //    	  null
  //    	}
  //    	null
  //    }
  //    
  //    //TODO
  //    cont(handle)
  //  }
  //  trait TraversableIteratee[I, O] {
  //    def traverse(): Iteratee[I, O]
  //  }
  //  implicit def iterateeToTraversable[I, O](it: Iteratee[I, Traversable[O]]): TraversableIteratee[I, O] = new TraversableIteratee[I, O] {
  //    override def traverse() = IterateeFun.traverse(it)
  //  }
}