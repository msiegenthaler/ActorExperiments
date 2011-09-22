package io

package object iteratee {
  def cont[I, O](f: Input[I] => Iteratee[I, O]) = new Iteratee[I, O] {
    override def apply(in: Input[I]) = f(in)
    override def isDone = false
    override def hasResult = false
  }
  def cont[I, O](f: Input[I] => Iteratee[I, O], o: O) = new Iteratee[I, O] with Result[O] {
    override def apply(in: Input[I]) = f(in)
    override val out = o
    override def isDone = false
    override def hasResult = true
  }
  val done: Iteratee[Any, Nothing] with Done = new Iteratee[Any, Nothing] with Done {
    override def apply(in: Input[Any]) = done
    override def isDone = true
    override def hasResult = false
  }
  def done[O](o: O): Iteratee[Any, O] with Result[O] with Done = new Iteratee[Any, O] with Result[O] with Done {
    override def apply(in: Input[Any]) = done
    override val out = o
    override def isDone = true
    override def hasResult = true
  }

  def cont[I, O](data: I => Iteratee[I, O], empty: => Iteratee[I, O], eof: => Iteratee[I, O]): Iteratee[I, O] =
    cont(ips(data, empty, eof))
  def cont[I, O](data: I => Iteratee[I, O], empty: => Iteratee[I, O], eof: => Iteratee[I, O], o: O): Iteratee[I, O] with Result[O] =
    cont(ips(data, empty, eof), o)
  private def ips[I, O](data: I => Iteratee[I, O], empty: => Iteratee[I, O], eof: => Iteratee[I, O]): Input[I] => Iteratee[I, O] = (in: Input[I]) => in match {
    case Data(d) => data(d)
    case Empty => empty
    case EOF => eof
  }

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

  sealed trait Result[+O] {
    def out: O
  }
  object Result {
    def unapply[I, O](it: Iteratee[I, O]) = it match {
      case r: Result[O] => Some(r.out)
      case _ => None
    }
  }

  sealed trait Done

  sealed trait Iteratee[-I, +O] {
    def apply(in: Input[I]): Iteratee[I, O]
    def isDone: Boolean
    def hasResult: Boolean
    def compose[A](it: Iteratee[O, A]) = iteratee.compose(this, it)
  }
}