package io

package object iteratee {
  def cont[I, O](f: Input[I] => Iteratee[I, O]): ContIteratee[I, O] = new ContIteratee[I, O] {
    override def apply(in: Input[I]) = f(in)
  }
  def cont[I, O](f: Input[I] => Iteratee[I, O], o: O) = new ContWithResultIteratee[I, O] {
    override def apply(in: Input[I]) = f(in)
    override val out = o
  }
  val done: DoneIteratee = new DoneIteratee {
    override def apply(in: Input[Any]) = done
  }
  def done[O](o: O): DoneWithResultIteratee[O] = new DoneWithResultIteratee[O] {
    override def apply(in: Input[Any]) = done
    override val out = o
  }

  def cont[I, O](data: I => Iteratee[I, O], empty: => Iteratee[I, O], eof: => Iteratee[I, O]): ContIteratee[I, O] =
    cont(ips(data, empty, eof))
  def cont[I, O](data: I => Iteratee[I, O], empty: => Iteratee[I, O], eof: => Iteratee[I, O], o: O): ContIteratee[I, O] =
    cont(ips(data, empty, eof))
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
    def map[A](f: O => A): Iteratee[I, A]
    def mapIn[A](f: A => I): Iteratee[A, O]
    def compose[A](it: Iteratee[O, A]) = iteratee.compose(this, it)
  }

  //impls
  sealed trait ContIteratee[-I, +O] extends Iteratee[I, O] {
    override def isDone = false
    override def hasResult = false
    override def map[A](f: O => A): ContIteratee[I, A] = {
      val me = this
      new ContIteratee[I, A] {
        override def apply(in: Input[I]) = me(in).map(f)
      }
    }
    override def mapIn[A](f: A => I): ContIteratee[A, O] = {
      val me = this
      new ContIteratee[A, O] {
        override def apply(in: Input[A]) = me(in.map(f)).mapIn(f)
      }
    }
  }
  sealed trait ContWithResultIteratee[-I, +O] extends Iteratee[I, O] with Result[O] {
    override def isDone = false
    override def hasResult = true
    override def map[A](f: O => A): ContWithResultIteratee[I, A] = {
      val me = this
      new ContWithResultIteratee[I, A] {
        override def apply(in: Input[I]) = me(in).map(f)
        override def out = f(me.out)
      }
    }
    override def mapIn[A](f: A => I): ContWithResultIteratee[A, O] = {
      val me = this
      new ContWithResultIteratee[A, O] {
        override def apply(in: Input[A]) = me(in.map(f)).mapIn(f)
        override def out = me.out
      }
    }
  }
  sealed trait DoneIteratee extends Iteratee[Any, Nothing] with Done {
    override def isDone = true
    override def hasResult = false
    override def map[A](f: Nothing => A): DoneIteratee = this
    override def mapIn[A](f: A => Any): DoneIteratee = this
  }
  sealed trait DoneWithResultIteratee[+O] extends Iteratee[Any, O] with Result[O] with Done {
    override def isDone = true
    override def hasResult = true
    override def map[A](f: O => A): DoneWithResultIteratee[A] = {
      val me = this
      new DoneWithResultIteratee[A] {
        override def apply(in: Input[Any]) = this
        override def out = f(me.out)
      }
    }
    override def mapIn[A](f: A => Any): DoneWithResultIteratee[O] = this
  }

}