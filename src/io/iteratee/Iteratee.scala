package io
package iteratee

sealed trait Result[+O] {
  def out: O
  def outOption = Some(out)
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
  def outOption: Option[O]

  def compose[A](it: Iteratee[O, A]) = IterateeFun.compose(this, it)
}

object Iteratee {
  def cont[I, O](f: Input[I] => Iteratee[I, O]): Iteratee[I, O] = new Iteratee[I, O] with NoResult[O] {
    override def apply(in: Input[I]) = f(in)
    override def isDone = false
  }
  def cont[I, O](f: Input[I] => Iteratee[I, O], o: O): Iteratee[I, O] with Result[O] = new Iteratee[I, O] with Result[O] {
    override def apply(in: Input[I]) = f(in)
    override val out = o
    override def isDone = false
    override def hasResult = true
  }
  val done: Iteratee[Any, Nothing] with Done = new Iteratee[Any, Nothing] with Done with NoResult[Nothing] {
    override def apply(in: Input[Any]) = done
    override def isDone = true
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

  private trait NoResult[O] {
    def hasResult = false
    def outOption = None
  }
}