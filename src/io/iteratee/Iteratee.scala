package io
package iteratee

sealed trait Iteratee[-I, +O] {
  def apply(in: Input[I]): Iteratee[I, O]
  def isDone: Boolean

  def hasResult: Boolean
  def outOption: Option[O]

  def compose[A](it: Iteratee[O, A]) = IterateeFun.compose(this, it)
  def |>[A](it: Iteratee[O, A]) = compose(it)
}
object Iteratee {
  def cont[I, O](f: Input[I] ⇒ Iteratee[I, O]) = ContWithoutResult(f)
  def cont[I, O](f: Input[I] ⇒ Iteratee[I, O], o: O) = ContWithResult(f, o)
  def done = DoneWithoutResult
  def done[O](o: O) = DoneWithResult(o)

  def cont[I, O](data: I ⇒ Iteratee[I, O], empty: ⇒ Iteratee[I, O], eof: ⇒ Iteratee[I, O]) = ContWithoutResult(ips(data, empty, eof))
  def cont[I, O](data: I ⇒ Iteratee[I, O], empty: ⇒ Iteratee[I, O], eof: ⇒ Iteratee[I, O], o: O) = ContWithResult(ips(data, empty, eof), o)
  private def ips[I, O](data: I ⇒ Iteratee[I, O], empty: ⇒ Iteratee[I, O], eof: ⇒ Iteratee[I, O]): Input[I] ⇒ Iteratee[I, O] = (in: Input[I]) ⇒ in match {
    case Data(d) ⇒ data(d)
    case Empty   ⇒ empty
    case EOF     ⇒ eof
  }
}

sealed trait Result[+O] {
  def hasResult = true
  def out: O
  def outOption = Some(out)
}
object Result {
  def unapply[I, O](it: Iteratee[I, O]) = it match {
    case r: Result[O] ⇒ Some(r.out)
    case _            ⇒ None
  }
}

sealed trait NoResult[O] {
  def hasResult = false
  def outOption = None
}
object NoResult {
  def unapply[I, O](it: Iteratee[I, O]) = it match {
    case r: NoResult[O] ⇒ Some(())
    case _              ⇒ None
  }
}

sealed trait Cont[I, O] extends Iteratee[I, O] {
  override def isDone = false
}
object Cont {
  def unapply[I, O](it: Iteratee[I, O]) = {
    if (!it.isDone) Some((it, it.outOption))
    else None
  }
}
case class ContWithResult[I, O](f: Input[I] ⇒ Iteratee[I, O], out: O) extends Cont[I, O] with Result[O] {
  override def apply(in: Input[I]): Iteratee[I, O] = f(in)
}
case class ContWithoutResult[I, O](f: Input[I] ⇒ Iteratee[I, O]) extends Cont[I, O] with NoResult[O] {
  override def apply(in: Input[I]): Iteratee[I, O] = f(in)
}

sealed trait Done[O] extends Iteratee[Any, O] {
  override def apply(in: Input[Any]): Iteratee[Any, O] = this
  override def isDone = true
}
object Done {
  def unapply[I, O](it: Iteratee[I, O]) = {
    if (it.isDone) Some(it.outOption)
    else None
  }
}
case class DoneWithResult[O](out: O) extends Done[O] with Result[O]
case object DoneWithoutResult extends Done[Nothing] with NoResult[Nothing]
