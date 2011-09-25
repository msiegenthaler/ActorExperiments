package io
package iteratee

sealed trait Iteratee[-I, +O] {
  //  def apply(in: Input[I]): Iteratee[I, O]
  def isDone: Boolean
  def isContinue: Boolean
  def isCallAgain: Boolean

  def hasResult: Boolean
  def outOption: Option[O]

  def compose[A](it: Iteratee[O, A]) = IterateeFun.compose(this, it)
  def |>[A](it: Iteratee[O, A]) = compose(it)
}
object Iteratee {
  def cont[I, O](f: Input[I] ⇒ Iteratee[I, O]) = ContWithoutResult(f)
  def cont[I, O](f: Input[I] ⇒ Iteratee[I, O], o: O) = ContWithResult(f, o)
  def contOption[I, O](f: Input[I] ⇒ Iteratee[I, O], o: Option[O]) = o match {
    case Some(o) ⇒ cont(f, o)
    case None    ⇒ cont(f)
  }
  def callAgain[I, O](f: NoDataInput ⇒ Iteratee[I, O]) = CallAgainWithoutResult(f)
  def callAgain[I, O](f: NoDataInput ⇒ Iteratee[I, O], o: O) = CallAgainWithResult(f, o)
  def callAgainOption[I, O](f: NoDataInput ⇒ Iteratee[I, O], o: Option[O]) = o match {
    case Some(o) ⇒ callAgain(f, o)
    case None    ⇒ callAgain(f)
  }
  def done = DoneWithoutResult
  def done[O](o: O) = DoneWithResult(o)
  def doneOption[O](o: Option[O]) = o match {
    case Some(o) ⇒ done(o)
    case None    ⇒ done
  }

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
  def apply(in: Input[I]): Iteratee[I, O]
  override def isDone = false
  override def isContinue = true
  override def isCallAgain = false
}
object Cont {
  def unapply[I, O](it: Iteratee[I, O]) = it match {
    case c: Cont[I, O] ⇒ Some(c)
    case _             ⇒ None
  }
}
case class ContWithResult[I, O](f: Input[I] ⇒ Iteratee[I, O], out: O) extends Cont[I, O] with Result[O] {
  def apply(in: Input[I]): Iteratee[I, O] = f(in)
}
case class ContWithoutResult[I, O](f: Input[I] ⇒ Iteratee[I, O]) extends Cont[I, O] with NoResult[O] {
  def apply(in: Input[I]): Iteratee[I, O] = f(in)
}

sealed trait CallAgain[I, O] extends Iteratee[I, O] {
  def apply(in: NoDataInput): Iteratee[I, O]
  override def isDone = false
  override def isContinue = false
  override def isCallAgain = true
}
object CallAgain {
  def unapply[I, O](it: Iteratee[I, O]) = it match {
    case c: CallAgain[I, O] ⇒ Some(c)
    case _                  ⇒ None
  }
}
case class CallAgainWithResult[I, O](f: NoDataInput ⇒ Iteratee[I, O], out: O) extends CallAgain[I, O] with Result[O] {
  def apply(in: NoDataInput): Iteratee[I, O] = f(in)
}
case class CallAgainWithoutResult[I, O](f: NoDataInput ⇒ Iteratee[I, O]) extends CallAgain[I, O] with NoResult[O] {
  def apply(in: NoDataInput): Iteratee[I, O] = f(in)
}

sealed trait Done[O] extends Iteratee[Any, O] {
  override def isDone = true
  override def isContinue = false
  override def isCallAgain = false
}
object Done {
  def unapply[I, O](it: Iteratee[I, O]) = it match {
    case d: Done[O] ⇒ Some(d)
    case _          ⇒ None
  }
}
case class DoneWithResult[O](out: O) extends Done[O] with Result[O]
case object DoneWithoutResult extends Done[Nothing] with NoResult[Nothing]
