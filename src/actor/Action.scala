package actor

sealed trait Action[+A] {
  def +[M](to: Actor[M], msg: M): Action[A] = this + Send(to, msg)
  def +(o: StatelessAction): Action[A]
}

sealed trait StatelessAction extends Action[Nothing] {
  def continue[A](f: ActorFun[A]): StatefulAction[A]
  def +(o: StatelessAction): StatelessAction
  private[actor] def msgs: Traversable[Send[_]]
}

sealed trait StatefulAction[+A] extends Action[A]

object Noop extends StatelessAction {
  override def +(s: StatelessAction) = s
  override def continue[A](f: ActorFun[A]) = Continue(f)
  private[actor] override def msgs = Nil
}

case class Send[M](to: Actor[M], msg: M) extends StatelessAction {
  override def +(s: StatelessAction): Sends = Sends(List(this) ++ s.msgs)
  override def continue[A](f: ActorFun[A]) = Actions(f, List(this))
  private[actor] override def msgs = List(this)
}

case class Sends(msgs: Traversable[Send[_]]) extends StatelessAction {
  override def +(s: StatelessAction): Sends = Sends(msgs ++ s.msgs)
  override def continue[A](f: ActorFun[A]) = Actions(f, msgs)
}

case class Continue[A](f: ActorFun[A]) extends StatefulAction[A] {
  override def +(s: StatelessAction): Actions[A] = Actions(f, s.msgs)
}

case class Actions[A](f: ActorFun[A], msgs: Traversable[Send[_]]) extends StatefulAction[A] {
	override def +(s: StatelessAction): Actions[A] = Actions(f, msgs ++ s.msgs)
}