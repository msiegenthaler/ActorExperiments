package actor

sealed trait Action[+A] {
  def +[M](to: Actor[M], msg: M): Action[A] = this + Send(to, msg)
  def +[M](s: Send[M]): Action[A]
}

sealed trait StatelessAction extends Action[Nothing] {
  def continue[A](f: ActorFun[A]): StatefulAction[A]
}

sealed trait StatefulAction[+A] extends Action[A]

object Noop extends StatelessAction {
  override def +[M](s: Send[M]) = Sends(List(s))
  override def continue[A](f: ActorFun[A]) = Continue(f)
}

case class Send[M](to: Actor[M], msg: M) extends StatelessAction {
  override def +[M](s: Send[M]) = Sends(List(this, s))
  override def continue[A](f: ActorFun[A]) = Actions(f, List(this))
}

case class Sends(msgs: Traversable[Send[_]]) extends StatelessAction {
  override def +[M](s: Send[M]) = Sends(msgs ++ List(s))
  override def continue[A](f: ActorFun[A]) = Actions(f, msgs)
}

case class Continue[A](f: ActorFun[A]) extends StatefulAction[A] {
  override def +[M](s: Send[M]) = Actions(f, List(s))
}

case class Actions[A](f: ActorFun[A], msgs: Traversable[Send[_]]) extends StatefulAction[A] {
  override def +[M](s: Send[M]) = Actions(f, msgs ++ List(s))
}