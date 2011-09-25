package actor

import execution.ExecutionStrategy

sealed trait Actor[-M] {
  //accessed only through ActionImplementor#handleAction
  private[actor] def processMessage(msg: M)

  def ![A <: M](msg: A) = Send(this, msg)
  def comap[T](f: T ⇒ M): Actor[T] = {
    val a = this
    new Actor[T] {
      override def processMessage(msg: T) = a.processMessage(f(msg))
    }
  }
}

trait ActorImplementor {
  protected trait ActorImpl[-M] extends Actor[M]

  protected def handleAction[T](a: Action[T]) = a match {
    case Noop ⇒ None
    case Send(to, m) ⇒
      to processMessage m
      None
    case Sends(msgs) ⇒
      msgs foreach {
        _ match {
          case Send(to, m) ⇒ to processMessage m
        }
      }
      None
    case Continue(f) ⇒ Some(f)
    case Actions(f, msgs) ⇒
      msgs foreach {
        _ match {
          case Send(to, m) ⇒ to processMessage m
        }
      }
      Some(f)
  }
}

