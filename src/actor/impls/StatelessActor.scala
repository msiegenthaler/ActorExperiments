package actor
package impls

import execution.ExecutionStrategy

object StatelessActor extends ActorImplementor {
  def apply[M](fun: StatelessActorFun[M])(implicit s: ExecutionStrategy): Actor[M] = {
    new StatelessActorImpl[M] {
      override val strategy = s
      override val f = fun
    }
  }

  private trait StatelessActorImpl[M] extends ActorImpl[M] {
    protected val f: ActorFun[M]
    protected val strategy: ExecutionStrategy
    override def processMessage(msg: M) = strategy(handleAction(f(msg)))
  }
}