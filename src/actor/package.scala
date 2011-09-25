package object actor {
  type ActorFun[M] = M ⇒ Action[M]
  type StatelessActorFun[M] = M ⇒ StatelessAction

  implicit def sendTraversableToSends(t: Traversable[Send[_]]): Sends = Sends(t)
  implicit def funToContinue[T](f: ActorFun[T]) = Continue(f)

  import actor.impls._
  import execution.ExecutionStrategy

  def spawnPure[M](fun: ActorFun[M])(implicit s: ExecutionStrategy): Actor[M] = PureActor(fun)(s)
  def spawnStateless[M](fun: StatelessActorFun[M])(implicit s: ExecutionStrategy): Actor[M] = StatelessActor(fun)(s)
}