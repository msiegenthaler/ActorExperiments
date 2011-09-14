package actor
package impls

import execution._

abstract class MainActor {
  def strategy: ExecutionStrategy = Pooled
  def body(args: Array[String]): Action[Array[String]]

  def main(args: Array[String]) {
    Impl.start(args)
  }

  private object Impl extends ActorImplementor {
    def start(args: Array[String]) = {
      val actor = spawnPure(body)(strategy)
      handleAction(Send(actor, args))
      actor
    }
  }

}