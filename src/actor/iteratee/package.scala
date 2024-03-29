package actor

import io.iteratee._
import Iteratee._
import IterateeFun._
import execution._

package object iteratee {
  type ActionIteratee[I] = Iteratee[I, StatelessAction]

  def sendTo[M](to: Actor[M]): Iteratee[M, StatelessAction] = mapping(Send(to, _))

  def spawnIteratee[I, O](it: ActionIteratee[I])(implicit s: ExecutionStrategy): Actor[I] = {
    def f(it: ActionIteratee[I])(m: I): Action[I] = {
      val next = it match {
        case Cont(it)      ⇒ f(it(Data(m))) _
        case CallAgain(it) ⇒ f(it(Empty)) _
        case Done(it)      ⇒ terminated _
      }
      it.outOption.getOrElse(Noop) continue next
    }
    def terminated(I: I) = Noop
    spawnPure[I](f(it))(s)
  }

  def fixed[I, O](it: Iteratee[I, O])(forward: Actor[O])(implicit s: ExecutionStrategy): Actor[I] =
    spawnIteratee(it compose sendTo(forward))
}