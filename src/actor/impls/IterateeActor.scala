package actor
package impls

import execution.ExecutionStrategy
import io.iteratee._
import IterateeFun._

object IterateeActor {

  type ActionIteratee[I] = Iteratee[I, StatelessAction]

  def sendTo[M](to: Actor[M]): Iteratee[M, StatelessAction] = mapping(Send(to, _))

  def apply[I, O](it: ActionIteratee[I])(implicit s: ExecutionStrategy): Actor[I] = {
    def f(it: ActionIteratee[I])(m: I): Action[I] = {
      it(Data(m)) match {
        case nit @ Result(r) ⇒ r continue (f(nit))
        case nit             ⇒ Continue(f(nit))
      }
    }
    spawnPure[I](f(it))(s)
  }

  def fixed[I, O](it: Iteratee[I, O])(forward: Actor[O])(implicit s: ExecutionStrategy): Actor[I] = {
    def f(it: Iteratee[I, O])(m: I): Action[I] = {
      it(Data(m)) match {
        case nit @ Result(r) ⇒ (forward ! r) continue (f(nit))
        case nit             ⇒ Continue(f(nit))
      }

    }
    spawnPure[I](f(it))(s)
  }

}