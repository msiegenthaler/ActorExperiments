package actor
package impls

import execution.ExecutionStrategy
import io.iteratee._

object IterateeActor {
  def fixed[I, O](it: Iteratee[I, O])(forward: Actor[O])(implicit s: ExecutionStrategy): Actor[I] = {
    def f(it: Iteratee[I, O])(m: I): Action[I] = {
      it(Data(m)) match {
        case nit @ Result(r) =>
          (forward ! r) continue (f(nit))
        case nit =>
          Continue(f(nit))
      }

    }
    spawnPure[I](f(it))(s)
  }

  def apply[I, O](it: Iteratee[I, StatelessAction])(implicit s: ExecutionStrategy): Actor[I] = {
    def f(it: Iteratee[I, StatelessAction])(m: I): Action[I] = {
      it(Data(m)) match {
        case nit @ Result(r) =>
          r continue (f(nit))
        case nit =>
          Continue(f(nit))
      }
    }
    spawnPure[I](f(it))(s)
  }
}