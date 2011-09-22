package io
package iteratee

import actor._


object ActorIteratee extends ActorImplementor {
  
  // TODO this is impure.. find a nice way to encapsulate this..
  
  def apply[M](to: Actor[M]) = {
    def handle(in: Input[M]): Iteratee[M, Unit] = {
      in match {
        case Data(d) => handleAction(Send(to, d))
        case Empty => ()
        case EOF => ()
      }
      iteratee.cont(handle)
    }
    iteratee.cont(handle)
  }
}