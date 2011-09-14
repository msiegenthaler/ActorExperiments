package actor
package impls

import annotation._
import execution.ExecutionStrategy

object PureActor extends ActorImplementor {
  def apply[M](fun: ActorFun[M])(implicit s: ExecutionStrategy): Actor[M] = {
    new PureActorImpl[M] {
      override val strategy = s
      override def initialFun = fun
    }
  }

  private trait PureActorImpl[M] extends ActorImpl[M] {
    protected def initialFun: ActorFun[M]
    protected val strategy: ExecutionStrategy

    import java.util.concurrent._

    private val working = new atomic.AtomicBoolean
    private val queue = new ConcurrentLinkedQueue[M]
    @volatile protected var f = initialFun

    override def processMessage(msg: M) = {
      //start worker if not already started
      if (working.compareAndSet(false, true)) {
        strategy {
          handleMessage(msg)
          doWork
          assert(working.compareAndSet(true, false), "Error in PureActor")
        }
      } else {
        queue offer msg
        // if not running now then start it again, since the previous run might have missed us
        if (working.compareAndSet(false, true)) {
          strategy {
            doWork
            assert(working.compareAndSet(true, false), "Error in PureActor")
          }
        }
      }
    }
    
    @tailrec private def doWork {
      val msg = queue.poll
      if (msg != null) {
        // process msg
        handleMessage(msg)
        doWork
      } else () // no more messages
    }
    private def handleMessage(msg: M) {
      handleAction(f(msg)) foreach (f = _)
    }
  }
}
