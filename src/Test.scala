import actor._
import execution._

object Test extends impls.MainActor {

  object CounterActor {
    def spawn(recv: Actor[Int]) = {
      spawnPure(new Counter(recv).body(0))(Pooled)
    }

    private class Counter(recv: Actor[Int]) {
      def body(soFar: Int)(msg: Any): Action[Any] = {
        val nc = soFar + 1
        Send(recv, nc) continue body(nc)
      }
    }
  }

  object ConsoleActors extends ActorImplementor {
    val SysoutActor: Actor[String] = new SysoutActor
    private class SysoutActor extends ActorImpl[String] {
      override def processMessage(m: String) = {
        println("> " + m)
        Noop
      }
    }

    val SysinActor: Actor[Actor[String]] = new SysinActor
    private class SysinActor extends ActorImpl[Actor[String]] {
      private var subscribers: List[Actor[String]] = Nil

      start
      private def start = Thread {
        while (true) {
          val l = readLine
          synchronized {
            subscribers.foreach(to => handleAction(Send(to, l)))
          }
        }
      }

      override def processMessage(m: Actor[String]) = synchronized {
        subscribers = m :: subscribers
      }
    }
  }

  import ConsoleActors._
  
  def body(args: Array[String]) = {
    val out: Actor[Int] = SysoutActor.comap("Count is " + _.toString)
    val counter = CounterActor.spawn(out)
    val in = SysinActor
    Send(in, counter)
  }
}