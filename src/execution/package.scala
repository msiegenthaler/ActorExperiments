
package object execution {
  
  /**
   * Makes an execution easier identifiable by naming the execution thread after it. The name is retained only while f
   * is evaluated.
   * Warning: This is a rather expensive method, only use for longer runtime execution units.
   */
  def namedExecution[A](name: String)(f: ⇒ A): A = {
    val t = java.lang.Thread.currentThread
    val oldName = t.getName
    try {
      t setName name
      f
    } finally {
      t setName oldName
    }
  }
}