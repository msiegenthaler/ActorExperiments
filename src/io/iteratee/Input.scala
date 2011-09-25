package io
package iteratee

sealed trait Input[+E] {
  def isEOF: Boolean
  def isData: Boolean

  def map[A](f: E ⇒ A): Input[A]
}

case class Data[+E](e: E) extends Input[E] {
  override def isEOF = false
  override def isData = false
  override def map[A](f: E ⇒ A) = Data(f(e))
  override def toString = "Data(" + e.toString + ")"
}

object Empty extends Input[Nothing] {
  override def isEOF = false
  override def isData = false
  override def map[A](f: Nothing ⇒ A) = Empty
  override def toString = "Empty"
}

object EOF extends Input[Nothing] {
  override def isEOF = true
  override def isData = false
  override def map[A](f: Nothing ⇒ A) = EOF
  override def toString = "EOF"
}
