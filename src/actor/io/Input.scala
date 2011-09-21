package actor
package io

sealed trait Input[+E] {
  def isEOF: Boolean
  def isData: Boolean
}

case class Data[+E](e: E) extends Input[E] {
  override def isEOF = false
  override def isData = false
}

object Empty extends Input[Nothing] {
  override def isEOF = false
  override def isData = false
}

object EOF extends Input[Nothing]  {
  override def isEOF = true
  override def isData = false
}
