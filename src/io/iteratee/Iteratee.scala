package io
package iteratee


trait Result[+O] {
  val out: O
}
trait Done

trait Iteratee[-I, +O] {
  def apply(in: Input[I]): Iteratee[I, O]
}
  