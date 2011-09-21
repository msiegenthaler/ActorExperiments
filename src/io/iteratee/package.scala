package io

package object iteratee {
  def cont[I,O](f: Input[I] => Iteratee[I,O]) = new Iteratee[I,O] {
  	override def apply(in: Input[I]) = f(in)
  }
  def cont[I,O](f: Input[I] => Iteratee[I,O], o: O) = new Iteratee[I,O] with Result[O] {
  	override def apply(in: Input[I]) = f(in)
  	override val out = o
  }
  val done: Iteratee[Any,Nothing] = new Iteratee[Any,Nothing] {
    override def apply(in: Input[Any]) = done 
  }
  def done[O](o: O): Iteratee[Any,O] with Result[O] with Done = new Iteratee[Any,O] with Result[O] with Done {
    override def apply(in: Input[Any]) = done 
    override val out = o
  }
  
  
  implicit def fun2cont[I,O](f: Input[I] => Iteratee[I,O]): Iteratee[I,O] = new Iteratee[I,O] {
  	override def apply(in: Input[I]) = f(in)
  }
}