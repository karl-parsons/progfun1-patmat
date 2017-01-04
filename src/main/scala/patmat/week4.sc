
object week4 {

  abstract class Nat {

    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat = new Succ(this)

    def +(that: Nat): Nat

    def -(that: Nat): Nat
  }

  object Zero extends Nat {

    override def isZero = true

    override def predecessor = throw new Error("Zero.predecessor")

    def +(that: Nat): Nat = that

    def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negative")

  }

  class Succ(n: Nat) extends Nat {

    override def isZero = false

    override def predecessor: Nat = n

    def +(that: Nat): Nat = new Succ(n + that)

    def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  }

  trait List[T] {

    def isEmpty: Boolean

    def head: T

    def tail: List[T]

  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {

    def isEmpty = false


  }

  class Nil[T] extends List[T] {

    def isEmpty = true

    def head = throw new NoSuchElementException("Nil.head")

    def tail = throw new NoSuchElementException("Nil.tail")

  }

  object List {

    def apply[T](): List[T] = new Nil

    def apply[T](x: T): List[T] = new Cons[T](x, new Nil)

    def apply[T](x: T, y: T) = new Cons[T](x, new Cons[T](y, new Nil))

  }

  val l1 = List()
  val l2 = List(1)
  val l3 = List(2, 3)



}