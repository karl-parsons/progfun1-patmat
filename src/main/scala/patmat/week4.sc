package week4

object week4 {

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }

  object Zero extends Nat {

    override def isZero = true

    override def predecessor = throw new Error("Zero.predecessor")

    def + (that: Nat): Nat = that

    def - (that: Nat): Nat = if (that.isZero) this else throw new Error("negative")

  }

  class Succ(n: Nat) extends Nat {

    override def isZero = false

    override def predecessor = n

    def + (that: Nat): Nat = new Succ(n + that)

    def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  }

}