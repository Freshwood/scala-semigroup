package net.softler.blog

trait Semigroup[A] {
  def append(a: A, b: A): A
}
object Groups {
  implicit object IntGroup extends Semigroup[Int] {
    override def append(a: Int, b: Int): Int = a + b
  }

  implicit object StringGroup extends Semigroup[String] {
    override def append(a: String, b: String): String = a + b
  }

  implicit object DoubleGroup extends Semigroup[Double] {
    override def append(a: Double, b: Double): Double = a + b
  }

  implicit object CarGroup extends Semigroup[Car] {
    override def append(a: Car, b: Car): Car = Car(a.horsePower + b.horsePower)
  }

  implicit object IntIntGroup extends Semigroup[Int => Int] {
    override def append(a: Int => Int, b: Int => Int): Int => Int =
      input => a(input) + b(input)
  }
}

case class Car(horsePower: Int)

/**
  * Shows the usage of the scala semi group
  * This is only a example
  */
object SemigroupBlog extends App {

  import Groups._

  def calc[A](input: A)(implicit semigroup: Semigroup[A]): A =
    semigroup.append(input, input)

  def reduce[A](input: List[A])(implicit semigroup: Semigroup[A]): A =
    input.reduce(semigroup.append)

  def associativity[A](a1: A, a2: A, a3: A)(implicit semigroup: Semigroup[A]): Boolean = {
    val first = semigroup.append(a1, semigroup.append(a2, a3))
    val second = semigroup.append(semigroup.append(a1, a2), a3)
    println(first + "==" + second)
    first == second
  }

  println(calc(100d))

  println(reduce(List(1, 2, 3, 4, 5)))

  println(calc(Car(150)))

  println(associativity(100, 50, 100))

  println(reduce(List(Car(20), Car(50), Car(111))))

  val fn: Int => Int = _ * 2

  println {
    val result = calc(fn)
    result(10)
  }

  println {
    val result = reduce(List(fn, fn, fn, fn, fn, fn, fn, fn))
    result(1)
  }
}
