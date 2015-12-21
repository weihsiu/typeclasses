package typeclasses

import simulacrum._

object Monoids2 extends App {
  @typeclass trait Monoid[A] {
    def mempty: A
    @op("|+|") def mappend(x: A, y: A): A
  }

  case class First[A](getFirst: Option[A])

  object Monoid {
    implicit object SumMonoid extends Monoid[Int] {
      def mempty = 0
      def mappend(x: Int, y: Int) = x + y
    }
    implicit object StringMonoid extends Monoid[String] {
      def mempty = ""
      def mappend(x: String, y: String) = x + y
    }
    implicit def ListMonoid[A] = new Monoid[List[A]] {
      def mempty = List.empty
      def mappend(x: List[A], y: List[A]) = x ::: y
    }
    implicit def OptionMonoid[A : Monoid] = new Monoid[Option[A]] {
      def mempty = None
      def mappend(x: Option[A], y: Option[A]) = (x, y) match {
        case (None, m) => m
        case (m, None) => m
        case (Some(m1), Some(m2)) => Some(Monoid[A].mappend(m1, m2))
      }
    }
    implicit def FirstMonoid[A] = new Monoid[First[A]] {
      def mempty = First(None)
      def mappend(x: First[A], y: First[A]) = (x, y) match {
        case (f@First(Some(_)), _) => f
        case (First(None), m) => m
      }
    }
  }

  import Monoid.ops._

  assert((1 |+| 2) == 3)
  assert(("hello" |+| " " |+| "world") == "hello world")

  implicit object ProductMonoid extends Monoid[Int] {
    def mempty = 1
    def mappend(x: Int, y: Int) = x * y
  }

  assert((2 |+| 3 |+| 4) == 24)
}
