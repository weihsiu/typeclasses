package typeclasses

import simulacrum._

object Foldables2 extends App {
  import typeclasses.Monoids.Monoid

  @typeclass trait Foldable[F[_]] {
    def foldMap[A, M : Monoid](fa: F[A])(f: A => M): M =
      foldRight(fa, Monoid[M].mempty)((x, a) => Monoid[M].mappend(f(x), a))
    def foldRight[A, B](fa: F[A], z: B)(f: (A, B) => B): B
  }

  object Foldable {
    implicit val ListFoldable = new Foldable[List] {
      def foldRight[A, B](fa: List[A], z: B)(f: (A, B) => B) = fa.foldRight(z)(f)
    }
    implicit def EitherFoldable[A] = new Foldable[Either[A, ?]] {
      def foldRight[B, C](fa: Either[A, B], z: C)(f: (B, C) => C) = fa.fold(Function.const(z), f(_, z))
    }
  }

  def foldRight[F[_] : Foldable, A, B](fa: F[A], z: B)(f: (A, B) => B): B = implicitly[Foldable[F]].foldRight(fa, z)(f)

  import Foldable.ops._

  assert(foldRight(List(1, 2, 3), 0)(_ + _) == 6)
  assert(foldRight(List(2, 3, 4), 1)(_ * _) == 24)

  implicit class ToEither[A](x: A) {
    def left[B]: Either[A, B] = Left(x)
    def right[B]: Either[B, A] = Right(x)
  }

  assert(Foldable[Either[Exception, ?]].foldMap("success".right)(_.length) == 7)
  assert(Foldable[Either[Exception, ?]].foldMap((new Exception).left[String])(_.length) == 0)
}
