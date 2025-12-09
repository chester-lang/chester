package chester.utils.io.impl

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

import chester.utils.io.*

given DefaultRunner: Runner[Future] {
  override inline def doTry[T](IO: Future[T]): Future[Try[T]] = {
    IO.transformWith(result => Future.successful(result))
  }

  override inline def pure[A](x: A): Future[A] = Future.successful(x)

  override inline def flatMap[A, B](fa: Future[A])(
      f: A => Future[B]
  ): Future[B] = fa.flatMap(f)

  override inline def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

  override def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]): Future[B] = {
    f(a).flatMap {
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => Future.successful(b)
    }
  }
}
