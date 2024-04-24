import zio._
import scala.io.StdIn

val goShopping      = ZIO.attempt(println("Going to the grocery store"))
val goShoppingLater = goShopping.delay(1.hour)

val readLine = ZIO.attempt(StdIn.readLine())

def printLine(line: String) =
  ZIO.attempt(println(line))

val echo =
  for {
    line <- readLine
    _    <- printLine(line)
  } yield ()

val firstName =
  ZIO.attempt(StdIn.readLine("What is your first name? "))

val lastName =
  ZIO.attempt(StdIn.readLine("What is your last name? "))

val fName =
  firstName.zip(lastName)

val fullName =
  firstName.zipWith(lastName)(_ + " " + _).flatMap(printLine)

val helloWorld =
  ZIO.attempt(print("Hello ")) *> ZIO.attempt(println("world!"))

val printNumbers =
  ZIO.foreach(1 to 10)(n => printLine(n.toString))

val prints =
  List(
    printLine("the"),
    printLine("quick"),
    printLine("brown"),
    printLine("fox"),
  )

val printWords =
  ZIO.collectAll(prints)

final case class Zio[-R, +E, +A](run: R => Either[E, A]) { self =>
  def map[B](f: A => B): Zio[R, E, B] =
    Zio(r => self.run(r).map(f))

  def flatMap[R1 <: R, E1 >: E, B](f: A => Zio[R1, E1, B]): Zio[R1, E1, B] =
    Zio(r => self.run(r).fold(Zio.fail, f).run(r))

  def foldZio[R1 <: R, E1 >: E, B](
    failure: E => Zio[R1, E1, B],
    success: A => Zio[R1, E1, B],
  ): Zio[R1, E1, B] =
    Zio(r => self.run(r).fold(failure, success).run(r))

  def fold[B](failure: E => B, success: A => B): Zio[R, Nothing, B] =
    Zio(r => Right(self.run(r).fold(failure, success)))

  def provide(r: R): Zio[Any, E, A] =
    Zio(_ => self.run(r))
}

object Zio {
  def attempt[A](a: => A): Zio[Any, Throwable, A] =
    Zio { _ =>
      try Right(a)
      catch {
        case t: Throwable => Left(t)
      }
    }

  def fail[E](e: E): Zio[Any, E, Nothing] =
    Zio(_ => Left(e))

  def environment[R]: Zio[R, Nothing, R] =
    Zio(r => Right(r))
}

object Main extends ZIOAppDefault:
  override val bootstrap: ULayer[Unit] = Runtime.removeDefaultLoggers

  override val run = printWords
