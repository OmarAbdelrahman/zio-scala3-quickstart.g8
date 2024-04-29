object S99 extends App:
  object P01:
    def last[A](list: List[A]): A =
      list match
        case head :: Nil => head
        case _ :: tail   => last(tail)
        case _           => throw new NoSuchElementException

  object P02:
    // 2nd last element
    def penultimate[A](list: List[A]): A =
      list match
        case head :: _ :: Nil => head
        case _ :: tail        => penultimate(tail)
        case _                => throw new NoSuchElementException

  object P03:
    def nth[A](n: Int, list: List[A]): A =
      (n, list) match
        case (0, head :: _) => head
        case (n, _ :: tail) => nth(n - 1, tail)
        case (_, Nil)       => throw new NoSuchElementException

  object P04:
    def length[A](list: List[A]): Int =
      list match
        case Nil       => 0
        case _ :: tail => 1 + length(tail)

  import P04._

  println(length(List(1, 2, 3)))
  println(length(List(5, 6, 7, 8)))
  println(length(List(1, 1 )))
  println(length(List(1, 10)))
  println(length(List(1)))
  println(length(List.empty[Int]))
