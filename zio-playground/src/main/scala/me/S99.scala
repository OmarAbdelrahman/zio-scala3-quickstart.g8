
object S99 extends App:
  object P01:
    def last[A](list: List[A]): A =
      list match
        case head :: Nil => head
        case _ :: tail => last(tail)
        case _ => throw new NoSuchElementException

  object P02:
    // 2nd last element
    def penultimate[A](list: List[A]): A =
      list match
        case head :: _ :: Nil => head
        case _ :: tail => penultimate(tail)
        case _ => throw new NoSuchElementException

  import P02._

  println(penultimate(List(1, 2, 3)))
  println(penultimate(List(5, 6, 7, 8)))
  println(penultimate(List(1, 10)))