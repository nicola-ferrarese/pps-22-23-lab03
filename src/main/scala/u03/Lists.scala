package u03

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n > 0 => drop(t, n-1)
      case _ => l

    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
      case Cons(h, t) => Cons(h, append(t, l2))
      case Nil() => l2

    def flatMap[A, B](l: List[A])(function: A => List[B]): List[B] = l match
      case Cons(h, t) => append(function(h), flatMap(t)(function))
      case Nil() => Nil()

    def mapWithFlatMap[A, B](l: List[A])(function: A => B): List[B] =
      flatMap(l)(x => Cons(function(x), Nil()))

    def filterWithFlatMap[A](l: List[A])(pred: A => Boolean): List[A] = l match
      /* Is it allowed?
      case Cons(h, t) => flatMap(l)(x => if pred(x)
                                          then Cons(x, Nil())
                                          else Nil())
      */
      case Cons(h, t) => flatMap(l)(x => filter(Cons(x, Nil()))(pred))
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h,t) =>                 // List not empty
        val maxTail = max(t)            // call recursively on tail
        maxTail match
          case Some(tmp_max) =>         // tail is not empty
            if (h > tmp_max) Some(h)    // compare head & tmp_tail
            else Some(tmp_max)
          case None => Some(h)          // tail is empty, implies head is max
      case Nil() => None                // Empty List

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
