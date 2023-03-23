package u03

import u02.Modules.Person

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
// Task 1
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

// Task 2
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h,t) =>                 // List not empty
        val maxTail = max(t)            // call recursively on tail
        maxTail match
          case Some(tmp_max) =>         // tail is not empty
            if (h > tmp_max) Some(h)    // compare head & tmp_tail
            else Some(tmp_max)
          case None => Some(h)          // tail is empty, implies head is max
      case Nil() => None                // Empty List

//Task 3
  import List.*
  import u02.Modules.Person.*

  def getCourses(people: List[Person]): List[String] = people match
    case Cons(Student(_, _), t) => getCourses(t)
    case Cons(Teacher(_, course), t) => Cons(course, getCourses(t))
    case Nil() => Nil()

  def getCoursesWithFlatMap(people: List[Person]): List[String] = flatMap(people) {
    case Student(_, _) => Nil()
    case Teacher(_, course) => Cons(course, Nil())
  }

// Task 4
  def foldLeft[A,B](list: List[A])(default: B)(op: (B, A)=> B): B = list match
    case Cons(h, t) => foldLeft(t)(op(default, h))(op)
    case Nil() => default

  def foldRight[A,B](list: List[A])(default: B)(op: (A, B)=> B): B = list match
    case Cons(h, t) => op(h, foldRight(t)(default)(op))
    case Nil() => default