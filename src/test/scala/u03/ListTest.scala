package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:
  import List.*
  /*
    - Type List[A]
    - Constructors:
        Cons(head: A, tail: List[A])
        Nil()
    - Operations:
        sum: List[Int] -> Int
        get[E]: List[E] x Int -> Option[E]
        add[E]: List[E] x Int x E -> Option[List[E]]
        drop[E]: List[E] x Int -> Option[List[E]]

    - Axioms:
        sum(Nil()) = 0
        sum(Cons(h,t)) = h + sum(t)
        get(Nil,i) = None()
        get(Cons(h,t),0) = Some(h) // lista non vuota, almeno h presente
        get(Cons(h,t),i) = et(t, i-1)
        drop(Nil,i) = None()
        drop(Cons(h,t), i) = List
  */

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(None(), drop(l, 5)
    assertEquals(Some(Cons(30, Nil())), drop(l, 2))
    assertEquals(Some(Cons(10, Cons(30, Nil())), drop(l, 2)))


