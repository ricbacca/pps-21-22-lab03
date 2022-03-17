package u03

import org.junit.Test
import u03.Streams.*
import org.junit.*
import u03.Lists.*
import org.junit.Assert.*
import u03.Streams.Stream.{constant, fibs}

class StreamTest {
  import List.*
  val s: Stream[Int] = Stream.take(Stream.iterate(0)(_ + 1))(10)

  @Test def dropTest(): Unit =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def constantTest(): Unit =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(constant("x"))(5)))

  @Test def fibonacciTest(): Unit =
    assertEquals(Stream.toList(Stream.take(fibs())(8)), Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))))

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testFoldRight(): Unit =
    val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
}
