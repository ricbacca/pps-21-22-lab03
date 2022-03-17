package u03

import u02.AlgebraicDataTypes.Person
import u02.AlgebraicDataTypes.Person.Teacher
import u02.Optionals.Option.*
import u03.Lists.List.max

import scala.annotation.tailrec


object Lists extends App:

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

    def map2[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => flatMap(l)(f => Cons(mapper(f), Nil()))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def filterMap[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) => flatMap(l1)(f => if pred(f) then Cons(f, Nil()) else Nil())
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h,t) if n == 1 => t
      case Cons(h,t) if n > 1 => drop(t, n-1)
      case Nil() | Cons(_, _) => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def max(l: List[Int]): u02.Optionals.Option[Int] = l match
      case Cons(h, t) => max(t) match
        case None() => Some(h)
        case _ => if (orElse(max(t), 0) > h) max(t) else Some(h)
      case Nil() | Cons(_, _) => None()

    @tailrec
    def foldLeft(l: List[Int])(init: Int)(fold: (Int, Int) => Int): Int = l match
      case Cons(h, t) => foldLeft(t)(fold(init, h))(fold)
      case Nil() => init

    @tailrec
    def foldRight(l: List[Int])(init: Int)(fold: (Int, Int) => Int): Int = l match
      case Cons(h, t) => foldRight(t)(fold(-h, init))(fold)
      case Nil() => init

    def getCourses(persons: => List[Person]): List[String] = persons match
      case Cons(h, t) => map(filter(persons)(a => a.isInstanceOf[Teacher]))(f => f.asInstanceOf[Teacher].course)
      case Nil() => Nil()