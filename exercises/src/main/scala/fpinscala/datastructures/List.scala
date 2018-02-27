package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1: what will be the result of the following match expression?
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (_, 0) => l
      case (Nil, _) => Nil
      case (Cons(h, t), _) => drop(t, (n - 1))
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if !f(h) => l
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case _ => Nil
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a: A, acc: Int) => acc + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // Exercise 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A]) { case (acc, a) => append(List(a), acc) }
  }

  // Exercise 3.13
  def recFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    def g(b: B, a: A): B = f(a, b)
    foldLeft(as, z)(g)
  }

  // Exercise 3.14
  def foldedAppend[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a: A, b: List[A]) => Cons(a, b))

  // Exercise 3.15
  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil:List[A])(append(_, _))

  // Exercise 3.16
  def increase(ns: List[Int]): List[Int] = {
    foldRight(ns, Nil:List[Int])((n, acc) => Cons(n + 1, acc))
  }

  // Exercise 3.17
  def toStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil:List[String])((n, acc) => Cons(n.toString, acc))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((item, acc) => Cons(f(item), acc))

  // Exercise 3.19
  def filter[A](as: List[A])(pred: A => Boolean): List[A] =
    foldRight(as, Nil:List[A]) { case (item, acc) =>
      if (pred(item)) Cons(item, acc) else acc
    }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B]) { case (a, acc) =>
      append(f(a), acc)
    }
  }

  // Exercise 3.21
  def flatMapFilter[A](as: List[A])(pred: A => Boolean): List[A] = {
    flatMap(as)(item => if (pred(item)) List(item) else Nil)
  }

  // Exercise 3.22
  def zip[A](first: List[A], second: List[A]): List[(A, A)] = {
    (first, second) match {
      case (Nil, Nil) =>  Nil
      case (Cons(_, _), Nil) => Nil
      case (Nil, Cons(_, _)) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    }
  }

  def zipWithSum(ns: List[Int], nns: List[Int]): List[Int] = {
    map(zip(ns, nns))({ case (x1, x2) => x1 + x2 })
  }

  // Exercise 3.23
  def zipWith[A, B](as: List[A], aas: List[A])(f: ((A, A)) => B): List[B] = {
    map(zip(as, aas))(f)
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

  def main(args: Array[String]): Unit = {
    println("I say that x is 1 + 2")
    println("The compiler says that x is %d".format(x))
    println("The tail of List(1, 2, 3) is: %s".format(tail(List(1, 2, 3))))
    println("Setting the head of List(1, 2, 3) with 0 yields: %s"
              .format(setHead(List(1, 2, 3), 0)))
    println("Dropping 2 elements from List(1, 2, 3) yields: %s"
              .format(drop(List(1, 2, 3), 2)))
    println("Drop elements from List(2, 4, 6, 1, 3, 5) while even: %s"
              .format(dropWhile(List(2, 4, 6, 1, 3, 5), (x: Int) => x % 2 == 0)))
    println("Init of List(1, 2, 3, 4) yields: %s".format(init(List(1, 2, 3, 4 ))))
    println("Passing Nil and Cons to foldRight: %s"
              .format(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))))
    // The result is Cons(1,Cons(2,Cons(3,Nil))) which looks like Nil is the
    // accumulator starting value for any list, while Cons is the function
    // that creates a List out of any pair of values
    println("The length of List(1, 2, 3) is: %s".format(length(List(1, 2, 3))))
    println("foldLeft List(2, 4, 6) with 0 and +: %s".format(foldLeft(List(2, 4, 6), 0)(_ + _)))
    println("reverse List(1, 2, 3): %s".format(reverse(List(1, 2, 3))))
    println("recFoldRight works: %s".format(recFoldRight(List(1, 4, 9), 0)(_ + _)))
    println("foldedAppend List(1, 2) with List(3, 4): %s"
              .format(foldedAppend(List(1, 2), List(3, 4))))
    println("increase List(1, 2, 3): %s".format(increase(List(1, 2, 3))))
    println("map square on List(1, 2, 3): %s"
              .format(map(List(1, 2, 3))(x => x * x)))
    println("filter odd from List(1, 2, 3): %s"
              .format(filter(List(1, 2, 3))(x => x % 2 == 0)))
    println("flatMap List(1, 2, 3) with a repeat f: %s"
              .format(flatMap(List(1, 2, 3))(i => List(i, i))))
    println("zipWithSum(List(1, 2, 3), List(4, 5, 6)): %s"
              .format(zipWithSum(List(1, 2, 3), List(4, 5, 6))))
  }

}
