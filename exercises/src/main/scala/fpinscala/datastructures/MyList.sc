sealed trait MyList[+A]

// `List` data type, parameterized on a type, `A`
case object MyNil extends MyList[Nothing]

// A `List` data constructor representing the empty MyList
/* Another data constructor, representing nonempty MyLists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `MyCons`.
 */
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  // `List` companion object. Contains functions for creating and working with MyLists.
  def sum(ints: MyList[Int]): Int = ints match {
    // A function that uses pattern matching to add up a MyList of integers
    case MyNil => 0 // The sum of the empty MyList is 0.
    case MyCons(x, xs) => x + sum(xs) // The sum of a MyList starting with `x` is `x` plus the sum of the rest of the MyList.
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = // Variadic function syntax
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  val x = MyList(1, 2, 3, 4, 5) match {
    case MyCons(x, MyCons(2, MyCons(4, _))) => x
    case MyNil => 42
    case MyCons(x, MyCons(y, MyCons(3, MyCons(4, _)))) => x + y
    case MyCons(h, t) => h + sum(t)
    case _ => 101
  }
}

def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
  a1 match {
    case MyNil => a2
    case MyCons(h, t) => MyCons(h, append(t, a2))
  }

def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = // Utility functions
  as match {
    case MyNil => z
    case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def sum2(ns: MyList[Int]) =
  foldRight(ns, 0)((x, y) => x + y)

def product2(ns: MyList[Double]) =
  foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

def tail[A](l: MyList[A]): MyList[A] = l match {
  // alles voorzien, anders MatchError
  case MyNil => MyNil // beter afhandelen?
  case MyCons(_, foo) => foo
}

def setHead[A](l: MyList[A], h: A): MyList[A] = l match {
  // vervang de head van de lijst
  case MyNil => MyNil
  case MyCons(_, foo) => MyCons(h, foo)
}

def drop[A](l: MyList[A], n: Int): MyList[A] = {
  // drop het aantal opgegeven elementen
  // lus nodig - dat is hier wsl dan een tail recursive function?
  def go(l: MyList[A], n: Int): MyList[A] =
    if (n == 0) l
    else go(tail(l), n - 1)

  go(l, n)
}

def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
  case MyCons(x, y) if f(x) => dropWhile(y)(f)
  case _ => l
}

def init[A](l: MyList[A]): MyList[A] = l match {
  case MyNil => sys.error("wrong!")
  case MyCons(_, MyNil) => MyNil
  case MyCons(h, t) => MyCons(h, init(t))
}

def length[A](l: MyList[A]): Int = {
  foldRight(l, 0)((_, accumulator) => accumulator + 1)
}

@annotation.tailrec
def foldLeft[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = l match {
  case MyNil => z
  case MyCons(h, t) => foldLeft(t, f(z, h))(f)
}

def sum3(ns: MyList[Int]) =
  foldLeft(ns, 0)((x, y) => x + y)

def length2[A](l: MyList[A]): Int = {
  foldLeft(l, 0)((accumulator, _) => accumulator + 1)
}

def map[A, B](l: MyList[A])(f: A => B): MyList[B] = sys.error("todo")

val example: MyList[Int] = MyCons(1, MyCons(2, MyCons(3, MyNil)))
tail(example)
setHead(example, MyCons(4, MyNil))
drop(example, 2)
dropWhile(example)(x => x < 2)
init(example)
//foldRight(example, MyNil:MyList[Int])(MyCons(_,_))
sum2(example)
sum3(example)
length(example)
length2(example)