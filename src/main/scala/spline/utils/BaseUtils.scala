package spline.utils

import scala.annotation.tailrec

def error(msg: String = ""): Nothing = throw Exception(msg)

def fixpoint[A](f: A => A)(x0: A): A =
  @tailrec def loop(x: A): A =
    val nx = f(x)
    if nx == x then x else loop(nx)
  loop(x0)
