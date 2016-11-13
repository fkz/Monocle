package monocle.std

import monocle.{Prism, PPrism}
import monocle.catssupport.Implicits._

object either3 extends Either3Optics

trait Either3Optics {

  final def pLeft3[A, B, C, D]: PPrism[Either3[A, B, C], Either3[D, B, C], A, D] =
    PPrism[Either3[A, B, C], Either3[D, B, C], A, D] {
      case Left3(a)   => \/.right(a)
      case Middle3(b) => \/.left(Middle3(b))
      case Right3(c)  => \/.left(Right3(c))
    }(Left3.apply)

  final def left3[A, B, C]: Prism[Either3[A, B, C], A] =
    pLeft3[A, B, C, A]

  final def pMiddle3[A, B, C, D]: PPrism[Either3[A, B, C], Either3[A, D, C], B, D] =
    PPrism[Either3[A, B, C], Either3[A, D, C], B, D] {
      case Left3(a)   => \/.left(Left3(a))
      case Middle3(b) => \/.right(b)
      case Right3(c)  => \/.left(Right3(c))
    }(Middle3.apply)

  final def middle3[A, B, C]: Prism[Either3[A, B, C], B] =
    pMiddle3[A, B, C, B]

  final def pRight3[A, B, C, D]: PPrism[Either3[A, B, C], Either3[A, B, D], C, D] =
    PPrism[Either3[A, B, C], Either3[A, B, D], C, D] {
      case Left3(a)   => \/.left(Left3(a))
      case Middle3(b) => \/.left(Middle3(b))
      case Right3(c)  => \/.right(c)
    }(Right3.apply)

  final def right3[A, B, C]: Prism[Either3[A, B, C], C] =
    pRight3[A, B, C, C]
}
