package monocle.std

import monocle.{Lens, OnlyInScalaz}
import monocle.function.all._

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty
//import scalaz.Tree
//import scalaz.Tree.{Leaf, Node}

object tree extends TreeOptics

trait TreeOptics {

  @OnlyInScalaz
  final def rootLabel[A]: Lens[Tree[A], A] =
    Lens[Tree[A], A](_.rootLabel)(l => tree => Node(l, tree.subForest))

  @OnlyInScalaz
  final def subForest[A]: Lens[Tree[A], Stream[Tree[A]]] =
    Lens[Tree[A], Stream[Tree[A]]](_.subForest)(children => tree => Node(tree.rootLabel, children))

  @OnlyInScalaz
  final def leftMostLabel[A]: Lens[Tree[A], A] = {
    @tailrec
    def _get(tree: Tree[A]): A = tree.subForest match {
      case Empty    => tree.rootLabel
      case x #:: xs => _get(x)
    }

    def _set(newLeaf: A)(tree: Tree[A]): Tree[A] = tree.subForest match {
      case Empty => Leaf(newLeaf)
      case xs    => Node(tree.rootLabel, headOption[Stream[Tree[A]], Tree[A]].modify(_set(newLeaf))(xs) )
    }

    Lens(_get)(_set)
  }

  @OnlyInScalaz
  final def rightMostLabel[A]: Lens[Tree[A], A] = {
    @tailrec
    def _get(tree: Tree[A]): A = tree.subForest match {
      case Empty => tree.rootLabel
      case xs    => _get(xs.last)
    }

    def _set(newLeaf: A)(tree: Tree[A]): Tree[A] = tree.subForest match {
      case Empty => Leaf(newLeaf)
      case xs    => Node(tree.rootLabel, lastOption[Stream[Tree[A]], Tree[A]].modify(_set(newLeaf))(xs) )
    }

    Lens(_get)(_set)
  }

}