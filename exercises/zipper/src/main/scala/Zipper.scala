object Zipper {
  final case class Zipper[A](tree: BinTree[A], breadcrumbs: Breadcrumbs[A])

  type Breadcrumbs[A] = List[Crumb[A]]

  final case class Crumb[A](direction: Direction, value: A, tree: Option[BinTree[A]]) {
    def toTree(t: Option[BinTree[A]]): BinTree[A] = direction match {
      case Direction.Left  => BinTree(value, t, tree)
      case Direction.Right => BinTree(value, tree, t)
    }
  }

  sealed trait Direction extends Product with Serializable
  object Direction {
    type T = Direction
    case object Left extends T
    case object Right extends T
  }

  // Get a zipper focused on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt, Nil)

  // Get the complete tree from a zipper.
  def toTree[A](zipper: Zipper[A]): BinTree[A] =
    zipper.breadcrumbs.foldLeft[BinTree[A]](zipper.tree) { (t, c) => c.toTree(Some(t)) }

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = zipper.tree.value

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    val crumb = Crumb(Direction.Left, zipper.tree.value, zipper.tree.right)
    zipper.tree.left map (Zipper(_, crumb :: zipper.breadcrumbs))
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    val crumb = Crumb(Direction.Right, zipper.tree.value, zipper.tree.left)
    zipper.tree.right map (Zipper(_, crumb :: zipper.breadcrumbs))
  }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] =
    zipper.breadcrumbs.headOption map {
      b => Zipper(b.toTree(Some(zipper.tree)), zipper.breadcrumbs.tail)
    }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] =
    zipper.copy(tree = zipper.tree.copy(value = v))

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] =
    zipper.copy(tree = zipper.tree.copy(left = l))

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] =
    zipper.copy(tree = zipper.tree.copy(right = r))
}

// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])

