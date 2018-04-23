package algebraic.manipulator

trait Environment extends Element {
  val path: List[String]
  val base: Environment = this
  val names: List[String] = Nil
  val bound: Set[String] = Set.empty
  def apply(name: String): Element = throw new NoSuchElementException(name)
  def contains(name: String): Boolean = names.contains(name)
  def find(path: List[String]): Element
  def toFull(path: List[String]): List[String]

  override def filter(predicate: Element => Boolean): Traversable[Element] = None
  override def validate(env: Environment): Traversable[(List[String], String)] = None

  def apply(path: List[String]): Element =
    if (path.tail.isEmpty) apply(path.head)
    else apply(path.head) match {
      case env: Environment => env(path.tail)
      case _ => throw new IllegalArgumentException(s"${path.head} isn't an environment in ${this.path.mkString(".")}")
    }

  def scope(name: String): Environment = Environment.Scope(this, name)
  def ++(params: List[Definition]): Environment = ++(params.map(_.name).toSet)
  def ++(bound: Set[String]): Environment = ++(bound.map(_ -> Environment.empty).toMap)
  def ++(bound: Map[String, Element]): Environment = (this /: bound)(_+_)
  def +(e: (String, Element)): Environment = Environment.Compound(this, e._1, e._2)
}

object Environment {
  def empty: Environment = Empty

  case object Empty extends Environment {
    override val path: List[String] = Nil
    override def find(path: List[String]): Element = throw new NoSuchElementException(path.mkString("."))
    override def toFull(path: List[String]): List[String] = throw new NoSuchElementException(path.mkString("."))
    override def dependencies: Set[String] = Set.empty
  }

  case class Scope(parent: Environment, name: String) extends Environment {
    override val path: List[String] = parent.path :+ name
    override def find(path: List[String]): Element = parent.find(path)
    override def toFull(path: List[String]): List[String] = parent.toFull(path)
    override def dependencies: Set[String] = Set.empty
  }

  case class Compound(env: Environment, key: String, element: Element) extends Environment {
    override val path: List[String] = env.path
    override val base: Environment = env.base
    override val names: List[String] = env.names :+ key
    override val bound: Set[String] = env.bound + key
    override def apply(name: String): Element = if (name == key) element else env(name)

    override def find(path: List[String]): Element =
      if (path.head == key) apply(path)
      else env.find(path)

    override def toFull(path: List[String]): List[String] =
      if (path.head == key) this.path ++ path
      else env.toFull(path)

    override def filter(predicate: Element => Boolean): Traversable[Element] =
      if (predicate(element)) env.filter(predicate) ++ Some(element)
      else element match {
        case e: Environment => env.filter(predicate) ++ e.filter(predicate)
        case _ => env.filter(predicate)
      }

    override def dependencies: Set[String] = env.dependencies ++ (element.dependencies -- env.bound)

    override def validate(env: Environment): Traversable[(List[String], String)] =
      this.env.validate(env) ++ element.validate(this)
  }
}