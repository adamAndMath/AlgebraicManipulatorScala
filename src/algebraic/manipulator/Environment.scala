package algebraic.manipulator

trait Environment extends Element {
  val path: List[String]
  val base: Environment = this
  val names: Set[String] = Set.empty
  val bound: Set[String] = Set.empty
  def apply[E <: Element](name: String, predicate: E => Boolean = (_:E) => true): Option[E] = None
  def contains(name: String): Boolean = names.contains(name)
  def find[E <: Element](path: List[String], predicate: E => Boolean = (_:E) => true): Option[E]
  def toFull[E <: Element](path: List[String], predicate: E => Boolean = (_:E) => true): Option[List[String]]

  override def filter(predicate: Element => Boolean): Traversable[Element] = None
  override def validate(env: Environment): Traversable[(List[String], String)] = None

  def apply[E <: Element](path: List[String], predicate: E => Boolean): Option[E] =
    if (path.tail.isEmpty) apply(path.head, predicate)
    else this.apply(path.head, (_: Environment) => true).flatMap(_[E](path.tail, predicate))

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
    override def find[E <: Element](path: List[String], predicate: E => Boolean): Option[E] = None
    override def toFull[E <: Element](path: List[String], predicate: E => Boolean): Option[List[String]] = None
    override def dependencies: Set[String] = Set.empty
  }

  case class Scope(parent: Environment, name: String) extends Environment {
    override val path: List[String] = parent.path :+ name
    override def find[E <: Element](path: List[String], predicate: E => Boolean): Option[E] = parent.find(path, predicate)
    override def toFull[E <: Element](path: List[String], predicate: E => Boolean): Option[List[String]] = parent.toFull(path, predicate)
    override def dependencies: Set[String] = Set.empty
  }

  case class Compound(env: Environment, key: String, element: Element) extends Environment {
    override val path: List[String] = env.path
    override val base: Environment = env.base
    override val names: Set[String] = env.names + key
    override val bound: Set[String] = env.bound + key

    override def apply[E <: Element](name: String, predicate: E => Boolean): Option[E] =
      Some(element).filter(e => name == key && e.isInstanceOf[E]).map(_.asInstanceOf[E])
        .orElse(env(name, predicate))

    override def find[E <: Element](path: List[String], predicate: E => Boolean): Option[E] =
      apply(path, predicate).orElse(base.find(path, predicate))

    override def toFull[E <: Element](path: List[String], predicate: E => Boolean): Option[List[String]] =
      Some(this.path ++ path).filter(_ => path.head == key && apply(path, predicate).isDefined)
        .orElse(env.toFull(path, predicate))

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