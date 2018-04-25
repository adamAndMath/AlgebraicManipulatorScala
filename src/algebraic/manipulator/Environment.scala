package algebraic.manipulator

trait Environment extends Element {
  val path: List[String]
  val base: Environment = this
  val names: Set[String] = Set.empty
  def apply(name: String, predicate: Element => Boolean): Option[Element] = None
  def contains(name: String): Boolean = names.contains(name)
  def find(path: List[String], predicate: Element => Boolean): Option[Element]
  def toFull(path: List[String], predicate: Element => Boolean): Option[List[String]]

  override def filter(predicate: Element => Boolean): Traversable[Element] = None
  override def validate(env: Environment): Traversable[(List[String], String)] = None

  def apply(path: List[String], predicate: Element => Boolean): Option[Element] =
    if (path.tail.isEmpty) apply(path.head, predicate)
    else apply(path.head, (_:Element).isInstanceOf[Environment]).flatMap(_.asInstanceOf[Environment](path.tail, predicate))

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
    override def find(path: List[String], predicate: Element => Boolean): Option[Element] = None
    override def toFull(path: List[String], predicate: Element => Boolean): Option[List[String]] = None
  }

  case class Scope(parent: Environment, name: String) extends Environment {
    override val path: List[String] = parent.path :+ name
    override def find(path: List[String], predicate: Element => Boolean): Option[Element] = parent.find(path, predicate)
    override def toFull(path: List[String], predicate: Element => Boolean): Option[List[String]] = parent.toFull(path, predicate)
  }

  case class Compound(env: Environment, key: String, element: Element) extends Environment {
    override val path: List[String] = env.path
    override val base: Environment = env.base
    override val names: Set[String] = env.names + key

    override def apply(name: String, predicate: Element => Boolean): Option[Element] =
      Some(element).filter(e => name == key && predicate(e))
        .orElse(env(name, predicate))

    override def find(path: List[String], predicate: Element => Boolean): Option[Element] =
      apply(path, predicate).orElse(base.find(path, predicate))

    override def toFull(path: List[String], predicate: Element => Boolean): Option[List[String]] =
      Some(this.path ++ path).filter(_ => path.head == key && apply(path, predicate).isDefined)
        .orElse(env.toFull(path, predicate))

    override def filter(predicate: Element => Boolean): Traversable[Element] =
      if (predicate(element)) env.filter(predicate) ++ Some(element)
      else element match {
        case e: Environment => env.filter(predicate) ++ e.filter(predicate)
        case _ => env.filter(predicate)
      }

    override def validate(env: Environment): Traversable[(List[String], String)] =
      this.env.validate(env) ++ element.validate(this)
  }
}