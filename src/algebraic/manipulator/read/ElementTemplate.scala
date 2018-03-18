package algebraic.manipulator.read

import algebraic.manipulator.{Depending, Element, Project}

trait ElementTemplate extends Depending {
  def apply(finder: Project.Finder): Element
}
