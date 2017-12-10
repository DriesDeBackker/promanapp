package be.kuleuven.proman

import scalatags.generic.Bundle

case class Animal(breed: String, name: String)

class AnimalTemplate[Builder, Output <: FragT, FragT](
    val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def animalTemplate(a: Animal) = p(s"${a.name} is a ${a.breed}")
  def animalsTemplate(as: Seq[Animal]) = div(as.map(animalTemplate))
}
