package be.kuleuven.proman

import scalatags.generic.Bundle

case class Project(name: String) {
  var entries: Seq[Entry] = Seq()
}

class ProjectTemplate[Builder, Output <: FragT, FragT](
    val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def projectTemplate(p: Project) =
    div(id := p.name, p.name)

  def projectsTemplate(pjs: Seq[Project]) =
    div(
      pjs.map(projectTemplate)
    )
  def projectsViewTemplate() =
    div(
      div(id := "projects"),
      div(
        input(id := "projectName",
          tpe := "text",
          placeholder := "New Project"),
        button(id := "addProject", "Add")
      )
    )
}
