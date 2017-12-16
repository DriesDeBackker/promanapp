package be.kuleuven.proman

import scalatags.generic.Bundle

case class Project(var name: String) {
  var boards: Seq[Board] = Seq()

  def addBoard(board: Board): Unit = {
    boards = boards :+ board
  }

  def addNewBoard(boardName: String): Unit = {
    addBoard(Board(boardName))
  }

  def getBoard(name: String): Board = {
    boards.filter(_.hasName(name)).head
  }

  def hasName(searchName :String): Boolean = searchName == name
}

class ProjectTemplate[Builder, Output <: FragT, FragT](
    val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def projectTemplate(p: Project) =
    div(id := p.name, p.name,
      width := "100px",
      height := "90px",
      backgroundColor := "grey",
      margin := "10px",
      padding := "5px",
      color := "white",
      lineHeight := "90px",
      cursor := "pointer")

  def projectsTemplate(pjs: Seq[Project]) =
    div(
      pjs.map(projectTemplate)
    )
  def projectsViewTemplate() =
    div(
      textAlign := "center",
      padding := "30px")(
      div(id := "projects",
        display := "flex",
        justifyContent := "center"),
      br(),
      div(
        input(id := "projectName",
          tpe := "text",
          placeholder := "New Project"),
        button(id := "addProject", "Add")
      ),
      br()
    )
}
