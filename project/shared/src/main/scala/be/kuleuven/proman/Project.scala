package be.kuleuven.proman

import scalatags.generic.Bundle

case class Project(name: String) {
  var entries: Seq[Entry] = Seq()

  def addEntry(entry: Entry): Unit = {
    entries = entries :+ entry
  }

  def addNewEntry(entryContent: String): Unit = {
    addEntry(Entry(entries.size+1, done = false, entryContent))
  }

  def updateEntry(entry: Entry): Unit = {
    val storedEntry = getEntry(entry.id)
    storedEntry.done = entry.done
    storedEntry.content = entry.content
  }

  def getEntry(id: Int): Entry = {
    entries.filter(_.hasID(id)).head
  }

  def hasName(searchName :String): Boolean = searchName == name
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
      ),
      br()
    )
}
