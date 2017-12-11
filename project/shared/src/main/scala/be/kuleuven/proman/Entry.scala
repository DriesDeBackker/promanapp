package be.kuleuven.proman

import scalatags.generic.Bundle

case class Entry(name: String) {
  val creationTime = System.currentTimeMillis()
  var done = false;
  def setToUndone(): Unit = {
    done = true
  }
  def setToDone(): Unit = {
    done = false
  }
  def hasName(searchName: String): Boolean = name == searchName
}

class EntryTemplate[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def entryTemplate(e: Entry) =
    div(id := e.name)(
      p(e.name)
    )

  def entryButtonTemplate(e: Entry) =
    if (e.done) button("Mark as undone")
    else button("Mark as done")

  def entrysTemplate(es: Seq[Entry]) =
    div(
      es.map(entryTemplate)
    )
  def entriesViewTemplate() =
    div(
      div(id := "entries"),
      div(
        input(id := "entryName",
          tpe := "text",
          placeholder := "New Entry"),
        button(id := "addEntry", "Add")
      ),
      div(id := "entriesMessageBox"),
      button(id := "backToProjects", "Back to projects")
    )
}
