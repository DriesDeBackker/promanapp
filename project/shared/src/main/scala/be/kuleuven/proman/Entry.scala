package be.kuleuven.proman

import scalatags.generic.Bundle

case class Entry(id: Int, var done: Boolean, var content: String) {
  val creationTime: Long = System.currentTimeMillis()

  def hasID(searchId: Int): Boolean = searchId == id
}

class EntryTemplate[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def entryDivTemplate(e: Entry) =
    div(id := e.id)

  def entryTemplate(e: Entry) =
    p(e.content)

  def entryInputTemplate(e: Entry) =
    input(
      id := e.id + "input",
      tpe := "text",
      value := e.content,
      style := "border: none;"
    )

  def entryButtonTemplate(e: Entry) =
    if (e.done) button("Mark as to do")
    else button("Mark as done")

  def entrysTemplate(es: Seq[Entry]) =
    div(
      es.map(entryTemplate)
    )
  def entriesViewTemplate(projectName: String) =
    div(
      h2(projectName),
      div(id := "entries") (
        h3("To do"),
        div(id := "undoneEntries"),
        h3("Done"),
        div(id := "doneEntries")
      ),
      br(),
      div(
        input(id := "entryName",
          tpe := "text",
          placeholder := "New Entry"),
        button(id := "addEntry", "Add")
      ),
      br(),
      button(id := "backToProjects", "Back to projects")
    )
}
