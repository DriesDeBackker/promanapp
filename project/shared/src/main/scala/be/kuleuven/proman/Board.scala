package be.kuleuven.proman

import scalatags.generic.Bundle

case class Board(var name: String) {
  var entries: Seq[Entry] = Seq()

  def addEntry(entry: Entry): Unit = {
    entries = entries :+ entry
  }

  def addNewEntry(entryContent: String): Unit = {
    addEntry(Entry(entries.size+1, done = false, entryContent))
  }

  def getEntry(id: Int): Entry = {
    entries.filter(_.hasID(id)).head
  }

  def hasName(searchName :String): Boolean = searchName == name
}

class BoardTemplate[Builder, Output <: FragT, FragT](
                                                        val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def boardsViewTemplate(projectName: String) =
    div(id := projectName,
      backgroundColor:= "grey"
      h2(projectName,
        textTransform := "uppercase"),
      textAlign := "center",
      padding := "30px")(
      div(id := "boards",
        display := "flex",
        flexWrap:= "wrap",
        justifyContent := "center"),
      br(),
      div(
        input(id := "boardName",
          tpe := "text",
          placeholder := "New Board"),
        button(id := "addBoard", "Add")
      ),
      br(),
      button(id := "backToProjects", "Back to projects")
    )
}
