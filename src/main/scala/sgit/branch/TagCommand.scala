package sgit.branch

import sgit.io.{ConsolePrinter, CreateFile, SearchingTools}

object TagCommand {

  /**
   * Create a tag.
   * @param tagName : tag name
   * @return the printed string into the console
   */
  def newTag(tagName: String): String = {
    if(SearchingTools.searchSgitFolder()) {
      if(tagName=="") {
        val allTags: Option[Seq[(String, String)]] = SearchingTools.searchAllTags()
        if (allTags.nonEmpty){
          ConsolePrinter.displayList(allTags.get.map(tag => tag._1))
          "Display all the tags."
        } else "There is no tags."
      } else if (!SearchingTools.searchTag(tagName)) {
        val commit: Option[String] = SearchingTools.findLastCommit()
        //create a file in heads folder
        if (commit.isDefined) {
          //create the tag file
          CreateFile.writeTagsFile("refs/tags/"+ tagName, commit.get)
          ConsolePrinter.display("Success, the tag is created.")
          "Success, the tag is created."
        } else {
          ConsolePrinter.display("Do a commit before create a tag.")
          "Do a commit before create a tag."
        }
      } else {
        ConsolePrinter.display("The tag already exists.")
        "The tag already exists."
      }
    } else {
      ConsolePrinter.display("Do an sgit init.")
      "Do an sgit init."
    }
  }
}
