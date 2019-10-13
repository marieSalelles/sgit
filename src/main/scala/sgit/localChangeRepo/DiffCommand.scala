package sgit.localChangeRepo

import better.files.File
import sgit.io.{ConsolePrinter, ReadFile, RepoSearching, SearchingTools}
import sgit.objects.{Blob, StagedLine}

object DiffCommand {

  /**
   * Allows the displaying of the difference between 2 version of the same file
   * @return true if there are difference(s)
   */
  def diffBetweenCommitWD(): Boolean = {
    if(SearchingTools.searchSgitFolder()) {
      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)
      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      if (lastCommit.isDefined) {

        //content of last commit
        val commitContent: Seq[StagedLine]= ReadFile.readCommit(lastCommit.get)

        //find the modified files
        val fileWithDifference: Option[Seq[(Blob,Blob)]] = SearchingTools.searchDifferentFileBetweenWDCommit(workingDirectory,commitContent)

        if (fileWithDifference.nonEmpty) {

          fileWithDifference.get.map(f =>  {
            ConsolePrinter.display(f._1.path)
            showDifferences(f._1.content.replace("\r","").split("\n"),
              f._2.content.replace("\r","").split("\n"))
          })
          true
        } else {
          ConsolePrinter.display("There are no differences between the last commit and the working directory.")
          false
        }
      } else{
        ConsolePrinter.display("Do a sgit commit before a sgit diff.")
        false
      }
    } else {
      ConsolePrinter.display("Do a sgit init before a sgit diff.")
      false
    }
  }

  /**
   * Search the difference between two versions of the same document.
   * @param commitFileContent : content of the file in its older version
   * @param wdFileContent : content of the file in its current version.
   * @return a tuple with the deleted line and the added line in the file.
   */
  def showDifferences(commitFileContent: Array[String], wdFileContent: Array[String] ):(Array[String], Array[String]) = {
    val deletedLine: Array[String] = commitFileContent.diff(wdFileContent).distinct.filterNot(dl => dl =="")
   if(deletedLine.nonEmpty) ConsolePrinter.displayList(deletedLine)
    val addedLine: Array[String] = wdFileContent.diff(commitFileContent).distinct.filterNot(al => al =="")
    if(addedLine.nonEmpty) ConsolePrinter.displayList(addedLine)
    (deletedLine, addedLine)
  }
}
