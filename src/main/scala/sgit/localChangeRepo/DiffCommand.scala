package sgit.localChangeRepo

import better.files.File
import sgit.io.{ConsolePrinter, ReadFile, RepoSearching, SearchingTools}
import sgit.objects.{Blob, StagedLine}

import scala.annotation.tailrec

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
            //check if the content of the file is not the same in the 2 versions.
            if(f._1.content != f._2.content) {
              ConsolePrinter.display(f._1.path)
              showDifferences(f._1.content.replace("\r","").split("\n"),
                f._2.content.replace("\r","").split("\n"))
            }
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
   * Search the difference between two versions of the same file.
   * @param commitFileContent : content of the file in its older version
   * @param wdFileContent : content of the file in its current version.
   * @return a tuple with the deleted line and the added line in the file.
   */
  def showDifferences(commitFileContent: Array[String], wdFileContent: Array[String] ):(List[String], List[String]) = {
    //line with the index of the it
    val commitContent = commitFileContent.filterNot(dl => dl =="").toList.zipWithIndex
    val wdContent = wdFileContent.filterNot(dl => dl =="").toList.zipWithIndex
    //search deleted lines
    val deletedLine: List[(String,Int)] = commitContent.diff(wdContent).distinct
    //search added lines
    val addedLine: List[(String,Int)] = wdContent.diff(commitContent).distinct
    //print the line according to their position in the file
    printDiffLine(addedLine, deletedLine)

    (deletedLine.map(l => l._1), addedLine.map(l=>l._1))
  }

  /**
   * Order the lines from the first to the last in the file to print it in the asc order
   * @param addedLine : added line
   * @param deletedLine : deleted line
   */
  @tailrec
  def printDiffLine(addedLine: Seq[(String, Int)], deletedLine: Seq[(String, Int)]): Unit = {
    if (deletedLine.nonEmpty || addedLine.nonEmpty){
      if (addedLine.isEmpty){
        ConsolePrinter.displayRed(deletedLine.head._2.toString, deletedLine.head._1)
        printDiffLine(Seq(), deletedLine.tail)
      } else if (deletedLine.isEmpty){
        ConsolePrinter.displayGreen(addedLine.head._2.toString ,addedLine.head._1)
        printDiffLine(addedLine.tail,Seq())
      } else if (deletedLine.head._2 <= addedLine.head._2){
        ConsolePrinter.displayRed(deletedLine.head._2.toString, deletedLine.head._1)
        printDiffLine(addedLine, deletedLine.tail)
      } else if(deletedLine.head._2 > addedLine.head._2) {
        ConsolePrinter.displayGreen(addedLine.head._2.toString, addedLine.head._1)
        printDiffLine(addedLine.tail, deletedLine)
      }
    }
  }
}
