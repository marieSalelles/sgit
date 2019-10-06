package sgit.localChangeRepo

import better.files.File
import sgit.io.{CreateFile, RepoSearching}

import scala.util.matching.Regex

object AddCommand {

  /**
   * Check the argument type of add command and add the files to staged state.
   * @param files : sequence of string which can be a dot, a regex or filenames
   */
  def addAccordingTypeArg(files :Seq[String]): Unit = {
    if(files.nonEmpty) {
      if (files.head == ".") {
        val retrieveFiles :Seq[File]= RepoSearching.searchAllDirectoryFile()
        val blobs :Seq[String]= CreateFile.createObjectBlob(retrieveFiles.filterNot(f => f.isDirectory))
      }
      else {
        val regexFilename: Regex = "^[a-zA-Z0-9_]+\\.[A-Za-z]*$".r
        if (regexFilename.matches(files.head)) {
          val retrievesFiles: Seq[File] = RepoSearching.searchDirectoryFile(files)
        } else {
          val retrieveFiles: Seq[File] = RepoSearching.searchDirectoryFile(files.head.r)
          val blobs: Seq[String] = CreateFile.createObjectBlob(retrieveFiles.filterNot(f => f.isDirectory))
          //retrieveFiles.filter(f => f.isDirectory)
        }
      }
    } else println("enter an argument after the add command.")
  }
}
