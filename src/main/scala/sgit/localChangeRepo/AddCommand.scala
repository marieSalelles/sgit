package sgit.localChangeRepo

import better.files.File
import sgit.io.{CreateFile, RepoSearching, StateFileDefining, WriteFile}
import sgit.objects.Blob

import scala.util.matching.Regex

object AddCommand {

  /**
   * Check the argument type of add command and add the files to staged state.
   * @param files : sequence of string which can be a dot, a regex or filenames
   */
  def addAccordingTypeArg(files :Seq[String]): Unit = {
    if(files.nonEmpty) {
      if (files.head == ".") {
        //Search file in the user repo
        val retrieveFiles :Seq[File]= RepoSearching.searchAllDirectoryFile()
        //Create files in objects directory
        val blobs :Seq[Blob]= CreateFile.createObjectBlob(retrieveFiles.filterNot(f => f.isDirectory))
        //write the staged file
        WriteFile.writeStaged(StateFileDefining.updateStagedFile(blobs))
      }
      else {
        val regexFilename: Regex = "^[a-zA-Z0-9_/]+\\.[A-Za-z]*$".r
        if (regexFilename.matches(files.head)) {
          //Search file in the user repo
          val retrievesFiles: Seq[File] = RepoSearching.searchDirectoryFile(files)
          //Create files in objects directory
          val blobs = CreateFile.createObjectBlob(retrievesFiles)
          //write the staged file
          WriteFile.writeStaged(StateFileDefining.updateStagedFile(blobs))
        } else {
          //Search file in the user repo
          val retrieveFiles: Seq[File] = RepoSearching.searchDirectoryFile(files.head.r)
          //Create files in objects directory
          val blobs: Seq[Blob] = CreateFile.createObjectBlob(retrieveFiles.filterNot(f => f.isDirectory))
          //write the staged file
          WriteFile.writeStaged(StateFileDefining.updateStagedFile(blobs))
          //retrieveFiles.filter(f => f.isDirectory)
        }
      }
    } else println("enter an argument after the add command.")
  }
}
