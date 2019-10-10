package sgit.localChangeRepo

import better.files.File
import com.sun.xml.internal.bind.v2.runtime.reflect.Accessor.ReadOnlyFieldReflection
import sgit.io.{ReadFile, RepoSearching, SearchingTools}
import sgit.objects.StagedLine

object StatusCommand {

  def statusFile() = {
    val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile()
    val stagedFile :Option[Seq[StagedLine]]= ReadFile.readStaged()
    if (stagedFile.isDefined){
      val untrackedFiles: Option[Seq[File]] = SearchingTools.searchedUntrackedFiles(workingDirectory)
      //val deletedFiles: Option[Seq[StagedLine]] = SearchingTools.searchDeletedFiles(workingDirectory, stagedFile)
    } else {
      println("Untracked files :")
      workingDirectory.map(f => println(f.name))
    }
  }

}
