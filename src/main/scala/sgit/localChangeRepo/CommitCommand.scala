package sgit.localChangeRepo

import sgit.io.{ConsolePrinter, CreateFile, ReadFile, WriteFile}
import sgit.objects.StagedLine

object CommitCommand {

  def commit(): Option[String] = {
    //retrieve the last commit
    val currentBranch = ReadFile.readHEAD()
    val lastCommit: Option[String] = ReadFile.readHeads(currentBranch)
    //retrieve the file in the staged file
    val stagedFiles :Option[Seq[StagedLine]] = ReadFile.readStaged()
    if(stagedFiles.isEmpty){
      ConsolePrinter.display("No files have been staged.")
      None
    }
    else {
      val filesToWrite :Seq[StagedLine] = stagedFiles.get
      if (lastCommit.isDefined) {
        val filesPath = filesToWrite.map(f => f.path)

        //retrieve the content of the last commit
        val lastCommitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //create the content of the new commit, replace the old version of file in last commit which are staged
        val newContent: Seq[StagedLine] = lastCommitContent.map(f => {
          if (filesPath.contains(f.path)) {
            filesToWrite.filter(file => file.path == f.path).head
          } else f
        })
        //create the commit file with the new files newContent and the file untracked) and the old ones
        val newCommit :String= CreateFile.createCommit((lastCommit.get, ""),newContent.concat(filesToWrite).distinct)
        // write the new commit on the current ranch file
        WriteFile.writeHeadsFile(currentBranch, newCommit)
        //clear the staged file
        WriteFile.clearStaged()
        ConsolePrinter.display("Succes, the commit is created.")
        Some(newCommit)
      } else {
        //create the commit file and object
        val newCommit :String= CreateFile.createCommit(("",""),filesToWrite)
        // write the new commit on the current ranch file
        WriteFile.writeHeadsFile(currentBranch, newCommit)
        //clear the staged file
        WriteFile.clearStaged()
        ConsolePrinter.display("Succes, the commit is created.")
        Some(newCommit)
      }
    }
  }

}
