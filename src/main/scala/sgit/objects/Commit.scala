package sgit.objects

import java.time.Instant

case class Commit (sha: String, timestamp: Instant, parents: Seq[String], files: Seq[StagedLine]) {
}
