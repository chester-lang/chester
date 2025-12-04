package chester.utils.io

import java.io.IOException
import java.net.URL
import java.nio.file.{Files, Path, StandardCopyOption}

object FileDownloader {

  @throws[IOException]
  def downloadFile(urlString: String, targetPath: Path): Unit = {
    val tempFile = Files.createTempFile(targetPath.getParent, "temp-", ".tmp")

    try {
      val url = new URL(urlString)
      val inputStream = url.openStream()

      try {
        // Download the file to a temporary location
        Files.copy(inputStream, tempFile, StandardCopyOption.REPLACE_EXISTING)

        // Move the temporary file to the target location
        val _ = Files.move(tempFile, targetPath, StandardCopyOption.REPLACE_EXISTING)
      } finally inputStream.close()

    } catch {
      case e: IOException =>
        // Clean up in case of failure
        try
          Files.deleteIfExists(tempFile)
        catch {
          case _: IOException =>
        }
        throw e // Rethrow the exception to indicate failure
    }
  }
}
