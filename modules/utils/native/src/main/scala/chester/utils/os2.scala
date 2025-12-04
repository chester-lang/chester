package chester.utils

import os.Path

object os2 {
  // https://github.com/com-lihaoyi/os-lib/issues/318
  private lazy val pwdGraalVM: Path = {
    val result = os.Path(java.nio.file.Paths.get(".").toAbsolutePath)
    os.dynamicPwd.value = result
    result
  }
  def init(): Unit = {
    onNativeImageBuildTime {
      throw new IllegalStateException("os2.init() should not be called at build time")
    }
    onNativeImageRunTime {
      val _ = pwdGraalVM
    }
  }

  def pwd: Path = {
    onNativeImageBuildTime {
      throw new IllegalStateException("os2.pwd should not be called at build time")
    }
    ifNativeImageRunTime {
      pwdGraalVM
    } {
      os.pwd
    }
  }
  def path(x: String): os.Path = os.Path(x, pwd)
}
