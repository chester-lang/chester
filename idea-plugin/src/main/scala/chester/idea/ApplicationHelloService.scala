package chester.idea
// Copyright 2000-2020 JetBrains s.r.o. and other contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.

import com.intellij.openapi.application.{ApplicationInfo, ApplicationManager}
import com.intellij.openapi.components.Service
@Service
final class ApplicationHelloService {
  def getApplicationHelloInfo: String =
    ChesterPluginBundle.message(
      "hello.this.is.asstring",
      ApplicationInfo.getInstance().getBuild.asString()
    )
}
object ApplicationHelloService {
  def getInstance: ApplicationHelloService = ApplicationManager
    .getApplication()
    .getService(classOf[ApplicationHelloService])
}
