package chester.idea
// Copyright 2000-2020 JetBrains s.r.o. and other contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
import com.intellij.openapi.components.Service
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.NotNull
@Service
final class ProjectHelloService(project: Project) {
  def getProjectHelloInfo: String =
    ChesterPluginBundle.message("hello.from.project.getname", project.getName)
}
object ProjectHelloService {
  def getInstance(@NotNull project: Project): ProjectHelloService =
    project.getService(classOf[ProjectHelloService])
}