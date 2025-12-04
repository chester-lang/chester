package chester.utils

inline def onNativeImageBuildTime(f: => Unit): Unit = ()
inline def onNativeImageRunTime(f: => Unit): Unit = ()

inline def ifNativeImageRunTime[A](f: => A)(g: => A): A = g

inline def ifNativeImage[A](f: => A)(g: => A): A = g
