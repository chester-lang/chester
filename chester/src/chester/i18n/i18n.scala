package chester.i18n

extension (sc: StringContext) {
  def t(args: Any*): String = sc.s(args*)
}
