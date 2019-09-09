package object mtscheme {
  type EnvMapT = Map[String, ExprT]
  type EnvT = List[EnvMapT]

  def EnvT(xs: EnvMapT*) = List(xs:_*)
  def EnvMapT(xs: (String, ExprT)*) = Map(xs:_*)
  def EnvT() = List(EnvMapT())
}
