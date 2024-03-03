package lox

final class LoxFunction(declaration: Stmt.Function, closure: Environment, isInitializer: Boolean) extends LoxCallable:
  override def arity: Int = declaration.params.length

  override def call(interpreter: Interpreter, arguments: Seq[LoxValue]): LoxValue =
    val environment = Environment(closure)

    for (param, arg) <- declaration.params.zip(arguments) do
      environment.define(param.lexeme, arg)

    try
      interpreter.withEnvironment(environment):
        declaration.body.foreach(_.visit(interpreter))
      if isInitializer then closure.getAt(0, "this").get else null
    catch
      case returnValue: Return =>
        if isInitializer then
          closure.getAt(0, "this").get
        else
          returnValue.value

  override def toString: String = s"<fn ${declaration.name.lexeme}>"

  def bind(instance: LoxInstance): LoxFunction =
    val environment = Environment(closure)
    environment.define("this", instance)
    LoxFunction(declaration, environment, isInitializer)

end LoxFunction
