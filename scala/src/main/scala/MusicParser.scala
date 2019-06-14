import java.io.{PushbackReader, StringReader}
import Musica._
import scala.collection.mutable.ListBuffer


case class Note(name: String)

class MusicParser(input: String) {
  protected val inputStream = new PushbackReader(new StringReader(input))

  protected def parseChar(): Char = {
    val parsed = inputStream.read()
    if (parsed == -1) throw new EOIParserException
    return parsed.toChar
  }

  /*
  protected def parseNote(): Nota = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')
    Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotAnExpressionException(next))
  }
  */
  protected def parseExpression(): Expression = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')
    next match {
      case n if (n.isDigit) =>
        if (parseChar() != 'x' || parseChar() != '(')  // <-- Tiene que estar junto. Ej: "A B C B 2x(A B C D)"
          throw new SyntaxErrorException()
        else {
          val expression: ComplexExpression = ComplexExpression()
          try {
            var next2: Char = ' '

            while (next2 != ')') {
              expression.add(parseExpression())
              next2 = parseChar()
            }

          } catch {
            case _: EOIParserException => //<-- Revisar
          }

          return Multiplication(n.asDigit, expression)
        }
      case _ =>
        SimpleExpression(Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotAnExpressionException(next)))
    }
  }

  def parse(): List[Nota] = {
    val result: ComplexExpression = ComplexExpression()
    try while (true)
      result.add(parseExpression())
    catch {
      case _: EOIParserException =>
    }
    return result.toPartitura
  }
}

class ParserException(reason: String) extends Exception(reason)

class EOIParserException extends ParserException("reached end of input")

class NotAnExpressionException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G| Nx( ] but got $read")

trait Expression {
  def toPartitura: List[Nota]
}

case class Multiplication(multiplicador: Integer, contenido: Expression) extends Expression {
  def toPartitura: List[Nota] = {
    var aux: List[Nota] = List()
    (1 to multiplicador).foreach(_ => aux = aux ::: contenido.toPartitura)
    return aux
  }
}

case class SimpleExpression(contenido: Nota) extends Expression {
  def toPartitura: List[Nota] = List(contenido)
}

case class ComplexExpression(var contenido: List[Expression] = List()) extends Expression {
  def toPartitura: List[Nota] = contenido.flatMap(_.toPartitura)

  def add(expression: Expression): Unit = {
    this.contenido = this.contenido ::: List(expression)
  }
}

class SyntaxErrorException extends ParserException("Wrong syntax")