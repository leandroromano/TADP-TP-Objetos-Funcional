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

  protected def parseNote(): Nota = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')
    Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotANoteException(next))
  }

  def parse(): List[Nota] = {
    val result: ListBuffer[Nota] = ListBuffer()
    try while (true)
      result += parseNote()
    catch {
      case _: EOIParserException =>
    }
    return result.toList
  }
}

class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")

trait Expression {
  def toPartitura: List[Nota]
}
case class Multiplicacion(multiplicador: Integer, contenido: Exprecion){
  def toPartitura: List[Nota] = {
    var aux: List[Nota] = List()
    (1 to multiplicador).foreach{ _ => aux = aux ::: contenido.toPartitura }
    return aux
  }
}
case class SimpleExpression(contenido: List[Nota]){
  def toPartitura: List[Nota] = contenido
}