import ParserMelodia._

object PlayAudio extends App {
  val fc =
    "4C1/4 4C1/4 4D1/2 4C1/4 4F1/2 4E1/2 4C1/8 4C1/4 4D1/2 4C1/2 4G1/2 4F1/2 4C1/8 4C1/4 5C1/2 4A1/2 4F1/8 4F1/4 4E1/2 4D1/2"

//  Ahora convertir la partitura a la melodía y pasarle eso al AudioPlayer les toca hacerlo a ustedes.



  AudioPlayer.reproducir(ParserMelodia.parse(fc).getResultado)
}
