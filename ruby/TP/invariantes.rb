class Object
  def self.invariante(&condicion)
    @invariantes ||= []
    @invariantes.push(condicion)
  end

  def chequear_invariantes
    invariantes = self.class.class_eval {@invariantes}
    if !invariantes.all? {|condicion| self.instance_eval &condicion}
      raise "No se cumplen todas las invariantes!!!"
    end
  end
end

class Ejemplo
  attr_accessor :atributo

  def initialize
    self.atributo = 3
  end

  invariante { atributo > 0 }

  invariante { atributo < 10 }

  def m
    chequear_invariantes
    self.atributo -= 1
    chequear_invariantes
  end

  def a
    chequear_invariantes
    self.atributo += 5
    chequear_invariantes
  end
end
