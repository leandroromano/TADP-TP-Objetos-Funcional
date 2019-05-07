class InvarianteSinCumplir < RuntimeError
  def message
    "No se cumplen todas las invariantes."
  end
end

class Module
=begin
  def invariante(&invariante)
    @invariantes ||= [] # inicializo por default como "[]"
    @invariantes.push(invariante) # agrego proc "condicion"
  end
=end
  def before_and_after_each_call(before, after)
    inicializar_y_heredar_befores_and_afters
    @befores.push(before) # agrego proc "before"
    @afters.push(after) # agrego proc "after"
  end

  def inicializar_y_heredar_befores_and_afters # Me aseguro que esten inicializados como listas  para que no sean nil
      @befores ||= self.superclass.instance_variable_get(:@befores) || []
      @afters ||= self.superclass.instance_variable_get(:@afters) || []
  end

  def overriden_method? method
    @overriden_methods ||= [:initialize] # Inicializo Overriden_methods. Incluyo "initialize": algunas invariantes involucran atributos que se inicializan en este metodo. Evito que initialize se redefina para que no ocurra un error al tratar con atributos dentro de invariantes no inicializados
    @overriden_methods.include? method
  end

  def redefinir_metodo method
    aux = self.instance_method(method) #unbound. Guardo el metodo original
    befores = @befores
    afters = @afters
    define_method method do |*args| # redefino el metodo original con el mismo nombre y sus argumentos
      befores.reverse_each{|p| instance_eval &p} # evaluo todos los procs "before" dentro del contexto de la instancia correspondiente
      #self.chequear_invariantes
      retorno = aux.bind(self).call(*args) # bindeo y ejecuto el metodo original. Guardo el valor de retorno
      #self.chequear_invariantes
      afters.each{|p| instance_eval &p} # evaluo todos los procs "after" dentro del contexto de la instancia correspondiente
      retorno # retorno original
    end
  end

  def method_added method
    inicializar_y_heredar_befores_and_afters
    if !overriden_method? method # control para que haya un bucle de definicion y redefinicion infinito
      @overriden_methods.push method # agrego metodo a la lista de metodos sobreescritos
      redefinir_metodo method # redefino el metodo con las caracteristicas pedidas
    end
  end
end

=begin
class Object
  def chequear_invariantes
    if !invariantes_ok? # verifico que se cumplan todas las invariantes
      raise InvarianteSinCumplir # en caso de que no se cumplan tiro una excepcion
    end
  end

  def invariantes_ok? # verifica que se cumplan todas las invariantes
    invariantes = @invariantes || []
    invariantes.all? {|invariante| instance_eval &invariante}
  end
end
=end