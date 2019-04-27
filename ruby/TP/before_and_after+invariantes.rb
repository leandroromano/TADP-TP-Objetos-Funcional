class Object

  ############# invariantes ############### begin
  def self.invariante(&invariante)
    @invariantes ||= [] # inicializo por default como "[]"
    @invariantes.push(invariante) # agrego proc "condicion"
  end

  def chequear_invariantes
    if !invariantes_ok? # verifico que se cumplan todas las invariantes
      raise "No se cumplen todas las invariantes!!!" # en caso de que no se cumplan tiro una excepcion
    end
  end

  def invariantes_ok? # verifica que se cumplan todas las invariantes
    self.class.instance_variable_get(:@invariantes).all? {|invariante| instance_eval &invariante}
  end
  ############# invariantes ############### end
  #
  ############# before_and_after_each_call ############### begin
  def self.before_and_after_each_call(before, after)
    ensure_initialized_overriden_methods_befores_and_afters
    @befores.push(before) # agrego proc "before"
    @afters.push(after) # agrego proc "after"
    self.define_singleton_method :method_added do |method| # modifico el metodo de clase "method_added"
      if !overriden_method? method # control para que haya un bucle de definicion y redefinicion infinito
        @overriden_methods.push method # agrego metodo a la lista de metodos sobreescritos
        redefinir_metodo method # redefino el metodo con las caracteristicas pedidas
      end
    end
  end

  def self.ensure_initialized_overriden_methods_befores_and_afters # Me aseguro que esten inicializados como listas  para que no sean nil
    @overriden_methods = [:initialize] # inicializo. Incluyo "initialize": algunas invariantes involucran atributos que se inicializan en este metodo. Evito que initialize se redefina para que no ocurra un error al tratar con atributos dentro de invariantes no inicializados
    @befores ||= [] # inicializo por default como "[]"
    @afters ||= [] # inicializo por default como "[]"
  end

  def self.overriden_method? method
    @overriden_methods.include? method
  end

  def self.redefinir_metodo method
    aux = self.instance_method(method) #unbound. Guardo el metodo original
    befores = @befores
    afters = @afters
    define_method method do |*args| # redefino el metodo original con el mismo nombre y sus argumentos
      chequear_invariantes  # chequeo de invariantes a priori
      befores.reverse_each{|p| instance_eval &p} # evaluo todos los procs "before" dentro del contexto de la instancia correspondiente
      retorno = aux.bind(self).call(*args) # bindeo y ejecuto el metodo original. Guardo el valor de retorno
      afters.each{|p| instance_eval &p} # evaluo todos los procs "after" dentro del contexto de la instancia correspondiente
      chequear_invariantes # chequeo de invariantes a posteriori
      retorno # retorno original
    end
  end
  ############# before_and_after_each_call ############### end
end




# un ejemplo para testear
class Ejemplo
  attr_accessor :atributo

  before_and_after_each_call(proc {puts "Estoy entrando"}, proc {puts "Estoy saliendo"})
  before_and_after_each_call(proc {self.atributo -= 1}, proc {puts "Chau"})

  def initialize
    self.atributo = 3
  end

  invariante { atributo > 0 }

  invariante { atributo < 10 }

  def m
    self.atributo
  end

  def a
    self.atributo += 5
  end
end