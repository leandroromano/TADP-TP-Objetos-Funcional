class Object
  def self.invariante(&condicion)
    @invariantes ||= [] # inicializo por default como "[]"
    @invariantes.push(condicion) # agrego proc "condicion"
  end

  def chequear_invariantes
    invariantes = self.class.instance_variable_get :@invariantes # obtengo la lista de invariantes
    if !invariantes.all? {|condicion| instance_eval &condicion} # verifico que se cumplan todas las invariantes
      raise "No se cumplen todas las invariantes!!!" # en caso de que no se cumplan tiro una excepcion
    end
  end

  def self.before_and_after_each_call(before, after)
    @overriden_methods = [:initialize] # inicializo. Incluyo "initialize": algunas invariantes involucran atributos que se inicializan en este metodo. Evito que initialize se redefina para que no ocurra un error al tratar con atributos dentro de invariantes no inicializados.
    @befores ||= [] # inicializo por default como "[]".
    @befores.push(before) # agrego proc "before"
    @afters ||= [] # inicializo por default como "[]"
    @afters.push(after) # agrego proc "after"
    self.define_singleton_method :method_added do |method| # modifico el metodo de clase "method_added"
      if !@overriden_methods.include? method # control para que haya un bucle de definicion y redefinicion infinito
        @overriden_methods.push method # idem ^^^
        aux = self.instance_method(method) #unbound. Guardo el metodo original
        define_method method do |*args| # redefino el metodo original con el mismo nombre y sus argumentos
          instance_eval {self.send(:chequear_invariantes)}  # chequeo de invariantes a priori
          self.class.instance_variable_get(:@befores).reverse_each{|p| instance_eval &p} # evaluo todos los procs "before" dentro del contexto de la instancia correspondiente
          retorno = aux.bind(self).call(*args) # bindeo y ejecuto el metodo original; guardo el valor de retorno
          self.class.instance_variable_get(:@afters).each{|p| instance_eval &p} # evaluo todos los procs "after" dentro del contexto de la instancia correspondiente
          instance_eval {self.send(:chequear_invariantes)} # chequeo de invariantes a posteriori
          retorno # retorno el retorno original
        end
      end
    end
  end
end

# un ejemplo para testear
class Ejemplo
  attr_accessor :atributo

  before_and_after_each_call(proc {puts "Estoy entrando"}, proc {puts "Estoy saliendo"})

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
