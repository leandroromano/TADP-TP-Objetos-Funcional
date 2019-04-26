class Object
  def self.invariante(&condicion)
    @invariantes ||= []
    @invariantes.push(condicion)
  end

  def chequear_invariantes
    invariantes = self.class.instance_variable_get :@invariantes
    invariantes ||= []
    if !invariantes.all? {|condicion| instance_eval &condicion}
      raise "No se cumplen todas las invariantes!!!"
    else
      true
    end
  end

  def self.before_and_after_each_call(before, after)
    @overriden_methods = [:initialize]
    @befores ||= []
    @befores.push(before)
    @afters ||= []
    @afters.push(after)
    self.define_singleton_method :method_added do |method|
      if !@overriden_methods.include? method
        @overriden_methods.push method
        aux = self.instance_method(method) #unbound
        define_method method do |*args|
          self.class.instance_variable_get(:@befores).reverse_each{|p| instance_eval &p}
          retorno = aux.bind(self).call(*args)
          self.class.instance_variable_get(:@afters).each{|p| instance_eval &p}
          retorno
        end
      end
    end
  end
end

class Ejemplo
  attr_accessor :atributo

  before_and_after_each_call(proc {puts "Estoy entrando"}, proc {puts "Estoy saliendo"})
  before_and_after_each_call(proc{send :chequear_invariantes}, proc{send :chequear_invariantes})

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
