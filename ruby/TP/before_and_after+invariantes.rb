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
  def self.before_and_after_each_call(before, after)
    @overriden_methods = [:initialize]
    @befores ||= []
    @befores.push(before)
    @afters ||= []
    @afters.push(after)
    self.define_singleton_method :method_added do |method|
      if !@overriden_methods.include? method
        @overriden_methods.push method
        aux = self.new.method(method) #unbound
        define_method method do |*args|
          self.class.instance_eval do
            @befores.each{|p| p.call}
          end
          aux.call(*args)
          self.class.instance_eval do
            @afters.each{|p| p.call}
          end
        end
      end
    end
  end
end

class Ejemplo
  before_and_after_each_call(proc {puts "Estoy entrando"}, proc {puts "Estoy saliendo"})
  #before_and_after_each_call(proc{chequear_invariantes}, proc{chequear_invariantes})
  attr_accessor :atributo

  def initialize
    self.atributo = 3
  end

  invariante { atributo > 0 }

  invariante { atributo < 10 }

  def m
    self.atributo -= 1
  end

  def a
    self.atributo += 5
  end
end
