class Object
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
            @befores.reverse_each{|p| p.call}
          end
          retorno = aux.call(*args)
          self.class.instance_eval do
            @afters.each{|p| p.call}
          end
          retorno
        end
      end
    end
  end
end


class Ejemplo
  before_and_after_each_call(proc {puts "Estoy entrando"}, proc {puts "Estoy saliendo"})
  before_and_after_each_call(proc {puts "Estoy entrandoaa"}, proc {puts "Estoy saliendoaaa"})
  def saludar
    puts "Entre"
  end
end



