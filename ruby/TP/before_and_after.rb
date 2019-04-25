class Provider

  def self.before_and_after_each_call(before_proc, after_proc) 
    self.instance_methods(false).each do |method|
      aux = self.new.method(method.to_s)
      self.define_method(method.to_s) do
        before_proc.call 
        aux.call
        after_proc.call
      end
    end
  end

  
end

class MyClass < Provider
  def foo
    pp "bar"
  end

  before_and_after_each_call(proc { pp "Entrando"}, proc {pp "Saliendo"})

end


MyClass.new.foo