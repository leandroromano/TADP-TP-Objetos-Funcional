class Provider
  @@before_methods = []
  @@after_methods = []
  @adding = false

  
  def self.before_and_after_each_call(before, after)
    @@before_methods.push(before)
    @@after_methods.push(after) 
  end

  def self.method_added(method_name)
    if (!@adding)
      @adding  = true
      index = -1
      @@before_methods.each do
        pp index = index + 1
        aux = self.new.method(method_name)
        self.define_method (method_name.to_s) do
          @@before_methods[index].call
          aux.call
          @@after_methods[index].call  
        end
        @adding = false
      end 
    end
  end  

end

class MyClass < Provider
  before_and_after_each_call(proc { pp "Entrando"}, proc {pp "Saliendo"})
  before_and_after_each_call(proc { pp "RecontraEntrando"}, proc {pp "RecontraSaliendo"})
 
  def foo
    pp "bar"
  end

  def zoo 
    pp "rab"
  end

end

MyClass.new.foo
