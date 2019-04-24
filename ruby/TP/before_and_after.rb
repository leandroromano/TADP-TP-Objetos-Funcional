class Object
  def self.before_and_after_each_call(before, after)
    @overriden_methods = []
    self.define_singleton_method :method_added do |method|
      if !@overriden_methods.include? method
        @overriden_methods.push method
        aux = self.new.method(method) #unbound
        define_method method do |*args|
          before.call
          aux.call(*args)
          after.call
        end
      end
    end
  end
end


