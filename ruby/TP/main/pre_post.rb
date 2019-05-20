class Module
  def pre(&precondicion)
    @pre = precondicion
    llamar_before_and_after
  end

  def post(&postcondicion)
    @post = postcondicion
    llamar_before_and_after
  end

  def llamar_before_and_after
    unless @pres_and_posts_chequeando
    before_and_after_each_call(proc{|metodo| self.check_pre metodo}, proc {|metodo, retorno| self.check_post metodo, retorno})
    @pres_and_posts_chequeando = true
    end
  end

  def set_pres_and_posts method
    @pres_and_posts ||= Hash.new
    @pres_and_posts[method] = [@pre, @post]
    @pre = nil
    @post = nil
  end
end

class Object
  def check_pre method
    pre = self.instance.class.instance_variable_get(:@pres_and_posts)[method][0]
    unless pre.nil?
      raise NoSeCumplePrecondicion unless instance_eval &pre
    end
  end

  def check_post method, retorno
    post = self.instance.class.instance_variable_get(:@pres_and_posts)[method][1]
    unless post.nil?
      raise NoSeCumplePostcondicion unless instance_exec retorno, &post
    end
  end
end

class Context
  attr_accessor :instance

  def method_missing (method, *args)
    self.instance.send(method, *args)
  end

  def initialize (method_params, an_instance)
    method_params.keys.each{|key| self.class.attr_accessor key}
    method_params.each{|key, value| self.send((key.to_s + "=").to_sym, value)}
    self.instance = an_instance
  end
end

class NoSeCumplePrecondicion < RuntimeError; end
class NoSeCumplePostcondicion < RuntimeError; end