require_relative "../TP/before_and_after+invariantes"

describe("en clase") do
  it "deberia mantener consistencia entre varias llamadas de before and after" do
    module B
      class << self
        attr_accessor :i, :j
      end
    end

    B.i = 0
    B.j = 0

    class A
      before_and_after_each_call(proc {B.i = B.i + 1},proc {})
      before_and_after_each_call(proc {B.j = B.j + 1},proc {})

      def a
        "a"
      end
    end

    obj = A.new
    obj.a

    expect(B.i).to eq(1)
    expect(B.j).to eq(1)
  end
end