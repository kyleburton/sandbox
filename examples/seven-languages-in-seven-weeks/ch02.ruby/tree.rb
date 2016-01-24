class Tree1
  attr_accessor :children, :node_name

  def initialize(name, children=[]) 
    @children = children
    @node_name = name
  end

  def visit_all(&block)
    visit &block
    children.each {|c| 
      c.visit_all &block
    }
  end

  def visit(&block) 
    block.call self
  end
end

def tree_example
  ruby_tree = Tree1.new( 
                        "Ruby", 
                        [Tree1.new("Reia"),
                          Tree1.new("MacRuby")]
                       )
  puts "Visiting a node"
  ruby_tree.visit {|node| 
    puts node.node_name
  } 
  puts
  puts "visiting entire tree"
  ruby_tree.visit_all {|node| 
    puts node.node_name
  }
end

class Tree2
  attr_accessor :children, :node_name

  # • The Tree class was interesting, but it did not allow you to specify a new
  #   tree with a clean user interface. Let the initializer accept a nested
  #   structure of hashes. You should be able to specify a tree like this:
  #   {'grandpa' => { 'dad' => {'child 1' => {}, 'child 2' => {} }, 'uncle' =>
  #   {'child 3' => {}, 'child 4' => {} } } }.
  # NB: !!! name must be present in raw_tree !!!  it's the ancestor that the tree will be built from!
  def initialize(name, raw_tree={}) 
    @node_name = name
    @children = []
    # require 'debugger'; debugger; 1;
    #puts "name=#{name}"
    #puts "raw_tree=#{raw_tree}"
    raw_tree[name].each do |whom,begat|
      #puts "whom=#{whom}; begat=#{begat}"
      @children << Tree2.new(whom, {whom => begat})
    end
  end

  def visit_all(path=[self], depth=0, &block)
    visit path, depth, &block
    children.each {|c| 
      np = path + [c]
      c.visit_all np, depth+1, &block
    }
  end

  def visit(path=[self], depth=0, &block) 
    block.call self, path, depth
  end
end


def tree_example2
  some_tree = Tree2.new("grandpa", {
    'grandpa' => {
      'dad' => {
        'd.thing1' => {},
        'd.thing2' => {}
      },
      'uncle' => {
        'u.thing1' => {},
        'u.thing2' => {},
        'u.thing3' => {},
        'u.thing4' => {}
      }
    }
  })
  puts "Visiting a node"
  some_tree.visit {|node, path, depth| 
    ps = path.inject([]) do |acc,node|
      acc << node.node_name
    end
    puts "[#{depth}/#{ps}] #{node.node_name}"
  } 
  puts
  puts "visiting entire tree"
  some_tree.visit_all {|node, path, depth| 
    ps = path.inject([]) do |acc,node|
      acc << node.node_name
    end
    puts "[#{depth}/#{ps}] #{node.node_name}"
  }
end
