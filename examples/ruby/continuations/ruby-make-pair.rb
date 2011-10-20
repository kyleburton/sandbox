def make_pair a, b
  lambda do |v|
    if v
      a
    else
      b
    end
  end 
end


def left p
  p.call(true)
end

def right p
  p.call(false)
end

x = make_pair 1, 2
puts left(x)  # 1
puts right(x) # 2

__END__


































































































(define (make-pair a b)
 (lambda (v)
  (if v
   a
   b)))

   (define (left p)
    (p t))

    (define (right p)
     (p nil))


     (define x (make-pair 1 2))


     (left  x) => 1
     (right x) => 2

     ;; make-pair <=> cons
     ;; left      <=> car
     ;; right     <=> cdr

