;;;这是do-primes的粗暴函数version
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
#|
;;;do-primes宏的操作模式应该尽可能和已有的宏doList\doTimes一样，不引入过多不必要的新颖用法
;;;宏的调用示例
(do-primes (p 0 19)
  (format t "~d " p))
|#

;;;正确的代码展开形式
(do ((p (next-prime 0) (next-prime (1+ p))))
      ((> p 19))
  (format t "~d " p))

#|
宏形参方式(var start end)支持直接在形参初始化阶段形成匹配赋值
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
          ((> ,var ,end))
     ,@body))
;;;macroexpand 接受宏调用lisp表达式作为参数，持续深入地展开代码
;;;macroexpand-1 接受宏调用lisp表达式作为参数，并返回只展开第一层的代码展开式
;;;(macroexpand-1 '(do-primes (p 0 10) (format t "~d " p)))
;;;宏展开式可能阅读形式不好，比如全部在一行，可以在CL-USER输入(setf *print-pretty* t)

#|
堵住漏洞（the law of leaky abstractions)
Spolsky‘s view: 所有的抽象都存在某种意义上的泄漏，不存在完美的解决方案，但是这并不意味着
你可以容忍那些可以轻易堵上的漏洞

示例：(do-primes (p 0 (random 100))
                  (format t "~d " p))
这种调用则会产生多重求值漏洞问题，该宏代码的展示形式为：
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
    ((> P (RANDOM 100))) ;;这里的random操作将进行多次，而非初始进行一次随机赋值操作
  (FORMAT T "~d " P))

所谓最小惊动原则：priciple of least astonishment:
       即可以按照自己的宏形式展开的多种可能性设计尽可能全面的适应性强的代码，但是应尽可能的少的给用户调用者带来诸多限制，或者违反用户正常使用习惯，甚至连这种可能性也应尽可能少，以避免给用户带来不必要的难以发现的错误

封堵方法很简单，对end形参再进行一次求值
(defmacro do-primes ( (var start end)  &body body)
    `(do  ( (ending-value ,end)
	 (,var  (next-prime  ,start)  (next-prime (1+ ,var))))
	( (>  ,var  ending-value))
       ,@body))

但是这种封堵方式可能也会引入新的漏洞，
1.即do循环会先计算end参数值，后计算start,这种情况在start， end是0，19这种数字时没有影响，但是当start，end存在先后调用互相影响的情况时，则上法的调用方式颠倒了do-primes的参数传输顺序，故而不同的求值顺序可能是它们的结果发生影响，从而更合理的实现方式应该严格按照形参顺序进行的代码求值赋值操作：
(defmacro do-primes ( (var start end)  &body body)
    `(do  ((,var  (next-prime  ,start)  (next-prime (1+ ,var)))
	   (ending-value ,end))	 
	( (>  ,var  ending-value))
       ,@body))

2.漏洞则是其引入ending-value,这则可能导致下面这个看似无辜的do-primes调用无法进行
(do-primes (ending-value 0 19)
  (print ending-value))
为了堵上这种因为变量名冲突导致的莫名其妙的问题，很明显，解决的办法只有需要在宏代码展开
代码外永远不会被用到的变量名，这需要用到函数GENSYM
|#

#|
(defmacro  do-primes  ( (var start end)  &body body)
  (let  ( (ending-value-name  (gensym)))
    `(do  ( (,var  (next-prime  ,start)  (next-primes (1+  ,var)))
	 (,ending-value-name  ,end))
	( (>  ,var  ,ending-value-name))
       ,@body)))
|#

;;;lisp的宏主要的用途是用来将常见的句法模式抽象掉，对于反复出现在宏编写中的特定模式也是可
;;;以通过宏来抽象，即所谓的编写宏的宏
;;;考虑到以上宏编写中频繁模式为let( ())的初始赋值操作

(defmacro with-gensyms ( (&rest names)  &body body)
  `(let  ,(loop for n in names collect `( ,n  (gensym) ))
     ,@body))

(defmacro  do-primes  ( (var start end)  &body body)
  (with-gensyms  (ending-value-name)
    `(do  ( (,var  (next-prime  ,start)  (next-primes (1+  ,var)))
	 (,ending-value-name  ,end))
	( (>  ,var  ,ending-value-name))
       ,@body)))
