;;;����do-primes�Ĵֱ�����version
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
#|
;;;do-primes��Ĳ���ģʽӦ�þ����ܺ����еĺ�doList\doTimesһ������������಻��Ҫ����ӱ�÷�
;;;��ĵ���ʾ��
(do-primes (p 0 19)
  (format t "~d " p))
|#

;;;��ȷ�Ĵ���չ����ʽ
(do ((p (next-prime 0) (next-prime (1+ p))))
      ((> p 19))
  (format t "~d " p))

#|
���βη�ʽ(var start end)֧��ֱ�����βγ�ʼ���׶��γ�ƥ�丳ֵ
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
          ((> ,var ,end))
     ,@body))
;;;macroexpand ���ܺ����lisp���ʽ��Ϊ���������������չ������
;;;macroexpand-1 ���ܺ����lisp���ʽ��Ϊ������������ֻչ����һ��Ĵ���չ��ʽ
;;;(macroexpand-1 '(do-primes (p 0 10) (format t "~d " p)))
;;;��չ��ʽ�����Ķ���ʽ���ã�����ȫ����һ�У�������CL-USER����(setf *print-pretty* t)

#|
��ס©����the law of leaky abstractions)
Spolsky��s view: ���еĳ��󶼴���ĳ�������ϵ�й©�������������Ľ�������������Ⲣ����ζ��
�����������Щ�������׶��ϵ�©��

ʾ����(do-primes (p 0 (random 100))
                  (format t "~d " p))
���ֵ���������������ֵ©�����⣬�ú�����չʾ��ʽΪ��
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
    ((> P (RANDOM 100))) ;;�����random���������ж�Σ����ǳ�ʼ����һ�������ֵ����
  (FORMAT T "~d " P))

��ν��С����ԭ��priciple of least astonishment:
       �����԰����Լ��ĺ���ʽչ���Ķ��ֿ�������ƾ�����ȫ�����Ӧ��ǿ�Ĵ��룬����Ӧ�����ܵ��ٵĸ��û������ߴ���������ƣ�����Υ���û�����ʹ��ϰ�ߣ����������ֿ�����ҲӦ�������٣��Ա�����û���������Ҫ�����Է��ֵĴ���

��·����ܼ򵥣���end�β��ٽ���һ����ֵ
(defmacro do-primes ( (var start end)  &body body)
    `(do  ( (ending-value ,end)
	 (,var  (next-prime  ,start)  (next-prime (1+ ,var))))
	( (>  ,var  ending-value))
       ,@body))

�������ַ�·�ʽ����Ҳ�������µ�©����
1.��doѭ�����ȼ���end����ֵ�������start,���������start�� end��0��19��������ʱû��Ӱ�죬���ǵ�start��end�����Ⱥ���û���Ӱ������ʱ�����Ϸ��ĵ��÷�ʽ�ߵ���do-primes�Ĳ�������˳�򣬹ʶ���ͬ����ֵ˳����������ǵĽ������Ӱ�죬�Ӷ��������ʵ�ַ�ʽӦ���ϸ����β�˳����еĴ�����ֵ��ֵ������
(defmacro do-primes ( (var start end)  &body body)
    `(do  ((,var  (next-prime  ,start)  (next-prime (1+ ,var)))
	   (ending-value ,end))	 
	( (>  ,var  ending-value))
       ,@body))

2.©������������ending-value,������ܵ���������������޹���do-primes�����޷�����
(do-primes (ending-value 0 19)
  (print ending-value))
Ϊ�˶���������Ϊ��������ͻ���µ�Ī����������⣬�����ԣ�����İ취ֻ����Ҫ�ں����չ��
��������Զ���ᱻ�õ��ı�����������Ҫ�õ�����GENSYM
|#

#|
(defmacro  do-primes  ( (var start end)  &body body)
  (let  ( (ending-value-name  (gensym)))
    `(do  ( (,var  (next-prime  ,start)  (next-primes (1+  ,var)))
	 (,ending-value-name  ,end))
	( (>  ,var  ,ending-value-name))
       ,@body)))
|#

;;;lisp�ĺ���Ҫ����;�������������ľ䷨ģʽ����������ڷ��������ں��д�е��ض�ģʽҲ�ǿ�
;;;��ͨ���������󣬼���ν�ı�д��ĺ�
;;;���ǵ����Ϻ��д��Ƶ��ģʽΪlet( ())�ĳ�ʼ��ֵ����

(defmacro with-gensyms ( (&rest names)  &body body)
  `(let  ,(loop for n in names collect `( ,n  (gensym) ))
     ,@body))

(defmacro  do-primes  ( (var start end)  &body body)
  (with-gensyms  (ending-value-name)
    `(do  ( (,var  (next-prime  ,start)  (next-primes (1+  ,var)))
	 (,ending-value-name  ,end))
	( (>  ,var  ,ending-value-name))
       ,@body)))
