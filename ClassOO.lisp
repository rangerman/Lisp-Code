#|
compared to C++ or Java, the greast innovation for Object Oriented Coding System of Lisp is that the object operation method  is dependent of class definition .
和C++，java最大的不同，lisp将类和相关的操作函数分割开了，两者的独立性的分割，带来更多可变性，自由度的提高
lisp采用了广义函数机制，两者最能体现差距的是多重方法，多重方法无法存在于消息传递信息语言之中是因为它不属于任何一个类，相反，每一个多重方法都定义了广义函数的一个特定实现，但调用匹配该多重方法的所有特化参数时被调用
using GENERIC function to build another framework for method
for specific class example, you can define functions and methods specialized on the class, DEFCLASS is responsible only for defining the class as a data type.
同样和python一样，并非类的所有属性都需要显示在类定义表中显示指出，在函数的定义和使用过程中同样可以引入新的self.attribute,Lisp也有同样的效果，但是目前所知initialize-instance可以等同于python的__init__，可以随时引入新的类属性
|#
(defparameter *account-number*  0
  "unique account serial number")

(defclass bank-account ()
  ((customer-name
    :initarg  :customer-name
    :initform  (error  "Must supply a customer name to initialize class")
    :reader customer-name
    :writer (setf customer-name) )
   (balance
    :initarg  :balance
    :initform  0
    :accessor balance)
   (account-number
    :initform  (incf  *account-number*))
   account-type))

#|
;;;unlike C++ even Python, the make-instance function work is quiet simple, you need to use INITALIZE-INSTANCE method to get complete control. you can treat INITALIZE-INSTANCE as __init__ in python.
;;;
you can define an :AFTER method on INITALIZE-INSTANCE that sets the account-type slot based on the value that has been stored in the balance slot without directly rewriting INITALIZE-INSTANCE method to cover the default one.
;;;
also need to concentrated on &KEY formal para of INITALIZE-INSTANCE. if an INITALIZE-INISTANCE mehtod specialized on a particular class does specify a &KEY para, that para becomes a legal parameter to MAKE-INSTANCE when creating an instance of that Class.
|#
(defmethod initialize-instance  :after  ( (account bank-account)
					  &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf  (slot-value account 'balance)
	   (*  (slot-value  account  'balance)  (/ opening-bonus-percentage 100))))
  (let  ( (bal (slot-value account  'balance)))
    (setf  (slot-value account  'account-type)
	   (cond
	     ((>=  bal 100000)  :gold)
	     ((>=  bal  5000)     :silver)
	     (t  :bronze))))
)

;;;instantiation
(defparameter acct (make-instance
			     'bank-account
			     :customer-name "Sally Sue"
			     :balance 1000
			     :opening-bonus-percentage 5))
#|
对于lisp而言，对类属性的读写操作其实完全可以用setf和slot-value组合实现，CLisp并没有类似C++和Java那种严格的对象访问机制private、public等，但是为了保持类属性不被各种泛滥使用，所以需要代码实现者提供一套读写函数来进行浅度封装，虽然这样并不能限制slot-value对类属性的使用，但是可以通过这种方式说明某些槽不能直接访问，最好的方式就不是不导出这些槽的名称。
1.directly accessing the slots of an object can lead to fragile code.
2.on the other hand, define a read-or-write for a specific slot, you can make beautiful module coding

类似于C++静态语言的继承机制，广义函数等同于元根父类中的虚函数，后续的子类定义实现后，可以针对具体的子类设置相应的广义函数实现，只需要将广义函数特化到具体的类上即可
if you know you're going to define subclasses of bank-account, it might be a good idea to define balance as a generic function. That way, you can provide different methods on balance for those subclasses or extend its definition with auxiliary methods. So you might write this instead:
|#
(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))
#|
同样读写函数也需要设置成广义函数形式，这样方便后续子类的函数功能实现
as with reader functions, you'll probably want your SETF function to be generic, so you'd actually define it like this:
|#
(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))
;;;需要特别指出的，对于这里的setf customer-name调用形式为：
;;;(setf (customer-name acc) "Jack Chen")
;;;also define a reader function for customer-name:
(defgeneric  customer-name (account))

(defmethod  customer-name ((account  bank-account))
  (slot-value  account  'customer-name))

#|
this kind fo accessor functions completment is not LISP way, thus DEFCLASS supports three slot options that allow you to automatically create reader and writer functions for a specific slot.
1:reader option
(balance
 :initarg :balance
 :initform 0
 :reader balance)
2: writer
(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :reader customer-name
 :writer (setf customer-name))
3:accessor
(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :accessor customer-name)
|#

#|
WITH-SLOTS && WITH-ACCESSORS
这是LISP简洁风格的继承，WITH-SLOTS通过直接将相应槽位控制权转移到输入参数上，从而避免了verbose的slot-value过多使用。
(defmethod assess-low-balance-penalty ((account bank-account))
  (when (< (slot-value account 'balance) *minimum-balance*)
    (decf (slot-value account 'balance) (* (slot-value account 'balance) .01))))

WITH-SLOTS provides direct access to the slots, as if by SLOT-VALUE, while WITH-ACCESSORS provides a shorthand for accessor methods.
WITH-SLOTS包装相应槽位的直接控制权
WITH-ACCESSORS则是相应读写函数的简称
(with-slots (slot*) instance-form
  body-form*)
始终（slot*)可以只是一个元素，即槽名
或者是个两元元素列表，第一个是临时参数名，第二个是槽名
If you had defined balance with an :accessor rather than just a :reader, then you could also use WITH-ACCESSORS
如果同时定义了读写，即：accessor则可以利用WITH-ACCESSORS
The form of WITH-ACCESSORS is the same as WITH-SLOTS except each element of the slot list is a two-item list containing a variable name and the name of an accessor function.
是利用WITH-SLOTS还是WITH-ACCESSORS取决于你的目标，如果是想访问那些不被访问函数支持的槽，或者是为了显式避免定义在访问函数上的附加方法带来的负面影响，其余一般是应该优先使用WITH-ACCESSORS，但是我还是觉得WITH-SLOTS好用，只要你知道你在做什么，一切就好。
|#

(defparameter *minimum-balance*  100)

(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots  (balance)  account
    (when  (<  balance  *minimum-balance*)
      (decf  balance (* balance  0.01)))))

(defmethod assess-low-balance-penalty-through-accessors ((account1 bank-account)  (account2  bank-account))
  (with-accessors  ((balance1 balance)) account1
    (with-accessors  ((balance2 balance)) account2
      (incf balance1 balance2)
      (setf balance2 0))))

#|
多重继承问题，用以处理同一级别直接继承父类的主方法调用顺序问题
(defclass  money-market-account  (checking-account  savings-account)  () )
针对这一子类，现有问题：打印这个货币市场账号的所有信息，其中既有其继承支票账号的属性行为部分，也有存储账户的部分行为
;;;1 直接为最新的子类money-market-account设计新的特化的主方法。这种方式可以让设计者更好地控制新行为，但是需要更多的代码，代码复用性效果不好；
;;;2为3个类的主方法中调用CALL-NEXT-METHOD，这样可以充分利用类优先列表的信息，递进先后地调用money-market-account、checking-account和savings-account类的主方法，从而实现信息融合目标;
;;;3将主要的统一的功能直接定义在bank-account类上的主方法中，其余设计到各子类的特殊功能则通过auxiliary methods实现，这样个函数的功能清晰，但是执行顺序的灵活性受损，只能通过更改子类的直接基类列表来实现，故而如果你不关心执行顺序，且希望无论是何种广义函数的执行顺序都是一样的，那这种附加方法的途径是合适的。
|#