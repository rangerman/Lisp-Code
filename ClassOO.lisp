#|
compared to C++ or Java, the greast innovation for Object Oriented Coding System of Lisp is that the object operation method  is dependent of class definition .
��C++��java���Ĳ�ͬ��lisp�������صĲ��������ָ�ˣ����ߵĶ����Եķָ��������ɱ��ԣ����ɶȵ����
lisp�����˹��庯�����ƣ������������ֲ����Ƕ��ط��������ط����޷���������Ϣ������Ϣ����֮������Ϊ���������κ�һ���࣬�෴��ÿһ�����ط����������˹��庯����һ���ض�ʵ�֣�������ƥ��ö��ط����������ػ�����ʱ������
using GENERIC function to build another framework for method
for specific class example, you can define functions and methods specialized on the class, DEFCLASS is responsible only for defining the class as a data type.
ͬ����pythonһ������������������Զ���Ҫ��ʾ���ඨ�������ʾָ�����ں����Ķ����ʹ�ù�����ͬ�����������µ�self.attribute,LispҲ��ͬ����Ч��������Ŀǰ��֪initialize-instance���Ե�ͬ��python��__init__��������ʱ�����µ�������
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
����lisp���ԣ��������ԵĶ�д������ʵ��ȫ������setf��slot-value���ʵ�֣�CLisp��û������C++��Java�����ϸ�Ķ�����ʻ���private��public�ȣ�����Ϊ�˱��������Բ������ַ���ʹ�ã�������Ҫ����ʵ�����ṩһ�׶�д����������ǳ�ȷ�װ����Ȼ��������������slot-value�������Ե�ʹ�ã����ǿ���ͨ�����ַ�ʽ˵��ĳЩ�۲���ֱ�ӷ��ʣ���õķ�ʽ�Ͳ��ǲ�������Щ�۵����ơ�
1.directly accessing the slots of an object can lead to fragile code.
2.on the other hand, define a read-or-write for a specific slot, you can make beautiful module coding

������C++��̬���Եļ̳л��ƣ����庯����ͬ��Ԫ�������е��麯�������������ඨ��ʵ�ֺ󣬿�����Ծ��������������Ӧ�Ĺ��庯��ʵ�֣�ֻ��Ҫ�����庯���ػ�����������ϼ���
if you know you're going to define subclasses of bank-account, it might be a good idea to define balance as a generic function. That way, you can provide different methods on balance for those subclasses or extend its definition with auxiliary methods. So you might write this instead:
|#
(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))
#|
ͬ����д����Ҳ��Ҫ���óɹ��庯����ʽ�����������������ĺ�������ʵ��
as with reader functions, you'll probably want your SETF function to be generic, so you'd actually define it like this:
|#
(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))
;;;��Ҫ�ر�ָ���ģ����������setf customer-name������ʽΪ��
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
����LISP�����ļ̳У�WITH-SLOTSͨ��ֱ�ӽ���Ӧ��λ����Ȩת�Ƶ���������ϣ��Ӷ�������verbose��slot-value����ʹ�á�
(defmethod assess-low-balance-penalty ((account bank-account))
  (when (< (slot-value account 'balance) *minimum-balance*)
    (decf (slot-value account 'balance) (* (slot-value account 'balance) .01))))

WITH-SLOTS provides direct access to the slots, as if by SLOT-VALUE, while WITH-ACCESSORS provides a shorthand for accessor methods.
WITH-SLOTS��װ��Ӧ��λ��ֱ�ӿ���Ȩ
WITH-ACCESSORS������Ӧ��д�����ļ��
(with-slots (slot*) instance-form
  body-form*)
ʼ�գ�slot*)����ֻ��һ��Ԫ�أ�������
�����Ǹ���ԪԪ���б���һ������ʱ���������ڶ����ǲ���
If you had defined balance with an :accessor rather than just a :reader, then you could also use WITH-ACCESSORS
���ͬʱ�����˶�д������accessor���������WITH-ACCESSORS
The form of WITH-ACCESSORS is the same as WITH-SLOTS except each element of the slot list is a two-item list containing a variable name and the name of an accessor function.
������WITH-SLOTS����WITH-ACCESSORSȡ�������Ŀ�꣬������������Щ�������ʺ���֧�ֵĲۣ�������Ϊ����ʽ���ⶨ���ڷ��ʺ����ϵĸ��ӷ��������ĸ���Ӱ�죬����һ����Ӧ������ʹ��WITH-ACCESSORS�������һ��Ǿ���WITH-SLOTS���ã�ֻҪ��֪��������ʲô��һ�оͺá�
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
���ؼ̳����⣬���Դ���ͬһ����ֱ�Ӽ̳и��������������˳������
(defclass  money-market-account  (checking-account  savings-account)  () )
�����һ���࣬�������⣺��ӡ��������г��˺ŵ�������Ϣ�����м�����̳�֧Ʊ�˺ŵ�������Ϊ���֣�Ҳ�д洢�˻��Ĳ�����Ϊ
;;;1 ֱ��Ϊ���µ�����money-market-account����µ��ػ��������������ַ�ʽ����������߸��õؿ�������Ϊ��������Ҫ����Ĵ��룬���븴����Ч�����ã�
;;;2Ϊ3������������е���CALL-NEXT-METHOD���������Գ�������������б����Ϣ���ݽ��Ⱥ�ص���money-market-account��checking-account��savings-account������������Ӷ�ʵ����Ϣ�ں�Ŀ��;
;;;3����Ҫ��ͳһ�Ĺ���ֱ�Ӷ�����bank-account���ϵ��������У�������Ƶ�����������⹦����ͨ��auxiliary methodsʵ�֣������������Ĺ�������������ִ��˳������������ֻ��ͨ�����������ֱ�ӻ����б���ʵ�֣��ʶ�����㲻����ִ��˳����ϣ�������Ǻ��ֹ��庯����ִ��˳����һ���ģ������ָ��ӷ�����;���Ǻ��ʵġ�
|#