Notes on Adding Classes to the Interpreter:

April 8th:	
	1. Adding Classes to the Interpreter:
		a. We need to add class and object functions (create and access elements)
		b. Class:
			i. Class variable environment
			ii. Instance variable plus initial value expression environment
			iii. Method environment
			iv. Parent class
		c. Object:
			i. Instance variable values
			ii. True type class
		d. Create a new top level for the interpreter that reads class definitions and stores them in the environment
		e. Change Part 3's top level interpreter to read the class body (variable declarations and function definitions) and return the class, not the environment
			i. If a method is not static, append 'this' to the formal parameters
	2. Change Part 2 of the interpreter (interprets function bodies and expressions)
		a. x = y + 5
		b. So that each interpret function takes:
			i. statement
			ii. environment
			iii. return
			iv. break
			v. continue
			vi. class
			vii. instance 
	3. Change how we lookup a name
		a. Take a name, class, and instance and lookup name in class and instance, then return a value
		b. Take a name, environment, class, and instance, then lookup in the environment first
	4. Handle dot expressions
		a. A.x ==> (dot A x)
		b. Evaluate the left side to get the class and instance
			i. a.x ==> (a's true class, a)
			ii. super.x ==> (current class's parent class, instance)
	5. Update interpret assignment to store value in class variable, instance variables, or current environment
	6. Functions:
		a. Function definitions:
			i. Add a function to the closure that gets the owning class of the method
			ii. Every function needs to know the class that it's in
			iii. Should look up the name of the class being built in the environment
		b. Function calls:
			i. Called with a dot
				1) Get class name from left of dot
				2) Find function in that class name
			ii. Called without a dot
				1) If there is an instance, lookup in the instance's class's method environment
				2) If no instance, you will have a class, so lookup in the class's method environment

April 10th:
	Each interpreter function that interprets code (in a method) we have to pass:
		- Current environment
		- Class (current type)
		- Instance (this: null list if no instance)

	Given a dot expression:
		__ .x	Evaluate left-hand side to get a class and instance, lookup variable name in class
		__.m()	Evaluate left-hand side to get a class and instance, lookup method name in instance
		Exceptions: 
			○ there is no instance
			○ the left hand side is super - lookup in parent class of the current type (class)
		
	Instance:
		- values of instance (non-static) variables
		- true type
		
	Class
		- class variables (static)
		- methods
		- parent class
		- the name of the instance variables + initial value expressions
		- index variables from back to front
		
	To call a method:
		- We retrieved the method closure from the appropriate class
			a. Call the environment function from the closure to get the base layer(s) for the method's environment
			b. Evaluate each argument in the current environment and bind to the formal parameter and place in the top layer of the method environment
			c. Call the method body on the method environment
		- Call method with classes and objects:
			a. Call environment function from the closure to get base layer of method's environment
			b. Evaluate each argument in current environment, class, and instance and bind to the formal parameter in the top layer of the method environment
				i. Exception: for non-static methods, the first formal parameter will be this 
				ii. Bind the left-hand side's instance
			c. Call the method body on the method environment, the method's class (from the class function in the method's closure), and instance from left-hand side (bound to this)
	