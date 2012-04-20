# comments with #, goes whole line

# functions declared with 'f'
ftest\
	# Variable assignment, fucntion calls and array indices are all handled with ".".
	# We don't need no stinking 'obj.func' syntax, that can be handled just fine
	# with plain functions and getters like common lisp's slot-value.
	# Variables denoted with 'v'.
	# They are created if they don't exist already.
	vfoo.5;

	# Both will be set to 6
	vbar.vbaz.6;

	# Strings and floats also supported
	vstr."Hello";
	# Note the potential ambiguity
	vfloat.0.56;
	# Maybe lisp-style ratios a/w?

	# If statements written with 'i'
	ic>.vfoo,vbaz\
		# or "i foo > baz:"
		
		# calls (c) the 'print' function with string argument "9".
		cprint."9";
		
		# identifiers like 'v' still needed outside of declaration
		# \ used to start and end blocks, and can replace ; 
		cprint.vstr\
	# 'e' is else clause.
	# Else clauses can take expressions as well, in which case they're 
	# equivalent to c's "else if"s.
	e\cprint."no"\

	# Functions, loops and control statements return the last value of the
	# constituent operations, thus stuff like this is valid:
	vfoo.(!=.ibar,0)\5\e\6\;
	# It also eliminates the need for a ternary operator.

	# Looping is accomplished with 'w', to the effect of 'while' loops everywhere
	# else.
	vfoo.0;
	w(c!=.wvfoo,5)\vfoo.c+.vfoo,1\

	# w called with function-style parameters will act like a 'for' loop.
	# Here 'itoa' basically its c equivalent.
	w.(vfoo.0),(c!=.vfoo,5),(vfoo.(c+.vfoo,1))\cprint.citoa.vfoo\
	# note that this breaks expectation somewhat as the second and third 
	# 'arguments' will be evaluated five times, not once.

	# Arrays are denoted with [] and assigned to standard variables.
	varr.[5 3 2 1];

	# Get an array index by 'calling' it.
	carr.1 # -> 2
	# Related: there is not separate namespace between variables and functions.
	# Also functions should thus be able to be passed around by referring to them
	# by v.

	# function returns 5.
	5\

# general notes
# Given that block start and block end characters are the same, text editors
# might have trouble indenting statements correctly (if I bother making
# syntax modules for anything, obviously). It'll also make it Fun for the 
# interpreter.

# Mathematical operations are looking pretty special with their infix syntax. I
# wonder if it wouldn't be better to do +.5.3 instead. Or maybe I'm generalizing
# my brain into a pack of grey sludge. Either way.

# Maybe the best way to handle floats would be to use commas instead of dots
# to indicate decimals. Or switch to commas for the "everything operator".

# With the syntax I've defined I wonder if the semicolons are even necessary
# at all. 

# I had originally intended to use : for scoping, but then I had the thought
# that you should be able to write programs without pressing Shift at all.

# Beneath the wierd syntax Terse is a fairly standard imperative programming
# language. It just stays away from most standard naming/syntax.
