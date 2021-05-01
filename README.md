# ceder - a code structure traversal 

ceder is currentliy an, experimental, code traverser.

What you can do is to load erlang code and use
tty module to walk around in the code, and print/line nodes.

start with 

    erl -noinput -s ceder start <file.erl>
	
You need to clone and bulild

	https://github.com/tonyrog/tty
	
to use ceder interactivly.

For example you can try ceder on ceder.erl it self (in src directory)
There is function last in ceder.erl that we can peek and try to lint.

    # erl -noinput -s ceder start ceder.erl
	1> d
	2> l
	3854> P
	form: |test_fun(X) ->
    B = 1,
    case X of
        2 + 1 ->
            foo;
        Z ->
            X = foo + 1,
            N = now(),
            N + X + Z
    end.|
	3854> L
	nofile:3858: warn, variable 'B' is unused
	|B|
	nofile:3876: warn, erlang:now/0 is deprecated; see the "Time and Time Correction in Erlang" chapter of the ERTS User's Guide for more information
	|now()|
		nofile:3886: warn, export_all flag enabled - all functions will be exported
	|-compile(export_all).	
	3854> q
	
	
current commands are:

		u | up    : up
		d | down  : down
		p | left  : left
		n | right : right
		t | home  : top
		f |       : first
		l | end   : last
		P |       : print current node
		L |       : lint node (function/program)
		q         : quit
		
That's all for now
