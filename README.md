# A Janus interpreter in Standard ML

This archive contains a Janus interpreter.
The system was developed by

   Standard ML of New Jersey v110.71

SML/NJ is available from

   http://smlnj.org

I have developed this interpreter under Mac OS X.
To install a janus interpreter and the other tools, type

    $ aclocal                                #generate aclocal.m4
    $ touch README AUTHORS ChangeLog NEWS
    $ automake --add-missing
    $ autoreconf
    $ ./configure --prefix=/path/to/directory
    $ make all
    $ make install

A typical example prefix is --prefix=$HOME/local/janus
I have succeeded to install this system on

   Mac OS X + SML/NJ v110.78

The name of wrapper file to invoke a Janus interpreter is
``janus''.  The installed tools include:

    janus             a Janus interpreter
    janus-inv         a Janus program inverter
    janus-trans       a Janus translator from a Janus program
                        to the input program for a self-interpreter
    janus-parse       a Janus parser

All the program takes the name of a Janus program file as the argument.
Here is the sample session:

    ~/Janus-A-sml/examples$ janus
    file? fib.janus
    > symbols
    n[0]    0x0     (0)
    x1[0]   0x0     (0)
    x2[0]   0x0     (0)
    > call main_fwd
    > symbols
    n[0]    0x0     (0)
    x1[0]   0x5     (5)
    x2[0]   0x8     (8)
    > quit

You can also use the Janus system without installation.  Type

    ~/Janus-A-sml/src$ sml
    - CM.make "sources.cm";

Then, for example, you can test a Fibonacci forward execution by typing

    - Prompt.start ();
    file? ../examples/fib.janus
    > call main_fwd
    > symbols
    n[0]    0x0     (0)
    x1[0]   0x5     (5)
    x2[0]   0x8     (8)
    > quit

A self-interpreter can be invoked by typing

    ~/Janus-A-sml/examples$ janus
    file? sint.janus
    > add "test_sint_fib_fwd.janus"
    > call sint_init
    > call test_fib_fwd
    > call exec
    > symbols
    
    [ ... snip ... ]
    
    sigma[0]        0x0     (0)
    sigma[1]        0x5     (5)
    sigma[2]        0x8     (8)
    
    [ ... snip ... ]
    
    > quit


The usage of a program inverter is:

    ~/Janus-A-sml/src$ sml
    - CM.make "sources.cm";
    - Invert.invert "../examples/fib.janus";

or

    ~/Janus-A-sml/src$ janus-inv ../examples/fib.janus

It should be noted that the name of procedure does not change.
The usage of a translator from a Janus program to the additional input file to Sint

    ~/Janus-A-sml/src$ sml
    - CM.make "sources.cm";
    - Trans.trans "../examples/fib.janus";
    
    [ ... copy the output to "../examples/test_sint_fib.janus" and
          change the index of tmp_stack[] into 10 ... ]
    
    - Prompt.start' "../examples/sint.janus";
    > add "../examples/test_sint_fib.janus"
    > call sint_init
    > pc=45                                                (* set program counter *)
    > call fib
    > call exec
    > symbols

Please check the shell scripts for testing janus programs in
Janus-A-sml/tests


## An example session

    examples$ janus "../examples/sort.janus";
    > n=5
    > call readlist
    5
    2
    1
    3
    4
    > call makeidperm
    > symbols
    list[0] 0x5     (5)
    list[1] 0x2     (2)
    list[2] 0x1     (1)
    list[3] 0x3     (3)
    list[4] 0x4     (4)
    list[5] 0x0     (0)
    list[6] 0x0     (0)
    list[7] 0x0     (0)
    list[8] 0x0     (0)
    list[9] 0x0     (0)
    list[10]        0x0     (0)
    list[11]        0x0     (0)
    perm[0] 0x0     (0)
    perm[1] 0x1     (1)
    perm[2] 0x2     (2)
    perm[3] 0x3     (3)
    perm[4] 0x4     (4)
    perm[5] 0x0     (0)
    perm[6] 0x0     (0)
    perm[7] 0x0     (0)
    perm[8] 0x0     (0)
    perm[9] 0x0     (0)
    perm[10]        0x0     (0)
    perm[11]        0x0     (0)
    n[0]    0x5     (5)
    i[0]    0x0     (0)
    j[0]    0x0     (0)
    > call sort
    > symbols
    list[0] 0x1     (1)
    list[1] 0x2     (2)
    list[2] 0x3     (3)
    list[3] 0x4     (4)
    list[4] 0x5     (5)
    list[5] 0x0     (0)
    list[6] 0x0     (0)
    list[7] 0x0     (0)
    list[8] 0x0     (0)
    list[9] 0x0     (0)
    list[10]        0x0     (0)
    list[11]        0x0     (0)
    perm[0] 0x2     (2)
    perm[1] 0x1     (1)
    perm[2] 0x3     (3)
    perm[3] 0x4     (4)
    perm[4] 0x0     (0)
    perm[5] 0x0     (0)
    perm[6] 0x0     (0)
    perm[7] 0x0     (0)
    perm[8] 0x0     (0)
    perm[9] 0x0     (0)
    perm[10]        0x0     (0)
    perm[11]        0x0     (0)
    n[0]    0x5     (5)
    i[0]    0x0     (0)
    j[0]    0x0     (0)
    > quit


## Interactive Environment

The Janus interactive environment is provided, in which the users can
interactively change stores and inspect them and execute procedures by
call or uncall.  When Janus is run, it will respond with the query
"file?", asking for the name of the file to be parsed.  After parsing,
Janus will prompt with ">".  The commands accepted by Janus run time
system are:

       index
             Prints out the value of var[index]
       var
             Prints out all elements of array var
       var=n
             Sets var[0] to n.
       var[index]=n
             Sets var[index] to n.
       add "filename"
             Read additional file.
       call name
             Calls procedure "name".
       uncall name
             Uncalls procedure "name".
       symbols
             Types table of all symbols and their attributes.
       trace
             Turns on the trace feature. Lists statements as they are
             executed, along with the values of any variables that are
             modified.
       untrace
             Turns off the trace feature.
       reset
             Resets all variables to zero.
       reset var
             Resets all elements of array var to zero.
       quit
             Quit the interactive system.
       time
             Toggle the timer.
