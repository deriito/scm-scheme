#! /bin/sh
# "myscript1.sh" (unix) script created by SLIB/batch 
# [-p darwin]
# ================ Write file with C defines
rm -f scmflags.h
echo '#define IMPLINIT "Init5f3.scm"'>>scmflags.h
echo '#define BIGNUMS'>>scmflags.h
echo '#define FLOATS'>>scmflags.h
echo '#define ARRAYS'>>scmflags.h
# ================ Compile C source files
cc -O3 -c continue.c scm.c scmmain.c findexec.c script.c time.c repl.c scl.c eval.c sys.c subr.c debug.c unif.c rope.c definedatatype.c ega.c disksave.c mymalloc.c
# ================ Link C object files
mv -f scm scm~
cc -o scm continue.o scm.o scmmain.o findexec.o script.o time.o repl.o scl.o eval.o sys.o subr.o debug.o unif.o rope.o definedatatype.o ega.o disksave.o mymalloc.o
