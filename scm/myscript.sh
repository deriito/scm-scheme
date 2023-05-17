#! /bin/sh
# "myscript.sh" (unix) script created by SLIB/batch 
# [-p darwin]
# ================ Write file with C defines
rm -f scmflags.h
echo '#define IMPLINIT "Init5f3.scm"'>>scmflags.h
echo '#define COMPILED_INITS init_socket();'>>scmflags.h
echo '#define CAUTIOUS'>>scmflags.h
echo '#define CAREFUL_INTS'>>scmflags.h
echo '#define CAN_DUMP'>>scmflags.h
# ================ Compile C source files
cc -O3 -c -g socket.c continue.c scm.c scmmain.c findexec.c script.c time.c repl.c scl.c eval.c sys.c subr.c debug.c unif.c rope.c definedatatype.c ega.c
# ================ Link C object files
mv -f scm scm~
cc -o scm socket.o continue.o scm.o scmmain.o findexec.o script.o time.o repl.o scl.o eval.o sys.o subr.o debug.o unif.o rope.o definedatatype.o ega.o -g
